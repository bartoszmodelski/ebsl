open EffectHandlers
open EffectHandlers.Deep 

let _ = Printexc.record_backtrace true

let with_mutex mtx f =
  Mutex.lock mtx; 
  let v = f () in
  Mutex.unlock mtx; 
  v;; 

type _ eff += Yield : unit eff
let yield () = perform Yield

module Scheduled = struct 
  type sched = 
    | Task of (unit -> unit)
    | Preempted_task of (unit, unit) continuation

  type t = 
    {s : sched; 
    execd : int Atomic.t}

  let task c = 
    {s = Task c; execd = Atomic.make 0}
  
  let preem c = 
    {s = Preempted_task c; execd = Atomic.make 0}

  let _ = preem

  let get {s; execd} = 
    Atomic.incr execd; 
    s;;

end

module Promise = struct 
  (* this should be a variant! *)
  type 'a t = {
    returned : 'a option ref;
    awaiting : ('a -> unit) List.t option ref;
    mutex : Mutex.t
  }

  let empty () = 
    ({ returned = ref None; 
    awaiting = ref (Some []);
    mutex = Mutex.create () } : 'a t)

  let await promise f = 
    let ({awaiting; mutex; returned} : 'a t) = promise in  
    with_mutex mutex (fun () ->  
      match !awaiting with 
      | Some awaiting_val -> 
        awaiting := Some (f :: awaiting_val); 
        `Scheduled
      | None -> 
        match !returned with 
        | None -> assert false 
        | Some v -> `Already_done v)

  let fill {returned; awaiting; mutex} value =
    with_mutex mutex (fun () -> 
      assert (Option.is_none !returned);
      returned := Some value;
      let maybe_awaiting_val = !awaiting in 
      awaiting := None;
      match maybe_awaiting_val with 
      | None -> assert false 
      | Some awaiting_val -> awaiting_val);;
end 

module type DataStructure = sig 
  type 'a t 

  val init : ?size_exponent:int -> unit -> 'a t
  val local_insert : 'a t -> 'a -> bool
  val local_insert_after_preemption : 'a t -> 'a -> bool 
  val local_remove : 'a t -> 'a option

  (** Scheduler calls [local_is_empty] before attempting stealing to ensure 
      there's room for new elements in the target queue. 
  *)
  val local_is_empty : 'a t -> bool

  val steal : from:'a t -> to_local:'a t -> int

  (** Debugging misuse of local_ methods is tricky. Scheduler calls 
      [register_domain_id] to register the domain considered local. 
      Domain id is then rechecked in calls to local_* functions. 
  *)
  val register_domain_id : 'a t -> unit

  val indicative_size : 'a t -> int

  val name : String.t
end;;

type _ eff += Await : 'a Promise.t -> 'a eff
let await promise = perform (Await promise)

type _ eff += Schedule : (unit -> 'a) -> 'a Promise.t eff
let schedule f = perform (Schedule f)

module Scheduler (DS : DataStructure) = struct 
  
  let scheduler_footprint = DS.name;;

  module Processor = struct 
    type t = {
      ds : Scheduled.t DS.t;
      latency_histogram : Histogram.t;
      executed_tasks : int ref 
    }

    let init ?size_exponent () = {
        ds = DS.init ?size_exponent (); 
        latency_histogram = Histogram.init ();
        executed_tasks = ref 0
      }

    let ds {ds; _} = ds
    let incr_tasks {executed_tasks; _} = 
      executed_tasks := !executed_tasks + 1
    let executed_tasks {executed_tasks; _} =
      !executed_tasks

    let zero_executed_tasks {executed_tasks; _} =
      executed_tasks := 0

    let latency_histogram {latency_histogram;_}= latency_histogram 
      
    let log_time {latency_histogram; _} span = 
      Histogram.log_val latency_histogram (Core.Int63.to_int_exn span) 
    
    let _ = log_time ;;
  end 
  
  let domain_id_key = Domain.DLS.new_key 
    (fun () -> -1);;

  let processors = ref (Array.make 0 (Processor.init () : Processor.t));;

  let with_processor f =
    let id = Domain.DLS.get domain_id_key in 
    let processor = Array.get !processors id in
    f processor;;

  let schedule_internal ~has_yielded task = 
    with_processor (fun processor ->
      let ds = Processor.ds processor in 
      let insert_f = 
        if has_yielded 
        then DS.local_insert_after_preemption 
        else DS.local_insert
      in
      while not (insert_f ds task) do () done);;

  let schedule_awaiting to_run result = 
    match to_run with 
    | [] -> () 
    | _ ->
      List.iter (fun awaiting -> 
        schedule_internal 
          ~has_yielded:false 
          (Scheduled.task (fun () -> awaiting result)))
          to_run;;

  let realtime_clock = 
    match Core.Unix.Clock.gettime with 
    | Error _ -> assert false 
    | Ok v -> v

  let clock () = realtime_clock Core.Unix.Clock.Monotonic

  let with_effects_handler f =
    try_with f () 
    { effc = fun (type a) (e : a eff) ->
      match e with
      | Schedule new_f ->  
        let time_start = clock () in
        Some (fun (k : (a, unit) continuation) -> 
          let promise = Promise.empty () in     
          schedule_internal ~has_yielded:false (Scheduled.task (fun () -> 
            let time_end = clock () in
            with_processor (fun processor -> 
              Processor.log_time processor (Core.Int63.(time_end - time_start));
              Processor.incr_tasks processor); 
            let result = new_f () in 
            let to_run = Promise.fill promise result in 
            (* it's tempting to re-use looked up queue here but this may 
            run on a different processor and violate local_  *)
            schedule_awaiting to_run result));
          continue k promise)
      | Yield -> 
        Some (fun k -> 
          schedule_internal 
            ~has_yielded:true 
            (Scheduled.task (fun () -> 
              continue k ())))
      | Await promise ->
        Some (fun k -> 
          match (Promise.await promise (continue k)) with 
          | `Scheduled -> () 
          | `Already_done returned -> continue k returned)
      | _ -> None}

  let steal ~my_ds = 
    let my_id = Domain.DLS.get domain_id_key in
    assert (my_id != -1);
    let other_queue_id = Random.int (Array.length !processors) in 
    if other_queue_id = my_id then
      ()
    else 
      (let other_processor = Array.get !processors other_queue_id in
      let other_ds = Processor.ds other_processor in 
      DS.steal ~from:other_ds ~to_local:my_ds 
      |> ignore)
    
  let _ = DS.local_is_empty
  let rec run_domain () = 
    let scheduled = 
      with_processor (fun processor ->
        let task_ds = Processor.ds processor in
        match DS.local_remove task_ds with 
        | Some task -> 
          task 
        | None -> 
          let task = ref None in 
          while Option.is_none !task do 
            steal ~my_ds:task_ds; 
            task := DS.local_remove task_ds
          done;
          match !task with 
          | None -> assert false 
          | Some task -> task)
    in
    (match Scheduled.get scheduled with
    | Task task -> 
      (with_effects_handler task)
    | Preempted_task task -> 
      continue task ());
    run_domain ();;

  let setup_domain ~id () = 
    Domain.at_exit (fun () -> assert false);
    Domain.DLS.set domain_id_key id;
    let processor = Array.get !processors id in 
    let ds = Processor.ds processor in 
    DS.register_domain_id ds;
    run_domain ();;
    
  let notify_user f () =
    try f () with e ->
      let msg = Printexc.to_string e
      and stack = Printexc.get_backtrace () in
        Printf.eprintf "There was an error: %s%s\n" msg stack;
        Stdlib.flush_all ();
        Stdlib.exit 0;;
  let domains = (ref [] : unit Domain.t list ref)

  let init ?size_exponent ~(f : unit -> unit) n =
    processors := List.init (n+1) 
        (fun _ -> Processor.init ?size_exponent ()) 
      |> Array.of_list;
    (* since this thread can schedule as well *)
    domains := List.init n (fun id ->
      Domain.spawn (notify_user (setup_domain ~id)));
    (* run f from within the pool *)
    Domain.DLS.set domain_id_key n;
    let processor = Array.get !processors n in 
    let ds = Processor.ds processor in 
    DS.register_domain_id ds;
    assert (DS.local_insert ds (Scheduled.task f));
    run_domain ();;

  let pending_tasks () = 
    Array.fold_right 
      (fun processor curr -> 
        (DS.indicative_size (Processor.ds processor)) + curr) 
    !processors 0

  module Stats = struct 
    let unsafe_print_latency_histogram () = 
      let histograms = Array.map (fun processor -> 
        Processor.latency_histogram processor) 
        !processors 
      in 
      let merged = 
        Array.to_list histograms 
        |> Histogram.merge
      in
      Array.iter Histogram.zero_out histograms;
      Histogram.dump merged;;

    let unsafe_print_executed_tasks () = 
      let counters = Array.map (fun processor -> 
        Processor.executed_tasks processor) 
        !processors in 
      Array.iter Processor.zero_executed_tasks !processors;
      counters 
      |> Array.to_list
      |> List.map Int.to_string
      |> String.concat ","
      |> Printf.printf "executed-tasks:[%s]\n"
  end

end

module FIFO = Scheduler(struct 
  include Spmc_queue
  
  let local_insert = local_enqueue
  let local_remove = local_dequeue
  let local_insert_after_preemption = local_enqueue  

  let name = "FIFO"
end)

module LIFO = Scheduler(struct 
  include Stack 
  let local_insert = local_push 
  let local_remove = local_pop

  let local_insert_after_preemption stack item = 
    (* actually, put the item in a random place in the stack 
    and push the swapped element on top (to be run immediately) *)
    let f =  
      (match local_replace_with_a_random_item stack item with 
      | None -> fun () -> local_push stack item
      | Some new_item -> fun () -> local_push stack new_item) 
    in
    while not (f ()) do () done;
    true
  ;;

  let name = "LIFO"
end) 


module type S = sig
  val init : ?size_exponent:int -> f:(unit -> unit) -> int -> unit
  val pending_tasks : unit -> int
  val scheduler_footprint : String.t
  module Stats : sig 
    val unsafe_print_latency_histogram : unit -> unit 
    val unsafe_print_executed_tasks : unit -> unit
  end 
end 