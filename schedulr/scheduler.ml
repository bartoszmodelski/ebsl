open EffectHandlers
open EffectHandlers.Deep 

module Custom_queue = Datastructures.Multi_mpmc_queue.Make(struct let num_of_queues = 2 end)

let _ = Printexc.record_backtrace true

type _ eff += Yield : unit eff
type _ eff += Await : 'a Promise.t -> 'a eff
type _ eff += Schedule : (unit -> 'a) -> 'a Promise.t eff
let yield () = perform Yield
let await promise = perform (Await promise)
let schedule f = perform (Schedule f)


module type DataStructure = sig 
  (* TODO: make global/local submodules *)
  type 'a t 

  val init : ?size_exponent:int -> unit -> 'a t
  val local_insert : 'a t -> 'a -> bool
  val local_insert_after_preemption : 'a t -> 'a -> bool 
  val local_remove : 'a t -> 'a option
  val global_steal : from:'a t -> to_local:'a t -> int

  val register_domain_id : 'a t -> unit
  val indicative_size : 'a t -> int

  val name : String.t
end;;

module Make (DS : DataStructure) = struct   
  let scheduler_name = DS.name;;

  module Processor = struct 
    type t = {
      ds : Task.t DS.t;
      id : int; 
      latency_histogram : Histogram.t;
      executed_tasks : int ref;
      steal_attempts : int ref; 
    }

    let init ?size_exponent id = {
        ds = DS.init ?size_exponent ();
        id; 
        latency_histogram = Histogram.init ();
        executed_tasks = ref 0;
        steal_attempts = ref 0;
      }
    
    let id {id; _} = id
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

    let take_from {steal_attempts; _} = 
      steal_attempts := !steal_attempts + 1;
      if !steal_attempts mod 4 = 0 then 
        `Global_queue 
      else 
        `Steal;;

  end 

  module Context = struct 
    type t = {
      processor : Processor.t;
      all_processors : Processor.t Array.t;
      global_queue : Task.t Custom_queue.t;
    }
  end

  type t = {
    global_queue : Task.t Custom_queue.t;
  }

  let inject_task {global_queue} f = 
    Custom_queue.enqueue global_queue (Task.new_task f)

  let domain_key = Domain.DLS.new_key 
    (fun () -> None);;

  let with_context f =
    let processor = 
      match Domain.DLS.get domain_key with 
      | None -> assert false 
      | Some p -> p 
    in
    f processor;;

  let with_processor f = 
    with_context (fun ({processor; _} : Context.t) ->
      f processor);;

  let schedule_internal ~has_yielded task = 
    with_context (fun {processor; global_queue; _} ->
      let ds = Processor.ds processor in 
      let insert_f = 
        if has_yielded 
        then DS.local_insert_after_preemption 
        else DS.local_insert
      in
      let spins = ref 0 in 
      while !spins < 30 && not (insert_f ds task) do 
        spins := !spins + 1
      done;
      if !spins == 30 then 
        (* chuck into the global queue *)
        (Custom_queue.enqueue global_queue task));;

  let schedule_awaiting to_run result = 
    match to_run with 
    | [] -> () 
    | _ ->
      List.iter (fun awaiting -> 
        schedule_internal 
          ~has_yielded:false 
          (Task.new_task (fun () -> awaiting result)))
          to_run;;

  let with_effects_handler f =
    try_with f () 
    { effc = fun (type a) (e : a eff) ->
      match e with
      | Schedule new_f ->  
        let time_start = Fast_clock.now () in
        Some (fun (k : (a, unit) continuation) -> 
          let promise = Promise.empty () in     
          schedule_internal ~has_yielded:false (Task.new_task (fun () -> 
            let time_end = Fast_clock.now () in
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
            (Task.new_task (fun () -> 
              continue k ())))
      | Await promise ->
        Some (fun k -> 
          match (Promise.await promise (continue k)) with 
          | `Task -> () 
          | `Already_done returned -> continue k returned)
      | _ -> None}

  let rec steal ~context = 
    let ({processor; all_processors; _} : Context.t) = context in 
    let num_of_processors = Array.length all_processors in 
    if num_of_processors < 2 
    then () 
    else
      (let my_id = Processor.id processor in
      let other_queue_id = Random.int num_of_processors in 
      if other_queue_id = my_id then
        steal ~context 
      else 
        (let other_processor = Array.get all_processors other_queue_id in
        let other_ds = Processor.ds other_processor in 
        let my_ds = Processor.ds processor in 
        DS.global_steal ~from:other_ds ~to_local:my_ds  
        |> ignore))
    
  let take_from_global_queue ~context = 
    let ({processor; global_queue; _} : Context.t) = context in 
    let ds = Processor.ds processor in  
    match Custom_queue.dequeue global_queue with 
    | None -> ()
    | Some task -> 
      assert (DS.local_insert ds task)
  ;;
      
  let find_work ~context =
    let ({processor; _}: Context.t) = context in 
    match Processor.take_from processor with 
    | `Global_queue -> take_from_global_queue ~context
    | `Steal -> steal ~context;;

  let rec run_domain () = 
    let scheduled = 
      with_context (fun context ->
        let ({processor; _} : Context.t) = context in 
        let task_ds = Processor.ds processor in
        match DS.local_remove task_ds with 
        | Some task -> task 
        | None -> 
          let task = ref None in 
          while Option.is_none !task do 
            find_work ~context; 
            task := DS.local_remove task_ds
          done;
          match !task with 
          | None -> assert false 
          | Some task -> task)
    in
    (match Task.get scheduled with
    | New task -> 
      (with_effects_handler task)
    | Preempted task -> 
      continue task ());
    run_domain ();;

  let setup_domain context = 
    Domain.DLS.set domain_key (Some context); 
    let ds =
      let ({processor; _} : Context.t) = context in 
      Processor.ds processor 
    in 
    DS.register_domain_id ds;
    run_domain ();;
    
  let notify_user f () =
    try f () with e ->
      let msg = Printexc.to_string e
      and stack = Printexc.get_backtrace () in
        Printf.eprintf "Uncaught exception: %s%s\n" msg stack;
        Stdlib.flush_all ();
        Stdlib.exit 1;;

  let init ?(afterwards=`join_the_pool) ?size_exponent ~(f : unit -> unit) n =
    assert (n > -1);
    let num_of_processors = 
      match afterwards with 
      | `join_the_pool -> n+1 
      | `return -> n
    in 
    let all_processors = 
      List.init num_of_processors 
        (fun id -> Processor.init ?size_exponent id) 
      |> Array.of_list 
    in
    let global_queue = Custom_queue.init () in 
    Custom_queue.enqueue global_queue (Task.new_task f);
    List.init n (fun index -> 
      let processor = Array.get all_processors index in  
      let context = 
        ({processor; all_processors; global_queue} : Context.t) 
      in 
      Domain.spawn (fun () -> 
        notify_user (setup_domain context) ()) |> ignore) 
      |> ignore;
    (* run f from within the pool *)
    match afterwards with
    | `return -> 
      {global_queue}
    | `join_the_pool -> 
      let processor = Array.get all_processors n in 
      let context = 
        ({processor; all_processors; global_queue} : Context.t) 
      in 
      setup_domain context;;

  let pending_tasks () = 
    with_context (fun ({all_processors; _} : Context.t) -> 
      Array.fold_right 
        (fun processor curr -> 
          (DS.indicative_size (Processor.ds processor)) + curr) 
      all_processors 0)

  module Stats = struct 
    let unsafe_print_latency_histogram () = 
      with_context (fun ({all_processors; _} : Context.t) -> 
        let histograms = 
          Array.map (fun processor -> 
            Processor.latency_histogram processor) 
            all_processors 
        in 
        let merged = 
          Array.to_list histograms 
          |> Histogram.merge
        in
        Array.iter Histogram.zero_out histograms;
        Histogram.dump merged);;

    let unsafe_print_executed_tasks () =
      with_context (fun {all_processors; _} -> 
        let counters = Array.map (fun processor -> 
          Processor.executed_tasks processor) 
          all_processors in 
        Array.iter Processor.zero_executed_tasks all_processors;
        counters 
        |> Array.to_list
        |> List.map Int.to_string
        |> String.concat ","
        |> Printf.printf "executed-tasks:[%s]\n");;
  end
end

module type S = sig
  type t

  val init : ?afterwards:[`join_the_pool | `return] 
    -> ?size_exponent:int 
    -> f:(unit -> unit) 
    -> int 
    -> t
  val inject_task : t -> (unit -> unit) -> unit

  val pending_tasks : unit -> int
  val scheduler_name : String.t
  module Stats : sig 
    val unsafe_print_latency_histogram : unit -> unit 
    val unsafe_print_executed_tasks : unit -> unit
  end 
end 