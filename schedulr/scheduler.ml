open EffectHandlers
open EffectHandlers.Deep 

(*module Custom_queue = Datastructures.Multi_mpmc_queue.Make(struct 
  let num_of_queues = 1
end) 
*)

(* module Custom_queue = Datastructures.Mpmc_queue *)

module Custom_queue = Datastructures.Lock_queue

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
      executed_tasks : int ref;
      steal_attempts : int ref; 
      waited_for_space_on_enque : int ref;
      local_requesting_queue : Task.t Datastructures.Queue_of_queues.Local.t;
      suggest_steal : int option Atomic.t; 
    }

    let init ?size_exponent requesting_queue id = 
      {
        ds = DS.init ?size_exponent ();
        id; 
        executed_tasks = ref 0;
        steal_attempts = ref 0;
        waited_for_space_on_enque = ref 0;
        local_requesting_queue = Datastructures.Queue_of_queues.Local.init requesting_queue;
        suggest_steal = Atomic.make None;
      }
    
    let id {id; _} = id
    let ds {ds; _} = ds
    let incr_tasks {executed_tasks; _} = 
      executed_tasks := !executed_tasks + 1

    let executed_tasks {executed_tasks; _} =
      !executed_tasks

    let zero_executed_tasks {executed_tasks; _} =
      executed_tasks := 0

    let incr_waited_for_space_on_enque {waited_for_space_on_enque; _} =
      waited_for_space_on_enque := !waited_for_space_on_enque + 1;;

    let waited_for_space_on_enque {waited_for_space_on_enque; _ } =
      !waited_for_space_on_enque;;
    
    let zero_waited_for_space_on_enque {waited_for_space_on_enque; _} = 
      waited_for_space_on_enque := 0;;

    let take_from {steal_attempts; suggest_steal = _; _} = 
      steal_attempts := !steal_attempts + 1;
      if !steal_attempts mod 500 = 0 then 
        `Global_queue 
      else (* 
        (let kind = 
          match Atomic.get suggest_steal with 
          | None -> None 
          | Some id -> 
            (Atomic.set suggest_steal None;
            Some id)
        in 
        `Steal kind)
      *)
        `Steal None      
      ;;

    let set_suggest_steal {suggest_steal; _} id = 
      Atomic.set suggest_steal (Some id);;

  end 

  module Context = struct 
    type t = {
      processor : Processor.t;
      all_processors : Processor.t Array.t;
      global_queue : Task.t Custom_queue.t;
      requesting_queue : Task.t Datastructures.Queue_of_queues.Global.t;
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


  let random_id ({processor; all_processors; _} : Context.t) =
    let num_of_other_processors = Array.length all_processors - 1 in 
    if num_of_other_processors < 1 
    then None 
    else
      Some (let r = Random.int num_of_other_processors in 
        let my_id = Processor.id processor in
        if r == my_id then
          r + 1 
        else 
          r);;

  let task_pressure context =
    let ({all_processors; processor; _} : Context.t) = context in 
    match random_id context with 
    | None -> () 
    | Some other_id ->
      let my_id = Processor.id processor in 
      let other_processor = Array.get all_processors other_id in 
      Processor.set_suggest_steal other_processor my_id;;

  let schedule_internal ~has_yielded task = 
    with_context (fun context ->
      let ({processor; global_queue; _} : Context.t) = context in 
      let ds = Processor.ds processor in 
      let insert_f = 
        if has_yielded 
        then DS.local_insert_after_preemption 
        else DS.local_insert
      in
      let spin_threshold = 30_000_000_000 in 
      let spins = ref 0 in 
      while !spins < spin_threshold && not (insert_f ds task) do 
        Processor.incr_waited_for_space_on_enque processor;
        spins := !spins + 1
      done;
      if !spins == spin_threshold then 
        (task_pressure context;
        (* chuck into the global queue *)
        Custom_queue.enqueue global_queue task));;

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
        Some (fun (k : (a, unit) continuation) -> 
          let promise = Promise.empty () in     
          schedule_internal ~has_yielded:false (Task.new_task (fun () -> 
            with_processor (fun processor -> 
              Processor.incr_tasks processor); 
            let result = new_f () in 
            let to_run = Promise.fill promise result in 
            (* it's tempting to re-use looked up queue here but this may 
            run on a different processor and violate local_  *)
            schedule_awaiting to_run result));
          continue k promise)
      | Yield -> 
        Some (fun k -> 
          with_context (fun {global_queue; _} -> 
            Custom_queue.enqueue global_queue (Task.new_task (fun () -> 
              continue k ()))))
      | Await promise ->
        Some (fun k -> 
          match (Promise.await promise (continue k)) with 
          | `Task -> () 
          | `Already_done returned -> continue k returned)
      | _ -> None}

  let steal ~context ~force_id = 
    let ({processor; all_processors; _} : Context.t) = context in 
    match force_id, random_id context with 
    | None, None -> () 
    | (Some other_queue_id, (Some _ | None)) 
    | (None, Some other_queue_id) ->
      (let other_processor = Array.get all_processors other_queue_id in
      let other_ds = Processor.ds other_processor in 
      let my_ds = Processor.ds processor in 
      DS.global_steal ~from:other_ds ~to_local:my_ds  
      |> ignore);;
    
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
    | `Steal id -> steal ~context ~force_id:id ;;

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

  let setup_domain context () = 
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

  let init ?(afterwards=`join_the_pool) ?overflow_size_exponent ?size_exponent ~(f : unit -> unit) n =
    assert (n > -1);
    let num_of_processors = 
      match afterwards with 
      | `join_the_pool -> n+1 
      | `return -> n
    in 
    let requesting_queue = Datastructures.Queue_of_queues.Global.init () in
    let all_processors = 
      List.init num_of_processors 
        (fun id -> Processor.init ?size_exponent requesting_queue id) 
      |> Array.of_list 
    in
    let global_queue = Custom_queue.init ?size_exponent:overflow_size_exponent () in 
    Custom_queue.enqueue global_queue (Task.new_task f);
    let counter = Atomic.make 0 in 
    List.init n (fun index -> 
      let processor = Array.get all_processors index in  
      let context = 
        ({processor; all_processors; global_queue; requesting_queue} : Context.t) 
      in 
      Domain.spawn (fun () -> 
        notify_user (fun () -> 
          Atomic.incr counter;
          setup_domain context ()) ()) |> ignore) 
      |> ignore;
    while Atomic.get counter < n do () done;
    (* run f from within the pool *)
    match afterwards with
    | `return -> 
      {global_queue}
    | `join_the_pool -> 
      let processor = Array.get all_processors n in 
      let context = 
        ({processor; all_processors; global_queue; requesting_queue} : Context.t) 
      in 
      notify_user (setup_domain context) ();;

  let pending_tasks () = 
    with_context (fun ({all_processors; _} : Context.t) -> 
      Array.fold_right 
        (fun processor curr -> 
          (DS.indicative_size (Processor.ds processor)) + curr) 
      all_processors 0)

  module Stats = struct 
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
        |> Printf.printf "\"executed-tasks\":[%s],\n");;

    let unsafe_print_waited_for_space_on_enque () =
      with_context (fun {all_processors; _} -> 
        let counters = Array.map (fun processor -> 
          Processor.waited_for_space_on_enque processor) 
          all_processors in 
        Array.iter Processor.zero_waited_for_space_on_enque all_processors;
        counters 
        |> Array.to_list
        |> List.fold_left (+) 0
        |> Int.to_string
        |> Printf.printf "\"waited-for-space-on-enque\":%s,\n");;
  end
end

module type S = sig
  type t

  val init : ?afterwards:[`join_the_pool | `return] 
    -> ?overflow_size_exponent:int 
    -> ?size_exponent:int 
    -> f:(unit -> unit) 
    -> int 
    -> t
  val inject_task : t -> (unit -> unit) -> unit

  val pending_tasks : unit -> int
  val scheduler_name : String.t
  module Stats : sig 
    val unsafe_print_executed_tasks : unit -> unit
    val unsafe_print_waited_for_space_on_enque : unit -> unit
  end 
end 