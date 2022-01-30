open EffectHandlers
open EffectHandlers.Deep 

let with_mutex mtx f =
  Mutex.lock mtx; 
  let v = f () in
  Mutex.unlock mtx; 
  v;; 

type _ eff += Yield : unit eff
let yield () = perform Yield

module Scheduled = struct 
  type t = 
    | Task of (unit -> unit)
    (* | Terminate *)
    | Preempted_task of (unit, unit) continuation
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
    let ({awaiting; mutex; _} : 'a t) = promise in  
    with_mutex mutex (fun () ->  
      match !awaiting with 
      | None -> false 
      | Some awaiting_val -> 
        awaiting := Some (f :: awaiting_val); 
        true)

  let returned_exn {returned; mutex; _} = 
    with_mutex mutex (fun () -> 
    match !returned with 
    | Some v -> v 
    | None -> assert false);;

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

type _ eff += Await : 'a Promise.t -> 'a eff
let await promise = perform (Await promise)

type _ eff += Schedule : (unit -> 'a) -> 'a Promise.t eff
let schedule f = perform (Schedule f)

let domain_id_key = Domain.DLS.new_key 
  (fun () -> -1);;

let queues = ref (Array.make 0 (Spmc_queue.init () : Scheduled.t Spmc_queue.t));;

let with_task_queue f =
  let id = Domain.DLS.get domain_id_key in 
  let task_queue = Array.get !queues id in
  f task_queue;;

let with_effects_handler f =
  let schedule task_queue s = 
    while not (Spmc_queue.local_enqueue task_queue s) do () done
  in
  try_with f () 
  { effc = fun (type a) (e : a eff) ->
    match e with
    | Schedule new_f -> 
      Some (fun (k : (a, unit) continuation) -> 
        let promise = Promise.empty () in 
        with_task_queue (fun task_queue -> 
          schedule task_queue (Scheduled.Task (fun () -> 
            let result = new_f () in 
            let to_run = Promise.fill promise result in 
          (* Note, this is inside a scheduled task, which can be 
            stolen. Must look the queue up again. *)
          with_task_queue (fun task_queue ->
            List.iter (fun awaiting -> 
              schedule task_queue (Scheduled.Task (fun () -> awaiting result)))
              to_run))));
        continue k promise)
    | Yield -> 
      Some (fun k -> 
        with_task_queue (fun task_queue -> 
          schedule task_queue (Scheduled.Preempted_task k)))
    | Await promise ->
      Some (fun k -> 
        if not (Promise.await promise (continue k)) 
        then 
          let returned = Promise.returned_exn promise in 
          continue k returned)
    | _ -> None}

let steal ~my_task_queue = 
  let my_id = Domain.DLS.get domain_id_key in
  assert (my_id != -1);
  let other_queue_id = Random.int (Array.length !queues) in 
  if other_queue_id = my_id then
    ()
  else 
    (let other_task_queue = Array.get !queues other_queue_id in
    let stolen = Spmc_queue.steal ~from:other_task_queue ~to_local:my_task_queue in 
    if stolen > 0 then (
      Printf.printf "!"; 
      Stdlib.flush_all ()))
  
let rec run_domain () =
  let scheduled = 
    with_task_queue (fun task_queue ->  
      match Spmc_queue.local_dequeue task_queue with 
      | Some task -> task 
      | None -> 
        if Spmc_queue.local_is_empty task_queue 
        then (steal ~my_task_queue:task_queue);  
        Option.value (Spmc_queue.local_dequeue task_queue)
          ~default:(Scheduled.Task Domain.cpu_relax))
  in
  match scheduled with
  (* | Terminate -> () *)
  | Task task -> 
    (with_effects_handler task;
    run_domain ())
  | Preempted_task task -> 
    (continue task ();
    run_domain ())

let setup_domain ~id () = 
  Domain.at_exit (fun () -> assert false);
  Domain.DLS.set domain_id_key id;
  let queue = Array.get !queues id in 
  Spmc_queue.register_domain_id queue;
  run_domain ();;
  
let notify_user f () =
  try f () with e ->
    let msg = Printexc.to_string e
    and stack = Printexc.get_backtrace () in
      Printf.eprintf "There was an error: %s%s\n" msg stack;
      assert false;;
  
let init ~(f : unit -> unit) n =
  queues := List.init (n+1) (fun _ -> Spmc_queue.init ()) |> Array.of_list;
  (* since this thread can schedule as well *)
  let _domains = List.init n (fun id ->
    Domain.spawn (notify_user (setup_domain ~id))) in
  (* run f from within the pool *)
  Domain.DLS.set domain_id_key n;
  Spmc_queue.register_domain_id (Array.get !queues n);
  with_effects_handler f;;
  (* run_domain () *) 