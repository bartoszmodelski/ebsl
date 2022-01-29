open EffectHandlers
open EffectHandlers.Deep 

type _ eff += Yield : unit eff
let yield () = perform Yield

module Scheduled = struct 
  type t = 
    | Task of (unit -> unit)
    | Terminate 
    | Preempted_task of (unit, unit) continuation
end

module Promise = struct 
  (* this should be a variant! *)
  type 'a t = {
    returned : 'a option Atomic.t;
    awaiting : ('a -> unit) List.t option Atomic.t 
  }

  let empty () = 
    ({ returned = Atomic.make None; 
    awaiting = Atomic.make (Some []) } : 'a t)

  let rec await promise f = 
    let ({awaiting; _} : 'a t) = promise in  
    let maybe_awaiting_val = Atomic.get awaiting in 
    match maybe_awaiting_val with 
    | None -> false 
    | Some awaiting_val -> 
      let new_awaiting_val = Some (f :: awaiting_val) in
      if Atomic.compare_and_set awaiting maybe_awaiting_val new_awaiting_val 
      then true 
      else await promise f;;

  let returned_exn {returned; _} = 
    match Atomic.get returned with 
    | Some v -> v 
    | None -> assert false ;;

  let fill {returned; awaiting} value =
    Atomic.set returned (Some value);
    let to_run = Atomic.get awaiting in
    Atomic.set awaiting (None); 
    match to_run with 
    | Some v -> v 
    | None -> assert false;;
end 

type _ eff += Await : 'a Promise.t -> 'a eff
let await promise = perform (Await promise)


type _ eff += Schedule : (unit -> 'a) -> 'a Promise.t eff
let schedule f = perform (Schedule f)

let domain_id_key = Domain.DLS.new_key 
  (fun () -> -1);;

let queues = ref (Array.make 0 (Spmc_queue.init () : Scheduled.t Spmc_queue.t));;

let with_task_queue f = 
  let my_id = Domain.DLS.get domain_id_key in 
  assert (my_id != -1);
  let task_queue = Array.get !queues my_id in
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
            List.iter (fun awaiting -> 
              schedule task_queue (Scheduled.Task (fun () -> awaiting result)))
              to_run)));
        continue k promise)
    | Yield -> 
      Some (fun k -> 
        with_task_queue (fun task_queue -> 
        schedule task_queue (Scheduled.Preempted_task k)))
    | Await promise ->
      Some (fun k -> 
        if Promise.await promise (continue k) 
        then () 
        else
          with_task_queue (fun task_queue -> 
            let returned = Promise.returned_exn promise in 
            schedule task_queue (Scheduled.Task (fun () -> continue k returned))))
    | _ -> None}

let steal ~my_task_queue = 
  if Domain.DLS.get domain_id_key == -1
  then assert false;
  let other_queue_id = Random.int (Array.length !queues) in 
  let other_task_queue = Array.get !queues other_queue_id in
  Spmc_queue.steal other_task_queue ~local_queue:my_task_queue 
  
  
let rec run_domain () =
  let scheduled = 
    with_task_queue (fun task_queue ->  
      if Spmc_queue.local_is_empty task_queue 
      then steal ~my_task_queue:task_queue |> ignore;  
      let maybe_task = Spmc_queue.local_dequeue task_queue in
      Option.value maybe_task ~default:(Scheduled.Task Domain.cpu_relax))
  in
  match scheduled with
  | Terminate -> ()
  | Task task -> 
    (with_effects_handler task;
    run_domain ())
  | Preempted_task task -> 
    (continue task ();
    run_domain ())

let setup_domain ~id () = 
  Domain.DLS.set domain_id_key id;
  run_domain ();;

let init ~(f : unit -> unit) n =
  queues := List.init (n+1) (fun _ -> Spmc_queue.init ()) |> Array.of_list;
  (* since this thread can schedule as well *)
  Domain.DLS.set domain_id_key n;
  let _a = List.init n (fun id -> Domain.spawn (setup_domain ~id)) in
  with_effects_handler f
