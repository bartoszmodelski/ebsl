open EffectHandlers
open EffectHandlers.Deep 

type _ eff += Yield : unit eff
let yield () = perform Yield

type _ eff += Schedule : (unit -> unit) -> unit eff
let schedule f = perform (Schedule f)

type _ eff += Dump_stats : unit eff
let dump_stats () = perform Dump_stats

module Scheduled = struct 
  type t = 
    | Task of (unit -> unit)
    | Terminate
    | Preempted_task of (unit,unit) continuation
end

let domain_id_key = Domain.DLS.new_key 
  (fun () -> -1);;

let queues = ref (Array.make 0 
  (Mutex.create (), (Queue.create () : Scheduled.t Queue.t)));;

let with_task_queue f = 
  let my_id = Domain.DLS.get domain_id_key in 
  let queue_id =
    if my_id != -1 
    then my_id 
    else Random.int (Array.length !queues)
  in 
  let (mutex, task_queue) = Array.get !queues queue_id in
  Mutex.lock mutex;
  let result = f task_queue in
  Mutex.unlock mutex;
  result;;

let with_effects_handler f =
  try_with f () 
  { effc = fun (type a) (e : a eff) ->
    match e with
    | Schedule new_f -> Some (fun (k : (a,_) continuation) -> 
      with_task_queue (fun task_queue -> Queue.add (Scheduled.Task new_f) task_queue);
        continue k ())
    | Yield -> Some (fun k -> 
      with_task_queue (fun task_queue -> Queue.add (Scheduled.Preempted_task k) task_queue))
    | Dump_stats -> Some (fun k -> 
      let size = with_task_queue (fun task_queue -> (Queue.length task_queue)) in
      Printf.printf " local queue size: %d\n" size;
      Stdlib.flush Stdlib.stdout;
      continue k ())
    | _ -> None }

let rec run_domain () =
  let scheduled = 
    with_task_queue (fun task_queue -> 
      if Queue.is_empty task_queue 
      then (Scheduled.Task Domain.cpu_relax)
      else (Queue.take task_queue))
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
  queues := List.init n (fun _ -> Mutex.create (), 
    (Queue.create () : Scheduled.t Queue.t)) |> Array.of_list;
  let _a = List.init n (fun id -> Domain.spawn (setup_domain ~id)) in
  with_effects_handler f
