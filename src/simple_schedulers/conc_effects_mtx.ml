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

let mutex = Mutex.create ()
let with_mutex f = 
  Mutex.lock mutex;
  let result = f () in
  Mutex.unlock mutex;
  result

let task_queue = (Queue.create () : Scheduled.t Queue.t)

let with_effects_handler f =
  try_with f () 
  { effc = fun (type a) (e : a eff) ->
    match e with
    | Schedule new_f -> Some (fun (k : (a,_) continuation) -> 
      with_mutex (fun () -> Queue.add (Scheduled.Task new_f) task_queue);
        continue k ())
    | Yield -> Some (fun k -> 
      with_mutex (fun () -> Queue.add (Scheduled.Preempted_task k) task_queue))
    | Dump_stats -> Some (fun k -> 
      let size = with_mutex (fun () -> (Queue.length task_queue)) in
      Printf.printf "  queue size: %d\n" size;
      Stdlib.flush Stdlib.stdout;
      continue k ())
    | _ -> None }


let rec run_domain () =
  Mutex.lock mutex;
  let scheduled = 
    if Queue.is_empty task_queue 
    then (Scheduled.Task Domain.cpu_relax)
    else (Queue.take task_queue);
  in
  Mutex.unlock mutex;
  match scheduled with
  | Terminate -> ()
  | Task task -> 
    (with_effects_handler task;
    run_domain ())
  | Preempted_task task -> 
    (continue task ();
    run_domain ())

let init ~(f : unit -> unit) n = 
  let _a = List.init n (fun _ -> Domain.spawn run_domain) in
  with_effects_handler f
