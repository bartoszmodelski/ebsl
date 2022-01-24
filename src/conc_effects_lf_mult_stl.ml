open EffectHandlers
open EffectHandlers.Deep 

type _ eff += Yield : unit eff
let yield () = perform Yield

type _ eff += Schedule : (unit -> unit) -> unit eff
let schedule f = perform (Schedule f)

module Scheduled = struct 
  type t = 
    | Task of (unit -> unit)
    | Terminate
    | Preempted_task of (unit,unit) continuation
end

let domain_id_key = Domain.DLS.new_key 
  (fun () -> -1);;

let queues = ref (Array.make 0 (Spmc_queue.init () : Scheduled.t Spmc_queue.t));;

let with_task_queue f = 
  let my_id = Domain.DLS.get domain_id_key in 
  let queue_id =
    if my_id != -1  
    then my_id 
    else (Array.length !queues) - 1 (* hack *)
  in 
  let task_queue = Array.get !queues queue_id in
  f task_queue 

let with_effects_handler f =
  try_with f () 
  { effc = fun (type a) (e : a eff) ->
    match e with
    | Schedule new_f -> Some (fun (k : (a,_) continuation) -> 
      with_task_queue (fun task_queue -> 
        Spmc_queue.local_enqueue task_queue (Scheduled.Task new_f) |>
        function 
          | true -> () 
          | false -> Stdlib.exit 255);
        continue k ())
    | Yield -> Some (fun k -> 
      with_task_queue (fun task_queue -> 
        Spmc_queue.local_enqueue task_queue (Scheduled.Preempted_task k) |>
        function 
          | true -> () 
          | false -> Stdlib.exit 255))
    | _ -> None }



let steal ~my_task_queue = 
  if Domain.DLS.get domain_id_key == -1
  then (* internal error, sadly throwing exception here doesn't kill other threads *)
    Stdlib.exit 255;
  let other_queue_id = Random.int (Array.length !queues) in 
  let other_task_queue = Array.get !queues other_queue_id in
  Spmc_queue.steal_half other_task_queue ~local_queue:my_task_queue 
  
  
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
  (* we technically should have a barrier here *)
  let _a = List.init n (fun id -> Domain.spawn (setup_domain ~id)) in
  with_effects_handler f
