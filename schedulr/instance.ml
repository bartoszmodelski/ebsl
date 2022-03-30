

module FIFO = Scheduler.Make(struct 
  include Spmc_queue
  
  let local_insert = local_enqueue
  let local_remove = local_dequeue
  let local_insert_after_preemption = local_enqueue  

  let global_steal = steal
  let name = "FIFO"
end)


module Stack_ext = struct
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

  let global_steal ~from ~to_local = 
    steal ~from ~to_local ()
end

module LIFO = Scheduler.Make(struct
  include Stack_ext
  let name = "LIFO"
end) 

module Hybrid_random = Scheduler.Make(struct 
  include Stack_ext
  
  let local_remove t = 
    if Random.bool () then
    steal ~auto_retry:true ~steal_size_limit:1 ~from:t ~to_local:t () |> ignore; 
    local_pop t;;

  let name = "Hybrid_random"
end)

module Hybrid_alternating = Scheduler.Make(struct 
  include Stack_ext
  
  let previously_stole = ref false 

  let local_remove t = 
    if not !previously_stole
    then
      steal ~auto_retry:true ~steal_size_limit:1 ~from:t ~to_local:t () 
      |> ignore;

    previously_stole := not !previously_stole;
    local_pop t;;
  
  let name = "Hybrid_alternating"
end)


module Hybrid_reverse_every_n = Scheduler.Make(struct 
  include Stack_ext
  
  let threshold = 200
  let curr_n = ref 0 
  let to_steal = ref 0 

  let local_remove t = 
    if !to_steal > 0
    then
      (steal ~auto_retry:true ~steal_size_limit:1 ~from:t ~to_local:t () 
      |> ignore; 
      to_steal := !to_steal - 1) 
    else if !curr_n > threshold 
    then
      (curr_n := 0;
      to_steal := indicative_size t)
    else 
      curr_n := !curr_n + 1;
    local_pop t;;

  let name = "Hybrid_reverse_every_n"
end)