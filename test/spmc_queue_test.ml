module Atomic = Dscheck.TracedAtomic
(*

let dump_spmc ({head; tail; mask = _; buffer} : int Spmc_queue.t) = 
  let head = Atomic.get head in 
  let tail = Atomic.get tail in 
  let data = Array.to_list buffer 
    |> List.map Atomic.get
    |> List.map (function 
      | Spmc_queue.Cell.Value v -> Int.to_string v
      | Empty -> "empty") 
    |> String.concat ", "
  in
  Printf.printf "head: %d, tail %d\n data: %s\n\n" head tail data;;


let count ({buffer; _} : int Spmc_queue.t) = 
  let vals = Array.map 
    (fun v ->
      let v = Atomic.get v in 
      match v with 
      | Spmc_queue.Cell.Value _ -> 1 
      | Empty -> 0) 
    buffer 
  in
  List.fold_right Int.add (Array.to_list vals) 0;;

let assert_enqueued = function
  | Spmc_queue.Enqueue_result.Enqueued -> () 
  | Overloaded -> assert false 

let assert_overloaded = function
  | Spmc_queue.Enqueue_result.Enqueued -> assert false 
  | Overloaded -> () 

let assert_dequeued v_1 = function 
  | Spmc_queue.Dequeue_result.Dequeued v_2 -> 
    if (v_1 = v_2) then () 
    else 
      (Printf.printf "compared values not equal: %d %d\n" v_1 v_2;
      assert false)
  | Empty -> assert false 

let assert_empty = function 
  | Spmc_queue.Dequeue_result.Dequeued _ -> assert false 
  | Empty -> () 

let _test_1 () = 
  let q = (Spmc_queue.init () : int Spmc_queue.t) in 
  dump_spmc q;
  for i = 1 to 64 do  
    Spmc_queue.local_enqueue q i |> assert_enqueued
  done;
  dump_spmc q;
  Spmc_queue.local_enqueue q 65 |> assert_overloaded;
  dump_spmc q;
  for i = 1 to 64 do  
    Spmc_queue.local_dequeue q |> assert_dequeued i;
  done;
  dump_spmc q;
  Spmc_queue.local_dequeue q |> assert_empty;
  dump_spmc q;
  for i = 1 to 64 do  
    Spmc_queue.local_enqueue q i |> assert_enqueued
  done;
  dump_spmc q;;




let _test_2 () = 
  Random.self_init ();
  let q = (Spmc_queue.init () : int Spmc_queue.t) in 
  let queue_size = ref 0 in 
  let last_val = ref 0 in
  for i = 1 to 100000000 do  
    if Random.int 100 > 50 then 
      (match Spmc_queue.local_enqueue q i with 
      | Overloaded -> ()
      | Enqueued -> queue_size := !queue_size + 1)
    else
      (match Spmc_queue.local_dequeue q with 
      | Empty -> ()
      | Dequeued v -> 
        if !last_val >= v then assert false;
        last_val := v;
        queue_size := !queue_size - 1);
  done;
  Printf.printf "Expected queue size: %d\n" !queue_size;
  dump_spmc q;;


let _test_2_compare_throughput () = 
  Random.self_init ();
  let q = (Queue.create () : int Queue.t) in 
  let queue_size = ref 0 in 
  for i = 1 to 100000000 do  
    if Random.int 100 > 50 then 
      if Queue.length q < 64 then 
        (Queue.add i q; 
        queue_size := !queue_size + 1)
      else 
        ()
    else
      (if Queue.take_opt q |> Option.is_some then 
        (queue_size := !queue_size - 1)
      else 
        ())
  done;
  Printf.printf "Expected queue size: %d\n" !queue_size;
  Printf.printf "Actual queue size: %d\n" (Queue.length q);;

let time_f f =
  let t = Sys.time() in
  Sys.opaque_identity (f ());
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  ();;
  

let _test_2_with_stealer () = 
  Random.self_init ();
  let q = (Spmc_queue.init () : int Spmc_queue.t) in 
  let queue_size = ref 0 in 
  let last_val = ref 0 in
  let domain = Domain.spawn (fun () ->  
    let stolen_count = ref 0 in  
    for _ = 1 to 100000 do
      let q_tmp = (Spmc_queue.init () : int Spmc_queue.t) in  
      Spmc_queue.steal q ~local_queue:q_tmp;
      let ({tail; _ } : int Spmc_queue.t) = q_tmp in
      let tail = Atomic.get tail in 
      stolen_count := !stolen_count + tail - 1 
    done;
    !stolen_count) 
  in
  for i = 1 to 100000000 do  
    if Random.int 100 > 50 then 
      (match Spmc_queue.local_enqueue q i with 
      | Overloaded -> ()
      | Enqueued -> queue_size := !queue_size + 1)
    else
      (match Spmc_queue.local_dequeue q with 
      | Empty -> ()
      | Dequeued v -> 
        if !last_val >= v then assert false;
        last_val := v;
        queue_size := !queue_size - 1);
  done;
  let stolen = Domain.join domain in
  Printf.printf "Expected queue size: %d, actual queue size: %d, stolen: %d\n" 
    !queue_size (count q) stolen;
  dump_spmc q;
  if !queue_size = (count q) + stolen then 
    Printf.printf "\n\n PASS\n" 
  else assert false;;  

(* 
let () = time_f _test_2_with_stealer
*)

*)