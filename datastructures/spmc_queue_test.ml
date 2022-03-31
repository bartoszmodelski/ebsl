module Atomic = Dscheck.TracedAtomic

let dump_spmc ({head; tail; array; _} : int Spmc_queue.t) = 
  let head = Atomic.get head in 
  let tail = Atomic.get tail in 
  let data = Array.to_list array 
    |> List.map Atomic.get
    |> List.map (function 
      | Some v -> Int.to_string v
      | None -> "_") 
    |> String.concat ", "
  in
  Printf.printf "head: %d, tail %d\n data: %s\n\n" head tail data;;


let%expect_test _ = 
  let q = (Spmc_queue.init ~size_exponent:4 () : int Spmc_queue.t) in 
  dump_spmc q;
  [%expect {|
    head: 0, tail 0
     data: _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _ |}];
  for i = 0 to 15 do  
    assert (Spmc_queue.local_enqueue q i); 
  done;
  assert (not (Spmc_queue.local_enqueue q 16));
  dump_spmc q;
  [%expect {|
    head: 0, tail 16
     data: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 |}];
  let output = List.init 16 (fun _ -> Spmc_queue.local_dequeue q |> 
      function | None -> assert false | Some v -> Int.to_string v) 
    |> String.concat "," 
  in 
  assert (Spmc_queue.local_dequeue q |> Option.is_none);
  assert (Spmc_queue.local_is_empty q);
  dump_spmc q;
  Printf.printf "dequeued: %s\n" output;
  [%expect {|
    head: 16, tail 16
     data: _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _

    dequeued: 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15 |}];;
