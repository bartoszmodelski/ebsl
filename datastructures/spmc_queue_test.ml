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


let%expect_test _ = 
  let q = (Spmc_queue.init ~size_exponent:4 () : int Spmc_queue.t) in 
  let q_2 = (Spmc_queue.init ~size_exponent:4 () : int Spmc_queue.t) in 
  for i = 0 to 14 do  
    assert (Spmc_queue.local_enqueue q i); 
  done;
  dump_spmc q;
  [%expect {|
    head: 0, tail 15
     data: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, _ |}];
  for i = 1 to 5 do 
    let stolen = Spmc_queue.steal ~from:q ~to_local:q_2 in 
    Printf.printf "=====\nSteal #%d. Stole %d items.\n" i stolen;
    dump_spmc q; 
    Printf.printf "--->\n";
    dump_spmc q_2;
    Printf.printf "\n";
  done;
  [%expect {|
    =====
    Steal #1. Stole 8 items.
    head: 8, tail 15
     data: _, _, _, _, _, _, _, _, 8, 9, 10, 11, 12, 13, 14, _

    --->
    head: 0, tail 8
     data: 0, 1, 2, 3, 4, 5, 6, 7, _, _, _, _, _, _, _, _


    =====
    Steal #2. Stole 4 items.
    head: 12, tail 15
     data: _, _, _, _, _, _, _, _, _, _, _, _, 12, 13, 14, _

    --->
    head: 0, tail 12
     data: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, _, _, _, _


    =====
    Steal #3. Stole 2 items.
    head: 14, tail 15
     data: _, _, _, _, _, _, _, _, _, _, _, _, _, _, 14, _

    --->
    head: 0, tail 14
     data: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, _, _


    =====
    Steal #4. Stole 1 items.
    head: 15, tail 15
     data: _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _

    --->
    head: 0, tail 15
     data: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, _


    =====
    Steal #5. Stole 0 items.
    head: 15, tail 15
     data: _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _

    --->
    head: 0, tail 15
     data: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, _ |}];;
    