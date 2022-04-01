module Atomic = Dscheck.TracedAtomic

let dump_spmc ?(with_mask=false) ({head; tail; array; mask; _} : int Spmc_queue.t) = 
  let mask = Atomic.get mask in 
  let head = Atomic.get head in 
  let tail = Atomic.get tail in 
  let data = Array.to_list (Atomic.get array) 
    |> List.map Atomic.get
    |> List.map (function 
      | Some v -> Int.to_string v
      | None -> "_") 
    |> String.concat ", "
  in
  let mask_s = 
    if not with_mask 
    then ""
    else Printf.sprintf ", mask: %d" mask
  in
  Printf.printf "head: %d, tail %d%s\n data: %s\n\n" head tail mask_s data;;


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


let%expect_test "steal" = 
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
    

let%expect_test "resize" = 
  let q = (Spmc_queue.init ~size_exponent:2 () : int Spmc_queue.t) in 
  for i = 0 to 3 do  
    assert (Spmc_queue.local_enqueue q i); 
  done;
  assert (not (Spmc_queue.local_enqueue q 4));
  dump_spmc ~with_mask:true q;
  [%expect {|
    head: 0, tail 4, mask: 3
     data: 0, 1, 2, 3 |}];
  Spmc_queue.local_resize q;
  dump_spmc ~with_mask:true q;
  [%expect {|
    head: 0, tail 4, mask: 7
     data: 0, 1, 2, 3, _, _, _, _ |}];
  assert (Spmc_queue.local_enqueue q 4);
  dump_spmc q;
  [%expect {|
    head: 0, tail 5
     data: 0, 1, 2, 3, 4, _, _, _ |}];;  


let%expect_test "auto-resize" = 
  let q = (Spmc_queue.init ~size_exponent:2 () : int Spmc_queue.t) in 
  dump_spmc q;
  [%expect {|
    head: 0, tail 0
     data: _, _, _, _ |}];
  for i = 0 to 9 do  
    Spmc_queue.local_enqueue_with_resize q i; 
  done;
  dump_spmc q;
  [%expect {|
    head: 0, tail 10
     data: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, _, _, _, _, _, _ |}];
  for _ = 0 to 4 do 
    ignore (Spmc_queue.local_dequeue q);
  done;
  dump_spmc q;
  [%expect {|
    head: 5, tail 10
     data: _, _, _, _, _, 5, 6, 7, 8, 9, _, _, _, _, _, _ |}];
  for i = 0 to 9 do  
    Spmc_queue.local_enqueue_with_resize q (20+i); 
  done;
  dump_spmc q;
  [%expect {|
    head: 5, tail 20
     data: 26, 27, 28, 29, _, 5, 6, 7, 8, 9, 20, 21, 22, 23, 24, 25 |}];