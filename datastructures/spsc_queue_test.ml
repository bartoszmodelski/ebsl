module Atomic = Dscheck.TracedAtomic

let dump_spsc ?(with_mask=false) ({head; tail; array; mask; _} : int Spsc_queue.t) = 
  let head = Atomic.get head in 
  let tail = Atomic.get tail in 
  let data = Array.to_list array
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
  let q = (Spsc_queue.init ~size_exponent:4 () : int Spsc_queue.t) in 
  dump_spsc q;
  [%expect {|
    head: 0, tail 0
     data: _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _ |}];
  for i = 0 to 15 do  
    assert (Spsc_queue.enqueue q i); 
  done;
  assert (not (Spsc_queue.enqueue q 16));
  dump_spsc q;
  [%expect {|
    head: 0, tail 16
     data: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 |}];
  let output = List.init 16 (fun _ -> 
    Spsc_queue.dequeue q |> 
      function 
      | None -> assert false 
      | Some v -> Int.to_string v) 
    |> String.concat "," 
  in 
  assert (Spsc_queue.dequeue q |> Option.is_none);
  dump_spsc q;
  Printf.printf "dequeued: %s\n" output;
  [%expect {|
    head: 16, tail 16
     data: _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _

    dequeued: 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15 |}];;
