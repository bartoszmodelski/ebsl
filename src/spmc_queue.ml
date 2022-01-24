module Atomic = Dscheck.TracedAtomic


(* Notes: 
  - Tail does not have to be atomic because there's just one writer.
  - Local deque does not have to synchronize with the writer, only other readers.
  - quasi_tail is quasi because the real tail is the cell with oldest Value.
  - Overloaded doesn't necessarily mean the queue is full, but there's no way to 
  enqueue. 

  We could just skip over the allocated entries in hope some other dequeuers were
  faster but keeping track of the size of the queue becomes troublesome. 

  - Local dequeue and enqueue are wait-free. Normal dequeue (steal) may spin, but 
  it only has to do it once for any number of elements. 
  - Lots of accesses do not have to be atomic but that's the only way current lib
  allows 'a Atomic.t to be accessed. Probably doesn't matter much on x86. 
  *)

type 'a t = {
  head : int Atomic.t; 
  tail : int Atomic.t;
  mask : int;
  array : 'a Atomic.t Array.t
} 

let empty_cell = Obj.magic 0 

let local_is_empty {head; tail; _} =
  let tail_val = Atomic.get tail in 
  let head_val = Atomic.get head in 
  tail_val <= head_val 

let init ?(size_pow=21) () =
  let size = Int.shift_left 1 size_pow in
  { head = Atomic.make 0;
    tail = Atomic.make 0;
    mask = size - 1;
    array = Array.init size (fun _ -> Atomic.make empty_cell)}

let local_enqueue {tail; mask; array; _} element =
  let index = (Atomic.get tail) land mask in 
  let cell = Array.get array index in 
  if Atomic.get cell != empty_cell then (
    Printf.printf "empty?"; 
    Stdlib.flush_all ();
    false)
  else 
    (Atomic.set cell element;
    Atomic.incr tail;
    true);;

let local_dequeue {head; tail; mask; array} : 'a option =
  (* local deque is optimistic because it can fix its mistake if needed *)
  let index = Atomic.fetch_and_add head 1 in
  let tail_val = Atomic.get tail in
  if index = tail_val then
    (Atomic.decr head;
    None)
  else if index > tail_val then
    assert false 
  else 
    (let cell = Array.get array (index land mask) in
    let element = Atomic.get cell in
    Atomic.set cell empty_cell; 
    Some element);;

    
let local_is_half_empty {head = _; tail; mask; array} : bool =
  let size = Array.length array in 
  let tail_value = Atomic.get tail in
  let seen_not_free = ref false in 
  let i = ref 0 in 
  while not !seen_not_free && !i < (size + 1)/ 2 do 
    let cell = Array.get array ((tail_value + !i) land mask) in 
    seen_not_free := Atomic.get cell != empty_cell; 
    i := !i + 1
  done; 
  not !seen_not_free

let steal_half {head; tail; mask; array} ~local_queue =
  (* if we only initiate stealing after running out of 
    tasks then this check is not needed *)
  (*if not (local_is_half_empty local_queue) then 
    false 
  else*)
  (let head_val = Atomic.get head in 
  let tail_val = Atomic.get tail in 
  let size = tail_val - head_val in 
  if size < 1  
  then false 
  else 
    (let stealable = 
      (* We want to steal even if there's a single element, thus +1 *)
      (size + 1)/2  
    in
    let new_head_val = head_val + stealable in
    if new_head_val > tail_val then (
        (* Printf.printf "new_hd %d, hd %d, tl %d, sz %d l\n" new_head_val 
        head_val tail_val size; *) 
        assert false
      );
    if not (Atomic.compare_and_set head head_val new_head_val)
    then
      false
    else 
      (for i = 0 to stealable - 1 do
        let cell = Array.get array ((head_val + i) land mask) in
        assert (local_enqueue local_queue (Atomic.get cell));
        Atomic.set cell empty_cell;
      done;
      true)));; 
  
let total_checked = ref 0

let create_test () =
  let queue = init ~size_pow:5 () in
  let queue_2 = init ~size_pow:5 () in
  Atomic.spawn (fun () -> 
    assert (local_enqueue queue "");
    assert (local_enqueue queue "");
    assert (Option.is_some (local_dequeue queue)));
  Atomic.spawn (fun () -> 
    while not (steal_half queue ~local_queue:queue_2) do () done);
  Atomic.final (fun () ->
    total_checked := !total_checked + 1;
    let ({head; tail; _} : string t) = queue in
    let head_value = Atomic.get head in
    let tail_value = Atomic.get tail in
    (*Atomic.check (fun () -> 
      Array.for_all (fun v -> Atomic.get v = empty_cell) array);*)
    Atomic.check (fun () -> 
      head_value = tail_value));;
  
let () =
  Atomic.trace ~depth_limit:32 create_test;
  Printf.printf "Total checked: %d\n" (!total_checked);;