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

module Enqueue_result = struct 
  type t = 
    | Enqueued 
    | Overloaded  
end 

module Dequeue_result = struct 
  type 'a t = 
    | Dequeued of 'a 
    | Empty 
end 

module Cell = struct 
  (* make it a pointer *)
  type 'a t =
    | Value of 'a 
    | Empty 

  let is_val = function
    | Empty -> false 
    | Value _ -> true

  let is_empty v = not (is_val v)
  let get_val = function 
    | Value v -> v 
    | Empty -> assert false

  let get_val2 = function 
  | Value v -> v 
  | Empty -> assert false
end

type 'a t = {
  quasi_head : int Atomic.t; 
  tail : int Atomic.t;
  mask : int;
  buffer : 'a Cell.t Atomic.t Array.t
} 

let local_is_empty {quasi_head; tail; mask = _; buffer = _} =
  let tail_val = Atomic.get tail in 
  let quasi_head_val = Atomic.get quasi_head in 
  tail_val = quasi_head_val + 1


let init ?(size_pow=21) () =
  let size = Int.shift_left 1 size_pow in
  { quasi_head = Atomic.make 0;
    tail = Atomic.make 1;
    mask = size - 1;
    buffer = Array.make size (Atomic.make Cell.Empty)
      |> Array.map (fun _ -> Atomic.make Cell.Empty)}

let local_enqueue {quasi_head = _; tail; mask; buffer} element : Enqueue_result.t =
  let next_cell = 
    let next_cell_index = (Atomic.get tail) land mask in 
    Array.get buffer next_cell_index
  in 
  if Cell.is_val (Atomic.get next_cell) then 
    Enqueue_result.Overloaded 
  else 
    (Atomic.set next_cell (Value element);
    Atomic.incr tail;
    Enqueue_result.Enqueued);;

let local_dequeue {quasi_head; tail; mask; buffer} : 'a Dequeue_result.t =
  (* local deque is optimistic because it can fix its mistake if needed *)
  let my_index = (Atomic.fetch_and_add quasi_head 1) + 1 in
  let tail_val = Atomic.get tail in
  if my_index = tail_val then
    (Atomic.decr quasi_head;
    Empty)
  else if my_index > tail_val then
    assert false 
  else 
    (let cell = Array.get buffer (my_index land mask) in
    let value = Cell.get_val (Atomic.get cell) in
    Atomic.set cell Cell.Empty; 
    Dequeued value);;

let local_is_half_empty {quasi_head = _; tail; mask; buffer} : bool =
  let size = Array.length buffer in 
  let tail_value = Atomic.get tail in
  let rec check_spot curr = 
    if curr < 0 then 
      true 
    else if 
      (let cell = Array.get buffer ((tail_value + curr) land mask) in
      Cell.is_val (Atomic.get cell))
    then 
      false 
    else 
      check_spot (curr-1)
  in
  check_spot (size / 2)
        
let steal_half {quasi_head; tail; mask; buffer} ~local_queue =
  if not (local_is_half_empty local_queue) then 
    ()
  else
  (let quasi_head_val = Atomic.get quasi_head in 
  let tail_val = Atomic.get tail in 
  let size = tail_val - quasi_head_val - 1 in 
  if size < 1  
  then ()
  else 
    (let stealable = 
      (* We want to steal even if there's a single element, thus +1 *)
      (size + 1)/2  
    in
    let new_quasi_head_val = quasi_head_val + stealable in
    if new_quasi_head_val >= tail_val then
      (
        Printf.printf "new_hd %d, hd %d, tl %d, sz %d l\n" new_quasi_head_val 
        quasi_head_val tail_val size;
        assert false
      );
    if Atomic.compare_and_set quasi_head quasi_head_val new_quasi_head_val 
    then  
      (for i = 1 to stealable do
        let value = Array.get buffer ((quasi_head_val + i) land mask) in
        local_enqueue local_queue (Cell.get_val2 (Atomic.get value)) |> (function
        | Enqueue_result.Enqueued -> ()
        | Overloaded -> 
          (* we've ensured that half of the queue is empty. *)
          assert false);
        Atomic.set value Empty;
      done)
    else 
      (* I'd expect we want to retry with probability log of queue occupation*)
      ()));;
        
    
