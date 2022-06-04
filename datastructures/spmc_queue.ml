module Atomic = Dscheck.TracedAtomic

let value_exn = function 
  | None -> assert false 
  | Some v -> v 

(* Notes: 
  - Local deque does not have to synchronize with the writer, only other readers.
  - Tail marks the "logical tail", physical one is the cell with oldest Value.
  - Local dequeue and enqueue are wait-free. Normal dequeue (steal) may spin, but 
  it only has to do it once for any number of elements. 
  - Some accesses do not have to be atomic but that's the only way current lib
  allows 'a Atomic.t to be accessed. Probably doesn't matter on x86. 
  *)

type 'a t = {
  head : int Atomic.t; 
  tail : int Atomic.t;
  mask : int Atomic.t;
  array : 'a option Atomic.t Array.t Atomic.t;
  owned_by_id: Domain.id option ref;
  slot: 'a option ref;
} 

let get_slot {slot; _ } = !slot;;
let set_slot {slot; _} new_item = 
  slot := new_item;;

let init ?(size_exponent=7) () =
  let size_exponent = 
    match Sys.getenv_opt "QUEUE_SIZE" with 
    | None -> size_exponent 
    | Some v -> int_of_string v
  in
  let size = Int.shift_left 1 size_exponent in
  { head = Atomic.make 0;
    tail = Atomic.make 0;
    mask = Atomic.make (size - 1);
    array = Atomic.make (Array.init size (fun _ -> Atomic.make None));
    owned_by_id = ref None;
    slot = ref None}

(* Cautionary check because debugging broken 'local' invariant 
  is hard. *)
let register_domain_id {owned_by_id; _} =
  owned_by_id := Some (Domain.self ());;
   
let assert_domain_id scenario owned_by_id = 
  let this_thr = Domain.self () in 
  match !owned_by_id with 
  | None -> ()
  | Some _id -> 
    if _id != this_thr then (
    Printf.printf "%s: local method accessed by a foreign thread!\n" scenario; 
    Stdlib.flush_all ();
    assert false)
    
(* [local_resize] resizes the array. 

  1. Create new array.
  2. Overshoot the head to discourage any new stealers.
  3. Transfer covered elements to the new array. 
  4. Ensure all other stealers finished (i.e. no elements in the old array).
  5. Swap arrays.
  6. Update mask, set tail and head to 0. In this order to discourage stealers. 
*)
let local_resize t = 
  let {mask; array; tail; head; _} = t in 
  let (mask_val,array_val) = Atomic.(get mask, get array) in 
  let size = (Atomic.get mask) + 1 in 
  let new_array_val = 
    Array.init (size * 2) (fun _ -> Atomic.make None)
  in 
  let old_head_val = Atomic.fetch_and_add head size in 
  let temp_head_val = old_head_val + size in 
  let tail_val = Atomic.get tail in 
  let num_of_items_to_copy = tail_val - old_head_val in 
  for i = 0 to num_of_items_to_copy - 1 do 
    let cell = Array.get array_val ((old_head_val + i) land mask_val) in 
    assert (Option.is_some (Atomic.get cell));
    Array.set new_array_val i cell;
  done;
  let num_of_items_to_ensure_taken = temp_head_val - tail_val - 1 in 
  for i = 0 to num_of_items_to_ensure_taken do
    let index = (tail_val + i) land mask_val in 
    while Option.is_some (Atomic.get (Array.get array_val index)) do () done;
  done;
  Atomic.set array new_array_val; 
  Atomic.set mask (Array.length new_array_val - 1);
  Atomic.set tail num_of_items_to_copy; 
  Atomic.set head 0;;


let local_enqueue {tail; head; mask; array; owned_by_id = _; _} element =
  (* assert_domain_id "enq" owned_by_id; *)
  let (mask,array) = Atomic.(get mask, get array) in 
  let tail_val = Atomic.get tail in 
  let head_val = Atomic.get head in 
  let index = tail_val land mask in 
  let cell = Array.get array index in 
  if tail_val == head_val + mask + 1
  then false
  else 
    (while Option.is_some (Atomic.get cell) do () done;
    Atomic.set cell (Some element);
    Atomic.set tail (tail_val + 1);
    true);;



let rec local_enqueue_with_resize t element =
  let {tail; mask; array; owned_by_id; _} = t in 
  assert_domain_id "enq" owned_by_id;
  let (mask,array) = Atomic.(get mask, get array) in 
  let tail_val = Atomic.get tail in 
  let cell = Array.get array (tail_val land mask) in 
  match Atomic.get cell with
  | None -> 
    Atomic.set cell (Some element);
    (* tail might have been changed by resize *)
    Atomic.set tail (Atomic.get tail + 1)
  | Some _ ->  
    let i = ref 0 in 
    (* I suppose we should be getting increasingly hesitant to increase 
    as the buffer is already big. *)
    while Option.is_some (Atomic.get cell) && !i < 30 do i := !i + 1 done;
    if Option.is_some (Atomic.get cell) 
    then local_resize t;
    local_enqueue_with_resize t element;;

let local_dequeue {head; tail; mask; array; owned_by_id = _; _} : 'a option =
  (* assert_domain_id "deq" owned_by_id; *)
  let (mask,array) = Atomic.(get mask, get array) in 
  (* local deque is optimistic because it can fix its mistake if needed *)
  let index = Atomic.fetch_and_add head 1 in
  let tail_val = Atomic.get tail in
  if index = tail_val 
  then
    (* nobody else would speculate *)
    ( (* assert (Atomic.compare_and_set head (index + 1) index); *) 
    Atomic.set head index;
    None)
  else if index - tail_val > 0 
  then 
    assert false
  else 
    (let cell = Array.get array (index land mask) in
    let element = Atomic.get cell in
    Atomic.set cell None; 
    assert (Option.is_some element); 
    element);;
    
    
(* successfuly rolled back *)
let local_is_empty_thorough {head = _; tail; mask; array; _} : bool =
  let (mask,array) = Atomic.(get mask, get array) in 
  let size = Array.length array in 
  let tail_value = Atomic.get tail in
  let seen_not_free = ref false in 
  let i = ref (size - 1) in 
  while not !seen_not_free && !i >= 0 do 
    let cell = Array.get array ((tail_value + !i) land mask) in 
    seen_not_free := Atomic.get cell != None; 
    i := !i - 1
  done; 
  not !seen_not_free


let local_is_empty queue =
  let {head; tail; _} = queue in
  let tail_val = Atomic.get tail in 
  let head_val = Atomic.get head in 
  if tail_val > head_val 
  then false
  else local_is_empty_thorough queue
  
let steal ~from ~to_local =
  let {owned_by_id = _; mask = target_mask; _} = to_local in 
  (* assert_domain_id "stl" owned_by_id; *)
  let target_size = Atomic.get target_mask + 1 in
  let ({head; tail; mask; array; _} : 'a t) = from in
  let (mask,array) = Atomic.(get mask, get array) in 
  (* assumes there's space in the queue *)
  let tail_val = Atomic.get tail in 
  let head_val = Atomic.get head in 
  let size = tail_val - head_val in 
  if size < 1  
  then 
    0
  else 
    (let stealable = 
      (* We want to steal even if there's a single element, thus +1 *)
      min ((size + 1)/2) target_size  
    in
    let new_head_val = head_val + stealable in
    assert (new_head_val <= tail_val);
    if not (Atomic.compare_and_set head head_val new_head_val)
    then
      0
    else 
      (for i = 0 to stealable - 1 do
        let cell = Array.get array ((head_val + i) land mask) in
        let value = ref (Atomic.get cell) in 
        if i == stealable - 1 
        then   
          (while Option.is_none !value do 
            value := Atomic.get cell
          done);    
        Atomic.set cell None;
        while not (local_enqueue to_local (value_exn !value)) do () done;
      done;
      stealable));; 
  

let indicative_size {head; tail; _} =
  max (Atomic.get tail - Atomic.get head) 0
      