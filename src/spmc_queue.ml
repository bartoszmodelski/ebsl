module Atomic = Dscheck.TracedAtomic

let value_exn = function 
  | None -> assert false 
  | Some v -> v 


let value_exn_2 = function 
| None -> assert false 
| Some v -> v 


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
  - Some accesses do not have to be atomic but that's the only way current lib
  allows 'a Atomic.t to be accessed. Probably doesn't matter on x86. 
  *)

type 'a t = {
  head : int Atomic.t; 
  tail : int Atomic.t;
  mask : int;
  array : 'a option Atomic.t Array.t;
  owned_by_id: Domain.id option ref;
  mutex : Mutex.t;
  id: int;
} 

let init ?(size_pow=22) ~id () =
  let size = Int.shift_left 1 size_pow in
  { head = Atomic.make 0;
    tail = Atomic.make 0;
    mask = size - 1;
    array = Array.init size (fun _ -> Atomic.make None);
    owned_by_id = ref None;
    mutex = Mutex.create ();
    id}

let register_domain_id {owned_by_id; _} =
  owned_by_id := Some (Domain.self ());;
   
let assert_domain_id scenario q_id owned_by_id = 
  let this_thr = Domain.self () in 
  match !owned_by_id with 
  | None -> 
    assert false
  | Some _id -> 
    if _id != this_thr then (
    Printf.printf "%s: mismatched ids (queue %d)! owned by %d and accessed by %d\n" 
      scenario q_id (Obj.magic _id) (Obj.magic this_thr); 
    Stdlib.flush_all ();
    assert false)
    
let with_mutex mtx f = 
  Mutex.lock mtx; 
  let v = f () in 
  Mutex.unlock mtx; 
  v

let local_enqueue {tail; mask; array; owned_by_id; mutex; id; _} element =
with_mutex mutex (fun () ->
  assert_domain_id "enq" id owned_by_id;
  let index = (Atomic.get tail) land mask in 
  let cell = Array.get array index in 
  if Option.is_some (Atomic.get cell)
  then false
  else 
    (Atomic.set cell (Some element);
    Atomic.incr tail;
    true));;

let local_dequeue {head; tail; mask; array; owned_by_id; mutex; id} : 'a option =
  with_mutex  mutex (fun () ->
  assert_domain_id "deq" id owned_by_id;
  (* local deque is optimistic because it can fix its mistake if needed *)
  let index = Atomic.fetch_and_add head 1 in
  let take_val () = 
    let cell = Array.get array (index land mask) in
    let element = Atomic.get cell in
    Atomic.set cell None; 
    Some (value_exn element)
  in
  let tail_val = Atomic.get tail in
  if index = tail_val then
    (if Atomic.compare_and_set head (index + 1) index then 
      (* successfuly rolled back *)
      None 
    else
      (* failed to rollback, since no one else can speculate, 
        there must be a value to take *)  
      take_val ())
  else if index > tail_val then
    assert false 
  else 
    take_val ());;

    
let local_is_empty_thorough {head = _; tail; mask; array; _} : bool =
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
  let {id; owned_by_id; _} = to_local in 
  assert_domain_id "stl" id owned_by_id;
  let ({head; tail; mask; array; _} : 'a t) = from in
  (* assumes there's space in the queue *)
  (let tail_val = Atomic.get tail in 
  let head_val = Atomic.get head in 
  let size = tail_val - head_val in 
  if size < 1  
  then 0 
  else 
    (let stealable = 
      (* We want to steal even if there's a single element, thus +1 *)
      (size + 1)/2  
    in
    let new_head_val = head_val + stealable in
    assert (new_head_val <= tail_val);
    if not (Atomic.compare_and_set head head_val new_head_val)
    then
      0
    else 
      (for i = 0 to stealable - 1 do
        let cell = Array.get array ((head_val + i) land mask) in
        assert (local_enqueue to_local (value_exn_2 (Atomic.get cell)));
        Atomic.set cell None;
      done;
      stealable)));; 
  
