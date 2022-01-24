(* 
    MPMC quasi-queue https://dl.acm.org/doi/pdf/10.1145/3437801.3441583

    Enqueue blocks if the queue is full. Dequeue blocks if the queue 
    is empty. In both cases the call returns only once the action can be
    finished. That is another thread made space in the queue or inserted 
    an element. 

    Is there a way to stop blocking? 
    Blocking is traded for handling main points of contention with FAD
    rather than CAS. I *think* that enquing could be fixed by leaving 
    some slots empty in the queue, and simply reporting full if FAD is 
    going to breach it (number of slots should be bigger than number of
    running threads). For the dequeue, dequeuer could give up and leave a 
    hint in the slot for the enqueuer. 
*)

module Atomic = Dscheck.TracedAtomic

type 'a t = {
  array: 'a Atomic.t Array.t;
  head: int Atomic.t;
  tail: int Atomic.t;
  mask: int
}
(*  
  head points one ahead of the first element in the queue 
  tail points one ahead of the last element in the
*)

let empty_cell = Obj.magic 0

let init ?(size_exponent=8) () : 'a t =
  let size = 2 lsl size_exponent in
  let array = Array.init size (fun _ -> Atomic.make (empty_cell)) in 
  let mask = size - 1 in
  let head = Atomic.make 0 in 
  let tail = Atomic.make 0 in  
  { array; head; tail; mask };;

let enqueue {array; tail; mask; _} element = 
  let index = (Atomic.fetch_and_add tail 1) land mask in
  Stdlib.flush_all ();
  let cell = Array.get array index in 
  while not (Atomic.compare_and_set cell empty_cell element) && false do
    Domain.cpu_relax ()     
  done;;

let dequeue queue =
  let ({array; head; mask; _} : 'a t) = queue in 
  let index = (Atomic.fetch_and_add head 1) land mask in
  let cell = Array.get array index in 
  let element = ref (Atomic.exchange cell empty_cell) in
  while empty_cell == !element do
    (* iterate on just load, which should be cheaper *)
    while empty_cell == Atomic.get cell do 
      Domain.cpu_relax ();
    done;
    element := (Atomic.exchange cell empty_cell);
  done;
  !element
  
let log ~thr s = 
  Printf.printf "%s: %s\n" thr s;
  Stdlib.flush Stdlib.stdout;;

let _test_1 () =
  let queue = init ~size_exponent:1 () in
  let a = 
    let log = log ~thr:"A" in 
    Domain.spawn (fun () ->
      enqueue queue "a";
      enqueue queue "b";
      enqueue queue "c";
      dequeue queue |> log;
      log "done a") 
  in 
  let b = 
    let log = log ~thr:"B" in
    Domain.spawn (fun () ->
      dequeue queue |> log;
      enqueue queue "d";
      dequeue queue |> log;
      dequeue queue |> log;
      log "done b")
  in 
  Domain.join b |> ignore; 
  Domain.join a |> ignore;
  ();;

let total_checked = ref 0

let create_test upto () =
  let queue = init ~size_exponent:1 () in
  for _ = 1 to upto do
    Atomic.spawn (fun () -> 
      enqueue queue "");
  done;
  for _ = 1 to upto do
    Atomic.spawn (fun () -> 
      Sys.opaque_identity(dequeue queue) |> ignore);
  done;
  Atomic.final (fun () ->
    total_checked := !total_checked + 1;
    let ({array; head; tail; _} : string t) = queue in
    let head_value = Atomic.get head in
    let tail_value = Atomic.get tail in
    Atomic.check (fun () -> 
      Array.for_all (fun v -> Atomic.get v = empty_cell) array);
    Atomic.check (fun () -> 
      head_value = upto);
    Atomic.check (fun () -> 
      tail_value = upto));;
  
let () =
  Atomic.trace ~depth_limit:30 (create_test 2);
  Printf.printf "Total checked: %d\n" (!total_checked);;