(* MPMC quasi-queue https://dl.acm.org/doi/pdf/10.1145/3437801.3441583
*)

type 'a t = {
  queue: 'a Atomic.t Array.t;
  head: int Atomic.t;
  tail: int Atomic.t;
  mask: int
}

let empty_cell = Obj.magic 0
let init ?(size_exponent=8) () : 'a t =
  let size = 2 lsl size_exponent in
  let queue = Array.init size (fun _ -> Atomic.make (empty_cell)) in 
  let mask = size - 1 in
  let head = Atomic.make 0 in 
  let tail = Atomic.make 0 in  
  { queue; head; tail; mask }

let enqueue {queue; tail; mask; _} element = 
  let index = (Atomic.fetch_and_add tail 1) land mask in
  Stdlib.flush_all ();
  let cell = Array.get queue index in 
  while not (Atomic.compare_and_set cell empty_cell element) do
    Domain.cpu_relax ()     
  done

let dequeue {queue; head; mask; _} = 
  let index = (Atomic.fetch_and_add head 1) land mask in
  let cell = Array.get queue index in 
  let element = ref (Atomic.exchange cell empty_cell) in 
  while empty_cell == !element do
    Domain.cpu_relax ();
    element := (Atomic.exchange cell empty_cell);
  done;
  !element

let log ~thr s = 
  Printf.printf "%s: %s\n" thr s;
  Stdlib.flush Stdlib.stdout;;

let test_1 () =
  let queue = init ~size_exponent:3 () in
  
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
      enqueue queue "d";
      dequeue queue |> log;
      dequeue queue |> log;
      dequeue queue |> log;
      log "done b") 
  in 
  Domain.join b |> ignore; 
  Domain.join a |> ignore;
  ();;

test_1 ()