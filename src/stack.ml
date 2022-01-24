(* 
  Stepping stack 
*)

module Atomic = Dscheck.TracedAtomic

let empty_cell = Obj.magic 0

type 'a t = {
  top : int Atomic.t;
  bottom: int Atomic.t;
  array: 'a Array.t;
  mask: int 
}

let init ?(size_exponent=8) () : 'a t =
  let size = 1 lsl size_exponent in
  let array = Array.init size (fun _ -> Atomic.make (empty_cell)) in 
  let mask = size - 1 in
  let top = Atomic.make 0 in 
  let bottom = Atomic.make 0 in  
  { top; bottom; array; mask };;

let local_enqueue {top; bottom; array; mask} element =
  let size = Array.length array in 
  let top_val = Atomic.get top in 
  let bottom_val = Atomic.get bottom in 
  if bottom_val + size >= top_val + 1 then
    (* TODO: confirm that +1 is redundant. *)
    false (* no space *)
  else (
    let cell = Array.get array (top_val land mask) in 
    Atomic.set cell element; 
    Atomic.incr top;
    true)


let local_dequeue {top; bottom; array; mask} =
  let size = Array.length array in 
  let top_val = Atomic.get top in 
  let bottom_val = Atomic.get bottom in 
  if bottom_val + size >= top_val + 1 then
    (* TODO: confirm that +1 is redundant. *)
    false (* no space *)
  else (
    let cell = Array.get array (top_val land mask) in 
    Atomic.set cell element; 
    Atomic.incr top;
    true)