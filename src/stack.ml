(* 
  Stepping stack 
*)
module Atomic = Dscheck.TracedAtomic


type 'a t = {
  top : int Atomic.t;
  bottom: int Atomic.t;
  array: 'a option Atomic.t Array.t;
  mask: int 
}
(* 
  top guarantees that there is elements under it (when the queue is non-empty) 
  bottom does not guarantee there is empty space under it 
*)

let init ?(size_exponent=2) () : 'a t =
  let size = 1 lsl size_exponent in
  let array = Array.init size (fun _ -> Atomic.make None) in 
  let mask = size - 1 in
  let top = Atomic.make 0 in 
  let bottom = Atomic.make 0 in  
  { top; bottom; array; mask };;

(* todo: don't incr/decr top atomically *)
let local_push {top; array; mask; _} element =
  let top_val = Atomic.get top in 
  let cell = Array.get array (top_val land mask) in
  if Option.is_some (Atomic.get cell) then
    false 
  else 
    (Atomic.set cell (Some element);
    Atomic.incr top;
    true);;

let local_pop {top; bottom; array; mask} =
  Atomic.decr top; 
  let top_val = Atomic.get top in 
  let bottom_val = Atomic.get bottom in
  (* -1 because because top has been decremented *)
  if top_val - bottom_val <= -1 then 
    (* queue was empty, fix it before returning *)
    (Atomic.incr top;
    None)
  else 
    (let cell = Array.get array (top_val land mask) in 
    let value = Atomic.get cell in 
    if Option.is_none value then 
      (* other thread stole it *)
      (Atomic.incr top;
      None)
    else (
      if not (Atomic.compare_and_set cell value None) then 
        (* other thread stole it *)
        (Atomic.incr top;
        None)
      else 
        value));;

let steal ~from:{top; bottom; array; mask} ~to_local =
  let bottom_val = Atomic.get bottom in
  let top_val = Atomic.get top in 
  let available_steal = (top_val - bottom_val + 1) / 2 in 
  if available_steal <= 0 then 
    0 
  else 
    (let stolen = 
      Atomic.compare_and_set bottom bottom_val (bottom_val + available_steal) 
    in 
    if not stolen then
      (* we could retry, but this inevitably means that someone else 
        succeded, so probably shouldn't without knowing more *)
      0 
    else (
      let old_bottom_val = bottom_val in
      let _bottom_val = bottom_val + available_steal in
      let finished = ref false in 
      let stolen = ref 0 in
      while !stolen < available_steal && not !finished do 
        let index = (old_bottom_val + !stolen) land mask in
        let cell = Array.get array index in 
        let value = Atomic.get cell in 
        if Option.is_some value then 
          (if Atomic.compare_and_set cell value None then 
            (let value = match value with 
              | None -> assert false 
              | Some v -> v 
            in 
            (while not (local_push to_local value) do () done;
            stolen := !stolen + 1)))
        else 
          (* pop took our element, there's two ways out: 
            1. Decrement the overshot bottom with CAS. 
            2. Keep polling the cell until something appears.
            
            We need to try both. Doing just 2 blocks. Doing just 1 
            may never finish if in the meantime more elements were 
            pushed and another steal is in progress. 
          *)
          (let rec recover_from_overshooting () = 
            if Atomic.compare_and_set bottom bottom_val (old_bottom_val + !stolen) then 
              (* corrected the bottom index *)
              finished := true
            else if Option.is_some (Atomic.get cell) then 
              (* something appeared, so just try to take it *)
              () 
            else
              recover_from_overshooting ()
          in 
          recover_from_overshooting ()) 
      done;
      !stolen));;



