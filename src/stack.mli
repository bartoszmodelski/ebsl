module Atomic = Dscheck.TracedAtomic


type 'a t = {
    top : int Atomic.t;
    bottom: int Atomic.t;
    array: 'a option Atomic.t Array.t;
    mask: int 
  } 

val init : ?size_exponent:int -> unit -> 'a t
val local_push : 'a t -> 'a -> bool
val local_pop : 'a t -> 'a option
val steal : from:'a t -> to_local:'a t -> int
val steal_and_throw_away : from:'a t -> int