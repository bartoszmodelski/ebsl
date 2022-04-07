module Atomic = Dscheck.TracedAtomic

type 'a t = private {
  array : 'a Option.t Array.t;
  tail : int Atomic.t;
  head : int Atomic.t;
  mask : int;
  enqueuer : Domain.id option ref;
  dequeuer : Domain.id option ref;
}

val init : ?size_exponent:int -> unit -> 'a t
val enqueue : 'a t -> 'a -> bool 
val dequeue : 'a t -> 'a option