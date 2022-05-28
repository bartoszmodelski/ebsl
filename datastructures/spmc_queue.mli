module Atomic = Dscheck.TracedAtomic

type 'a t = private {
  head : int Atomic.t; 
  tail : int Atomic.t;
  mask : int Atomic.t;
  array : 'a option Atomic.t Array.t Atomic.t;
  owned_by_id: Domain.id option ref;
  slot: 'a option ref;
} 

val init : ?size_exponent:int -> unit -> 'a t
val local_enqueue : 'a t -> 'a -> bool
val local_dequeue : 'a t -> 'a option
val steal : from:'a t -> to_local:'a t -> int
val local_resize : 'a t -> unit
val local_enqueue_with_resize : 'a t -> 'a -> unit
val set_slot : 'a t -> 'a option -> unit 
val get_slot : 'a t -> 'a option

val register_domain_id : 'a t -> unit


(** Scheduler calls [local_is_empty] before attempting stealing to ensure 
    there's room for new elements in the target queue. *)
val local_is_empty : 'a t -> bool
val indicative_size : 'a t -> int
