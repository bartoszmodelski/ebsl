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
val steal : ?auto_retry:bool 
  -> ?steal_size_limit:int 
  -> from:'a t 
  -> to_local:'a t 
  -> unit 
  -> int

val register_domain_id : 'a t -> unit
val local_is_empty : 'a t -> bool
val local_replace_with_a_random_item : 'a t -> 'a -> 'a option

val indicative_size : 'a t -> int