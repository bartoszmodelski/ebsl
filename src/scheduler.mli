
module Promise : sig 
  type 'a t 

end

val await : 'a Promise.t -> 'a
val schedule : (unit -> 'a) -> 'a Promise.t
val yield : unit -> unit

module FIFO : sig
  val await_completion : unit -> unit
  val init : f:(unit -> unit) -> int -> unit
end

module LIFO : sig
  val await_completion : unit -> unit
  val init : f:(unit -> unit) -> int -> unit
end