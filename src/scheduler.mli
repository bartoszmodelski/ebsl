
module Promise : sig 
  type 'a t 

end

val await : 'a Promise.t -> 'a
val schedule : (unit -> 'a) -> 'a Promise.t
val yield : unit -> unit

module FIFO : sig
  val init : f:(unit -> unit) -> int -> unit
  val pending_tasks : unit -> int
end

module LIFO : sig
  val init : f:(unit -> unit) -> int -> unit
  val pending_tasks : unit -> int
end