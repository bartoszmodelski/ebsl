
module Promise : sig 
  type 'a t 

end

val await : 'a Promise.t -> 'a
val schedule : (unit -> 'a) -> 'a Promise.t
val yield : unit -> unit

module type S = sig
  val init : f:(unit -> unit) -> int -> unit
  val pending_tasks : unit -> int
  val scheduler_footprint : String.t
  module Stats : sig 
    val unsafe_print_latency_histogram : unit -> unit 
    val unsafe_print_executed_tasks : unit -> unit
  end 
end 

module FIFO : sig
  include S
end

module LIFO : sig
  include S
end