
module Promise : sig 
    type 'a t 
end

val init : f:(unit -> unit) -> int -> unit
val schedule : (unit -> 'a) -> 'a Promise.t
val yield : unit -> unit
val await : 'a Promise.t -> 'a