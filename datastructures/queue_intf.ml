module type S = sig
  type 'a t 
  
val init : ?size_exponent:int -> unit -> 'a t
val enqueue : 'a t -> 'a -> unit 
val dequeue : 'a t -> 'a option 
end 