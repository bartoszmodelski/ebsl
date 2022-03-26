type 'a t 

val init : unit -> 'a t
val enqueue : 'a t -> 'a -> unit 
val dequeue : 'a t -> 'a option 