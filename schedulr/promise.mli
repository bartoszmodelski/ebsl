type 'a t = private {
  returned : 'a option ref;
  awaiting : ('a -> unit) List.t option ref;
  mutex : Mutex.t;
}

val empty : unit -> 'a t

val await : 'a t -> ('a -> unit) -> [> `Already_done of 'a | `Task ]
val fill : 'a t -> 'a -> ('a -> unit) list
