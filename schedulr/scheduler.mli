val await : 'a Promise.t -> 'a
val schedule : (unit -> 'a) -> 'a Promise.t
val yield : unit -> unit

module type S = sig
  type t 
  val init : ?afterwards:[`join_the_pool | `return] 
    -> ?size_exponent:int 
    -> f:(unit -> unit) 
    -> int 
    -> t
  val inject_task : t -> (unit -> unit) -> unit

  val pending_tasks : unit -> int
  val scheduler_name : String.t
  module Stats : sig 
    val unsafe_print_latency_histogram : unit -> unit 
    val unsafe_print_executed_tasks : unit -> unit
  end 
end 

module type DataStructure = sig 
  type 'a t 

  val init : ?size_exponent:int -> unit -> 'a t
  
  (** [local_insert] inserts item into the structure. Return false if 
      the item could not be enqueued.   
  *)
  val local_insert : 'a t -> 'a -> bool

  (** [local_insert_after_preemption] like [local_insert] but called
      after yield. Helps LIFO-ey structures not get stuck if yielding
      fiber requires some other work done. 
  *)
  val local_insert_after_preemption : 'a t -> 'a -> bool 
  val local_remove : 'a t -> 'a option
  val global_steal : from:'a t -> to_local:'a t -> int

  (** Debugging misuse of local_ methods is tricky. Scheduler calls 
      [register_domain_id] to register the domain considered local. 
      Domain id is then rechecked in calls to local_* functions. 
  *)
  val register_domain_id : 'a t -> unit

  val indicative_size : 'a t -> int

  val name : String.t
end;;


module Make : functor (DS : DataStructure) -> S

