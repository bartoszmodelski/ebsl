
module Make : functor (M : sig val num_of_queues : int end) -> Queue_intf.S