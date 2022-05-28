
module FIFO : sig
  include Scheduler.S
end

module FIFO_with_resize : sig
  include Scheduler.S
end

module LIFO : sig
  include Scheduler.S
end

module Hybrid_random : sig 
  include Scheduler.S 
end

module Hybrid_alternating : sig 
  include Scheduler.S
end

module Hybrid_reverse_every_n : sig
  include Scheduler.S
end

module FIFO_with_slot : sig  
  include Scheduler.S 
end