let parse_sched v = 
  match !v with 
  | "FIFO" -> (module Schedulr.Scheduler.FIFO : Schedulr.Scheduler.S)
  | "LIFO" -> (module Schedulr.Scheduler.LIFO)
  | "hybrid_random" -> (module Schedulr.Scheduler.Hybrid_random)
  | "hybrid_alternating" -> (module Schedulr.Scheduler.Hybrid_alternating)
  | "hybrid_reverse_every_n" -> (module Schedulr.Scheduler.Hybrid_reverse_every_n)
  | s -> failwith ("unknown scheduler type " ^ s)

