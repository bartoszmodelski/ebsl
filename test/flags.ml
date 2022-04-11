let parse_sched v = 
  match !v with 
  | "FIFO" -> (module Schedulr.Instance.FIFO : Schedulr.Scheduler.S)
  | "LIFO" -> (module Schedulr.Instance.LIFO)
  | "hybrid_random" -> (module Schedulr.Instance.Hybrid_random)
  | "hybrid_alternating" -> (module Schedulr.Instance.Hybrid_alternating)
  | "hybrid_reverse_every_n" -> (module Schedulr.Instance.Hybrid_reverse_every_n)
  | "FIFO_resize" -> (module Schedulr.Instance.FIFO_with_resize)
  | s -> failwith ("unknown scheduler type " ^ s)


let anon_fun _ = failwith "no anon parameters expected";;
