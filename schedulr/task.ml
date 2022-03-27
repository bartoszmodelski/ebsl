open EffectHandlers.Deep 

type sched = 
  | New of (unit -> unit)
  | Preempted of (unit, unit) continuation

type t = 
  {s : sched; 
  execd : int Atomic.t}

let new_task c = 
  {s = New c; execd = Atomic.make 0}

let preem c = 
  {s = Preempted c; execd = Atomic.make 0}

let _ = preem

let get {s; execd} = 
  Atomic.incr execd; 
  s;;
