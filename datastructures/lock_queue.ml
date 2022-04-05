
type 'a t = {
  queue : 'a Queue.t;
  mutex : Mutex.t;
  max_size : int;
}

let init ?(size_exponent=31) () = 
  let max_size = 1 lsl size_exponent in
  { queue = Queue.create ();
    mutex = Mutex.create ();
    max_size;
  }
;;

let with_mutex mtx f = 
  Mutex.lock mtx; 
  let v = f () in 
  Mutex.unlock mtx; 
  v;;

let rec enqueue t item = 
  let ({queue; mutex; max_size} : 'a t) = t in 
  let enqueued = 
    with_mutex mutex (fun () -> 
      if Queue.length queue < max_size 
      then 
        (Queue.push item queue;
        true) 
      else 
        false) 
  in
  if enqueued 
  then () 
  else 
    (Domain.cpu_relax (); 
    enqueue t item)

let dequeue {queue; mutex; _} = 
  with_mutex mutex (fun () -> 
    Queue.take_opt queue)