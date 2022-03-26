
type 'a t = {
  queue : 'a Queue.t;
  mutex : Mutex.t;
}

let init () = 
  { queue = Queue.create ();
    mutex = Mutex.create ();
  }
;;

let with_mutex mtx f = 
  Mutex.lock mtx; 
  let v = f () in 
  Mutex.unlock mtx; 
  v;;

let enqueue {queue; mutex} item = 
  with_mutex mutex (fun () -> 
    Queue.push item queue)
let dequeue {queue; mutex} = 
  with_mutex mutex (fun () -> 
    Queue.take_opt queue)