module Scheduler = Schedulr.Instance.LIFO

let pools = Atomic.make (Hashtbl.create 10 : (String.t, Scheduler.t) Hashtbl.t)

let creator_mtx = Mutex.create () 

let schedule ?(pool_size=1) ?pool_name f =
  match pool_name with 
  | None -> 
    Schedulr.Scheduler.schedule f 
    |> ignore
  | Some pool_name -> 
    (let pool = 
      match Hashtbl.find_opt (Atomic.get pools) pool_name with
      | None -> 
        Mutex.lock creator_mtx; 
        let copied_pools = Hashtbl.copy (Atomic.get pools) in 
        let pool = 
          (* Printf.printf "starting micropool %s (sz:%d)\n" pool_name pool_size;*)
          Stdlib.(flush stdout);
          Scheduler.init ~afterwards:`return ~f:(fun () -> ()) pool_size
        in
        Hashtbl.add copied_pools pool_name pool;
        Atomic.set pools copied_pools;
        Mutex.unlock creator_mtx; 
        pool 
      | Some pool -> pool   
    in 
    Scheduler.inject_task pool f);;