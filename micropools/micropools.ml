module Scheduler = Schedulr.Instance.FIFO

let pools = Atomic.make (Hashtbl.create 10 : (String.t, Scheduler.t) Hashtbl.t)

let creator_mtx = Mutex.create () 

let schedule ?pool_name f =
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
          Printf.printf "starting micropool %s\n" pool_name;
          Scheduler.init ~afterwards:`return ~f:(fun () -> ()) 1
        in
        Hashtbl.add copied_pools pool_name pool;
        Atomic.set pools copied_pools;
        Mutex.unlock creator_mtx; 
        pool 
      | Some pool -> pool   
    in 
    Scheduler.inject_task pool f);;