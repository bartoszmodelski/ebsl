module Scheduler = Schedulr.Instance.FIFO

let pools = (Hashtbl.create 10 : (String.t, Scheduler.t) Hashtbl.t)

let schedule ?pool_name f =
  match pool_name with 
  | None -> 
    Schedulr.Scheduler.schedule f 
    |> ignore
  | Some pool_name -> 
    (let pool = 
      match Hashtbl.find_opt pools pool_name with
      | None -> 
        let pool = 
          Printf.printf "starting micropool %s\n" pool_name;
          Scheduler.init ~afterwards:`return ~f:(fun () -> ()) 1
        in
        Hashtbl.add pools pool_name pool;
        pool 
      | Some pool -> pool   
    in 
    Scheduler.inject_task pool f);;