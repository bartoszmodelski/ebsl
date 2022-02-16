let buffer_size = 1600


let _log s = 
  Printf.printf "%s\n" s;
  Stdlib.(flush stdout);;


let _ = Printexc.record_backtrace true

let empty_buffer () = Bytes.create buffer_size
let random_buffer () = 
  let v = empty_buffer () in 
  for i = 0 to buffer_size / 4 - 5  do 
    try Bytes.set_int64_be v (i * 4) (Random.int64 Int64.max_int) with 
    | Invalid_argument _ -> Stdlib.exit 1;
  done;
  v;;

let start_leg ~n slot () = 
  let data = ref (empty_buffer ()) in
  for _ = 0 to n  do 
    while Option.is_some (Atomic.get slot) do 
      Schedulr.Scheduler.yield () 
    done;
    Atomic.set slot (Some !data);
    data := random_buffer ();  
  done;;

let start_mixer ~n_packets ~n_legs (f : bytes -> unit) () =
  let exchange_array = 
    Array.init n_legs (fun _ -> 
      let slot = Atomic.make None in 
      Schedulr.Scheduler.schedule (start_leg ~n:n_packets slot)
      |> ignore;
      slot) 
  in 
  let output = empty_buffer () in 
  for _ = 0 to n_packets do 
    for i = 0 to n_legs - 1 do 
      let element = 
        Array.get exchange_array i
      in 
      while Option.is_none (Atomic.get element) do 
        Schedulr.Scheduler.yield ()
      done;
      let buffer = 
        match Atomic.exchange element None with 
        | None -> assert false 
        | Some v -> v 
      in 
      for j = 0 to buffer_size /2 - 3 do 
        let v_1 = Bytes.get_int32_be buffer (j*2) in 
        let v_2 = Bytes.get_int32_be output (j*2) in 
        Bytes.set_int32_be output (j*2) (Int32.add v_1 v_2)
      done;
    done;
    let output = Sys.opaque_identity output in 
    (Schedulr.Scheduler.schedule (fun () -> f output) |> ignore);
  done;;

let log s = 
  Printf.printf "%s\n" s;
  Stdlib.(flush stdout);;
  
let total_executions = 5

module Scheduler = Schedulr.Scheduler.LIFO
let benchmark () = 
  Scheduler.init 3 ~f:(fun () ->  
    let promises = 
      Array.init total_executions 
        (fun _ -> 
          Schedulr.Scheduler.schedule  
          (start_mixer 
            ~n_packets:10000
            ~n_legs:3
            (fun v -> Sys.opaque_identity v |> ignore))) 
    in 
    Array.iter Schedulr.Scheduler.await promises; 
    while Scheduler.pending_tasks () != 0 do 
      Unix.sleepf 0.1;
    done;
    Stdlib.exit 0);;

benchmark ()