let buffers =
  let buffers = 
    List.init 10 (fun _ -> 
      Buffer.create 0)
  in 
  List.iteri (fun buff_i buffer -> 
    List.init (200 + buff_i * 30) (fun i -> 
      let c = 
        if i mod (20 + buff_i) = 0 && 0 < i && i < 300 
        then '\n' else '-'
      in
      Buffer.add_char buffer c) |> ignore) 
      buffers;
  buffers;;
let rec find_spaces buffer index bound already_found =
  let index = index + 1 in 
  if index >= bound 
  then 
    already_found 
  else 
    let current = 
      if Buffer.nth buffer index = '\n'        
      then ([index]) else []
    in 
    find_spaces buffer index bound (current @ already_found);;

    
let smoke_test = 
  assert (
    List.length (find_spaces (List.nth buffers 1) 0 230 []) = 10);
  assert (
    List.length (find_spaces (List.nth buffers 5) 0 230 []) = 9);;
    

let _log s = 
  Printf.printf "%s\n" s;
  Stdlib.(flush stdout);;
let finished = Atomic.make 0
let rec run_processor ~copy_out ~n () = 
  if n < 0 
  then 
    (Atomic.incr finished)
  else ( 
    let len = List.length buffers in
    let index = n mod len in 
    let buffer = 
      let source = List.nth buffers index in 
      if copy_out 
      then (
        let b = (Buffer.create 0) in 
        Buffer.add_buffer b source;
        b)
      else 
        source 
    in
    let len = Buffer.length buffer in 
    let f from to_ = 
      find_spaces buffer from to_ [] 
      |> Sys.opaque_identity 
      |> ignore
    in  
    f 0 (len/3);
    f (len/3) (2*len/3);
    f (2*len/3) (len - 1);
    Schedulr.Scheduler.schedule (run_processor ~copy_out ~n:(n-1)) 
    |> ignore);;

let total_executions = 4

module Sched = Schedulr.Scheduler.FIFO
let benchmark () = 
  Sched.init 3 ~f:(fun () ->
    for _j = 1 to 3 do  
      for _i = 1 to 10 do 
        Unix.sleepf 0.2;
        Atomic.set finished 0;
        let time_start = Core.Time_ns.now () in 
        let _ = 
          for _ = 1 to total_executions do 
            Schedulr.Scheduler.schedule (run_processor ~copy_out:false ~n:100_000)
            |> ignore
          done;
          while Atomic.get finished < total_executions  do 
            Schedulr.Scheduler.yield ()
          done; 
        in
        let time_end = Core.Time_ns.now () in 
        let difference = Core.Time_ns.diff time_end time_start 
          |> Core.Time_ns.Span.to_string in 
        (* if j > 1 then *) 
        Printf.printf "%s\n" difference;
        Stdlib.flush_all ();
        Unix.sleepf 0.2;
        while Sched.pending_tasks () != 0 do
          _log (Int.to_string (Sched.pending_tasks ()));
        Schedulr.Scheduler.yield ();
        done;
      done; 
    done;
    Stdlib.exit 0);;

benchmark ()