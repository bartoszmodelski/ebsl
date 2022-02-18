
let _ = Printexc.record_backtrace true

let log s = 
  Printf.printf "%s\n" s;
  Stdlib.(flush stdout);;

let finished = Atomic.make 0

let run_processor ~copy_out ~n () =
  for i = 1 to n do 
    Schedulr.Scheduler.schedule (fun () ->
      let packet = Mock_packet.get_by_index n ~copy_out in 
      let len = Buffer.length packet in 
      let f from to_ = 
        Mock_packet.find_spaces packet from to_ [] 
        |> Sys.opaque_identity 
        |> ignore
      in  
        f 0 len; 
        Atomic.incr finished) |> ignore;
    if i mod 1000 == 0 
    then Schedulr.Scheduler.yield ()
  done;;

let total_workers = 1
let items_per_worker = 100_000

let workload () =
  Atomic.set finished 0;
  let time_start = Core.Time_ns.now () in 
  let _ = 
    for _ = 1 to total_workers do 
      Schedulr.Scheduler.schedule (run_processor ~copy_out:true ~n:items_per_worker)
      |> ignore
    done;
    while Atomic.get finished < total_workers*items_per_worker do 
      Schedulr.Scheduler.yield ()
    done; 
  in
  let time_end = Core.Time_ns.now () in 
  let difference = Core.Time_ns.diff time_end time_start 
    |> Core.Time_ns.Span.to_int_ns in 
  Printf.printf "time:%d\n" (difference/1000_000);
  Stdlib.flush_all ();;


module Sched = Schedulr.Scheduler.LIFO

let num_of_domains = 6
let iterations = 3
let size_param = 10

let benchmark () = 
  Sched.init num_of_domains ~f:(fun () ->
    for _j = 1 to iterations do  
      for _i = 1 to size_param do 
        Unix.sleepf 0.1;
        workload (); 
        Unix.sleepf 1.;
        if Sched.pending_tasks () != 0  
        then assert false; 
        Sched.Stats.unsafe_print_latency_histogram (); 
        Sched.Stats.unsafe_print_executed_tasks ();
      done; 
    done;
    Stdlib.exit 0);;

benchmark ()

