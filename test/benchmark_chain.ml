
let _ = Printexc.record_backtrace true


let finished = Atomic.make 0

let wait_exp () = 
  let exponential () = log (Random.float 1.) *. -1. in
  Unix.sleepf (exponential () /. 1_000_000.);;

let log s = 
  Printf.printf "%s\n" s;
  Stdlib.(flush stdout);;

let run_processor ~n () =
  for _ = 1 to n do 
    Schedulr.Scheduler.schedule (fun () -> 
      let packet = Mock_packet.get_rand () in 
      Digestif.SHA1.digest_bytes packet 
      |> Digestif.SHA1.to_hex
      |> Sys.opaque_identity
      |> ignore;
      wait_exp ();
      Atomic.incr finished) 
    |> ignore;
   (* wait_exp (); *)
  done;;
let items_total = 1_000

let workload ~num_of_spawners () =
  let items_per_worker = items_total / num_of_spawners in 
  Atomic.set finished 0;
  let time_start = Core.Time_ns.now () in 
  for _ = 1 to num_of_spawners do 
      Schedulr.Scheduler.schedule (run_processor ~n:items_per_worker)
      |> ignore
    done;
  while Atomic.get finished < num_of_spawners * items_per_worker do () done; 
  let time_end = Core.Time_ns.now () in 
  let difference = Core.Time_ns.diff time_end time_start 
    |> Core.Time_ns.Span.to_int_ns in 
  Printf.printf "time:%d\n" (difference/1000_000);
  Stdlib.flush_all ();;

let iterations = 11

let benchmark ~num_of_domains ~num_of_spawners (module Sched : Schedulr.Scheduler.S) =
  Printf.printf "start(sched:%s,spawners:%d,domains:%d)\n"
    Sched.scheduler_name
    num_of_spawners
    num_of_domains;
  Reporting.With_latency.init (num_of_domains+1);
  Sched.init (num_of_domains-1) ~f:(fun () ->
    for i = 1 to iterations do 
      Printf.printf "iteration:%d\n" i;
      Unix.sleepf 0.1;
      workload ~num_of_spawners (); 
      Unix.sleepf 0.1;
      if Sched.pending_tasks () != 0  
      then assert false; 
      Sched.Stats.unsafe_print_executed_tasks ();
    done; 
    Printf.printf "done\n"; 
    Stdlib.flush_all ();
    Stdlib.exit 0) |> ignore;;

(* cmd *)

let () =
  let usage_msg = "benchmark <flags>" in 
  let anon_fun _ = failwith "no anon parameters expected" in
  let scheduler = ref "" in
  let num_of_domains = ref 0 in 
  let num_of_spawners = ref 0 in 
  let speclist =
    [("-scheduler", Arg.Set_string scheduler, "set scheduler algo");
    ("-num-of-domains", Arg.Set_int num_of_domains, "set num of domains");
    ("-num-of-spawners", Arg.Set_int num_of_spawners, "set num of spawners")] 
  in 
  Arg.parse speclist anon_fun usage_msg;
  let scheduler_module = Flags.parse_sched scheduler in  
  assert (0 < !num_of_domains && !num_of_domains < 512);
  assert (0 < !num_of_spawners && !num_of_spawners < 512);
  let num_of_domains = !num_of_domains in
  let num_of_spawners = !num_of_spawners in 
  benchmark ~num_of_domains ~num_of_spawners scheduler_module;;


