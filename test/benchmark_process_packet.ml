
let _ = Printexc.record_backtrace true

let log s = 
  Printf.printf "%s\n" s;
  Stdlib.(flush stdout);;

let finished = Atomic.make 0

let do_heavy_work _ = 
  Unix.sleepf 0.02
;;

let do_work packet = 
  for _ = 0 to 20 do
    let pos = Random.int (Buffer.length packet) in
    Buffer.nth packet pos
    |> Sys.opaque_identity
    |> ignore 
  done
;;

let finish start_time = 
  let difference = 
    let difference_ns = 
    let end_time = Core.Time_ns.now () in   
    Core.Time_ns.diff end_time start_time 
    |> Core.Time_ns.Span.to_int_ns 
    in
    difference_ns / 1000
  in 
  Reporting.Success.local_incr ();
  Reporting.Histogram.(add_val_log 
    (Per_thread.local_get_hist ()) (difference))

let run_processor ~copy_out ~n () =
  for _i = 1 to n do 
    let start_time = Core.Time_ns.now () in 
    Schedulr.Scheduler.schedule (fun () -> 
      let packet = Mock_packet.get_by_index n ~copy_out in
      Schedulr.Scheduler.schedule (fun () ->
        do_work packet;
        Schedulr.Scheduler.schedule (fun () ->
          do_work packet;

                        Schedulr.Scheduler.schedule (fun () ->
                        if Random.int 1000 > 1
                        then   
        
                          (do_work packet;
                          finish start_time) 
                        else 
                          Schedulr.Scheduler.schedule (fun () -> 
                            do_heavy_work packet;
                            Schedulr.Scheduler.schedule (fun () -> 
                              do_heavy_work packet;
                              finish start_time) 
                            ) |> ignore;
                            ) |> ignore
                          
    ) |> ignore ) |> ignore) |> ignore
    (* if _i mod 100 == 0
    then Schedulr.Scheduler.yield () *)
  done;
  Schedulr.Scheduler.schedule (fun () -> ())
;;
let items_total = ref 1_000_000 

let workload ~num_of_spawners () =
  let items_per_worker = !items_total / num_of_spawners in 
  Atomic.set finished 0;
  let time_start = Core.Time_ns.now () in 
  let _ = 
    for _ = 1 to num_of_spawners do 
      Schedulr.Scheduler.schedule (run_processor ~copy_out:true ~n:items_per_worker)
      |> ignore
    done;
    Schedulr.Scheduler.schedule (fun () -> ()) |> ignore;
    while Reporting.Success.unsafe_sum () < num_of_spawners * items_per_worker do 
      (* Schedulr.Scheduler.yield () *) ()
    done; 
    Reporting.Success.unsafe_zero_out ()
  in
  let time_end = Core.Time_ns.now () in 
  let difference = Core.Time_ns.diff time_end time_start 
    |> Core.Time_ns.Span.to_int_ns in 
  Printf.printf "\"time\":%d,\n" (difference/1000_000);
  Stdlib.flush_all ();
  difference;;

let iterations = ref 11

let benchmark ~num_of_domains ~num_of_spawners ~dist_policy (module Sched : Schedulr.Scheduler.S) =
  Printf.printf "{\"sched\":\"%s\",\"spawners\":\"%d\",\"domains\":\"%d\",\"items_total\":%d,\"data\":[\n"
    Sched.scheduler_name
    num_of_spawners
    num_of_domains
    !items_total;
  Sched.init (num_of_domains-1) ?work_distribution_strategy:dist_policy ~f:(fun () ->
    for i = 1 to !iterations do 
      Printf.printf "{\"iteration\":%d,\n" i;
      Unix.sleepf 0.1;
      Sched.Stats.unsafe_zero_steal_attempts ();
      let _ = workload ~num_of_spawners () in 
      Sched.Stats.unsafe_print_steal_attempts ();
      Unix.sleepf 0.1;
      if Sched.local_pending_tasks () != 0  
      then assert false; 
      Sched.Stats.unsafe_print_waited_for_space_on_enque ();
      Sched.Stats.unsafe_print_executed_tasks ();
      let () = 
        let open Reporting.Histogram in 
        let hist = Per_thread.all () in 
        (dump hist);
        Printf.printf "\"latency_median\":%d,\n\"latency_three_nine\":%d\n" 
          (quantile ~quantile:0.5 hist)
          (quantile ~quantile:0.999 hist);
        Reporting.Histogram.Per_thread.zero_out ();
      in
      Printf.printf "}";
      if i < !iterations 
      then Printf.printf ",\n"; 
    done; 
    Printf.printf "]}\n"; 
    Stdlib.flush_all ();
    Stdlib.exit 0) |> ignore;;

(* cmd *)

let () =
  let usage_msg = "benchmark -scheduler (FIFO|LIFO)" in 
  let anon_fun _ = failwith "no anon parameters expected" in
  let scheduler = ref "" in
  let num_of_domains = ref 0 in 
  let num_of_spawners = ref 0 in 
  let dist_policy = ref "" in 
  let speclist =
    [("-scheduler", Arg.Set_string scheduler, "set scheduler algo");
    ("-num-of-domains", Arg.Set_int num_of_domains, "set num of domains");
    ("-items-total", Arg.Set_int items_total, "set total items");
    ("-num-of-spawners", Arg.Set_int num_of_spawners, "set num of spawners");
    ("-iterations", Arg.Set_int iterations, "set num of iterations");
    ("-dist-policy", Arg.Set_string dist_policy, "set distribution policy")] 
  in 
  Arg.parse speclist anon_fun usage_msg;
  let dist_policy = 
    match !dist_policy with 
    | "" -> None
    | "steal" -> Some Schedulr.Scheduler.DistributionPolicy.Steal
    | "steal_localized" -> Some Schedulr.Scheduler.DistributionPolicy.Steal_localized
    | "steal_localized_small" -> Some Schedulr.Scheduler.DistributionPolicy.Steal_localized_small
    | "simple_request" -> Some Steal_and_simple_request
    | "sticky_request" -> Some Steal_and_sticky_request
    | "overflow_queue" -> Some Steal_and_overflow_queue
    | "steal_and_mpmc_overflow" -> Some Steal_and_mpmc_overflow
    | "steal_and_multi_mpmc_overflow" -> Some Steal_and_multi_mpmc_overflow 
    | "steal_and_advanced_request" -> Some Steal_and_advanced_request
    | "steal_slowed" -> Some Steal_slowed
    | _ -> failwith "unknown dist policy"
  in
  let scheduler_module = Flags.parse_sched scheduler in 
  assert (0 < !num_of_domains && !num_of_domains < 512);
  assert (0 < !num_of_spawners && !num_of_spawners < 512);
  assert (0 < !iterations);
  assert (0 < !items_total);
  let num_of_domains = !num_of_domains in
  let num_of_spawners = !num_of_spawners in 
  let _report = 
    let module M = (val scheduler_module : Schedulr.Scheduler.S) in 
    let params = 
      Printf.sprintf "scheduler:%s,domains:%d,spawners:%d" 
        M.scheduler_name num_of_domains num_of_spawners 
    in 
    Reporting.Report.init ~name:"process-packet" ~params
  in
  Reporting.Histogram.Per_thread.init 128; 
  Reporting.Success.init 128; 
  benchmark ~num_of_domains ~num_of_spawners ~dist_policy scheduler_module;;


