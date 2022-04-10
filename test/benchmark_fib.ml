let _ = Printexc.record_backtrace true

let realtime_clock = 
  match Core.Unix.Clock.gettime with 
  | Error _ -> assert false 
  | Ok v -> v

let clock () = 
  realtime_clock Core.Unix.Clock.Monotonic
  |> Core.Int63.to_int_exn
;;


let rec fib n = 
  match n with 
  | 0 -> 1 
  | 1 -> 1 
  | _ -> 
    let a = Schedulr.Scheduler.schedule (fun () -> fib (n-1)) in
    let b = Schedulr.Scheduler.schedule (fun () -> fib (n-2)) in 
    Schedulr.Scheduler.await a + Schedulr.Scheduler.await b;;

let log s = 
  Printf.printf "%s\n" s;
  Stdlib.(flush stdout);;

let workload ~n_jobs ~n () = 
  let promises = 
    Array.init n_jobs
      (fun _ -> 
        Schedulr.Scheduler.schedule  
        (fun () -> 
          fib n 
          |> Sys.opaque_identity
          |> ignore))
  in 
  Array.iter Schedulr.Scheduler.await promises;;

let time f = 
  let time_start = clock () in 
  f ();
  let time_end = clock () in 
  let difference = 
    time_end - time_start 
  in 
  Printf.printf "time:%d\n" (difference/1000_000);
  Stdlib.flush_all ();;

let get_gc_stat () = 
  let gc = Gc.quick_stat ()
 in
  (gc.minor_words, gc.major_words)
  

let benchmark ~domains ~n_jobs (module Sched : Schedulr.Scheduler.S) = 
  Sched.init (domains-1) ~f:(fun () ->  
    Printf.printf "start\n";
    for i = 0 to 10 do 
      Printf.printf "iteration:%d\n" i;
      Unix.sleepf 0.1;
      let () = 
        let (start_minor_words, start_major_words) = get_gc_stat () in 
        time (workload ~n_jobs ~n:20);
        let (end_minor_words, end_major_words) = get_gc_stat () in
        Printf.printf "minor_words:%.0f\nmajor_words:%.0f\n" 
          (end_minor_words -. start_minor_words)
          (end_major_words -. start_major_words)
      in
      while Sched.pending_tasks () != 0 do 
        Unix.sleepf 0.1;
      done;
      Stdlib.flush_all ();
    done;
    Printf.printf "done\n";
    Stdlib.flush_all ();
    Stdlib.exit 0) |> ignore;;


let () =
  let usage_msg = "benchmark -scheduler (FIFO|LIFO)" in 
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
  let domains = !num_of_domains in
  let n_jobs = !num_of_spawners in 
  benchmark ~domains ~n_jobs scheduler_module;;
