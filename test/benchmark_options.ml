
let _ = Printexc.record_backtrace true

let log s = 
  Printf.printf "%s\n" s;
  Stdlib.(flush stdout);;

let finished = Atomic.make 0

let price_option ~start_price ~volatility ~total_depth ~t ~call_price ~rate = 
  (* inits *)
  let e = 2.71828 in 
  let deltaT = t /. (Int.to_float total_depth) in 
  let up_move = e ** (volatility *. sqrt(deltaT)) in
  Printf.printf "up_move: %f\n" up_move;
  let down_move = 1. /. up_move in
  let p_of_up_move = 
    let x = e ** (rate *. deltaT) in 
    (x -. down_move) /. (up_move -. down_move) 
  in 
  (* comptue stock price *)
  let stock_layer = Array.init (total_depth+1) (fun _ -> 0.) in 
  let rec f price iter = 
    if iter == Array.length stock_layer 
    then () 
    else
      let price = price *. (down_move ** 2.) in 
      Array.set stock_layer iter price;
      f price (iter+1)
  in
  let top_price = start_price *. (up_move ** (Int.to_float total_depth)) in 
  Array.set stock_layer 0 top_price;
  f top_price 1;
  Array.iter (fun v -> Printf.printf "%f," v) stock_layer;
  let backward_layers = Array.init (total_depth+1) (fun i -> 
    Array.init (total_depth+1-i) (fun _ -> 0.)) in 
  (* copy over *)
  let backward_last = Array.get backward_layers 0 in 
  Array.iteri  (fun index v -> 
    let payout = max 0. (v -. call_price) in 
    Array.set backward_last index payout) 
    stock_layer;
  (* backpropagate *)
  for i = 1 to (Array.length backward_layers) - 1 do 
    let previous_array = Array.get backward_layers (i-1) in  
    let current_array = Array.get backward_layers (i) in
    for j = 0 to (Array.length current_array) - 1 do 
      let x = Array.get previous_array j in 
      let y = Array.get previous_array (j+1) in 
      assert (x >= y);
      let out = 
        (p_of_up_move *. x +. (1.-.p_of_up_move) *. y)
        *. (e ** (-.rate *. deltaT))
      in
      Array.set current_array j out;
    done;
  done;
  Printf.printf "\n-----\n";
  Array.iter (fun arr -> 
    Array.iter (fun v -> Printf.printf "%f," v) arr; 
    Printf.printf "|\n") 
    backward_layers;
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

let run_processor ~n () =
  for _i = 1 to n do 
    let start_time = Core.Time_ns.now () in 
    Schedulr.Scheduler.schedule (fun () -> 
      finish start_time) |> ignore
  done;
  Schedulr.Scheduler.schedule (fun () -> ())
;;
let items_total = ref 1_000

let workload ~num_of_spawners () =
  let items_per_worker = !items_total / num_of_spawners in 
  Atomic.set finished 0;
  let time_start = Core.Time_ns.now () in 
  let _ = 
    for _ = 1 to num_of_spawners do 
      Schedulr.Scheduler.schedule (run_processor ~n:items_per_worker)
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

let benchmark ~num_of_domains ~num_of_spawners (module Sched : Schedulr.Scheduler.S) =
  Printf.printf "{\"sched\":\"%s\",\"spawners\":\"%d\",\"domains\":\"%d\",\"items_total\":%d,\"data\":[\n"
    Sched.scheduler_name
    num_of_spawners
    num_of_domains
    !items_total;
  Sched.init (num_of_domains-1) ~f:(fun () ->
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
  let speclist =
    [("-scheduler", Arg.Set_string scheduler, "set scheduler algo");
    ("-num-of-domains", Arg.Set_int num_of_domains, "set num of domains");
    ("-num-of-spawners", Arg.Set_int num_of_spawners, "set num of spawners");
    ("-iterations", Arg.Set_int iterations, "set num of iterations")] 
  in 
  Arg.parse speclist anon_fun usage_msg;
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
  benchmark ~num_of_domains ~num_of_spawners scheduler_module;;


