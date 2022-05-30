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


let realtime_clock = 
  match Core.Unix.Clock.gettime with 
  | Error _ -> assert false 
  | Ok v -> v

let clock () = 
  realtime_clock Core.Unix.Clock.Monotonic
  |> Core.Int63.to_int_exn
;;

let delay = 20_000_000 
let window = 10_000_000

let codec data = 
  Digestif.MD5.digest_bytes data 
  |> Sys.opaque_identity |> ignore;;

let start_leg ~n ~start_time slot () = 
  for i = 0 to n do 
    let data = random_buffer () in 
    codec data;
    while start_time + delay * i > clock () do 
      Schedulr.Scheduler.yield () 
    done;
    while Option.is_some (Atomic.get slot) do 
      Schedulr.Scheduler.yield () 
    done;
    Atomic.set slot (Some data);
  done;;

let send_packet_out ~deadline data () = 
  codec data;
  if clock () > deadline 
  then Reporting.Success.local_incr ()
;;

let n_legs_const = 2
let global_latency_breach_count = Atomic.make 0;; 

let start_mixer ~n_packets ~n_legs () =
  let start_time = clock () in 
  let exchange_array = 
    Array.init n_legs (fun _ -> 
      let slot = Atomic.make None in 
      Micropools.schedule ~pool_name:"receive_legs" 
        (start_leg ~n:n_packets ~start_time slot)
      |> ignore;
      slot) 
  in 
  for i = 0 to n_packets do 
    let output = empty_buffer () in 
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
      for j = 0 to buffer_size / 2 - 3 do 
        let v_1 = Bytes.get_int32_be buffer (j*2) in 
        let v_2 = Bytes.get_int32_be output (j*2) in 
        Bytes.set_int32_be output (j*2) (Int32.add v_1 v_2)
      done;
    done;
    let deadline = start_time + delay * i + window in 
    for _ = 0 to n_legs - 1 do 
      Micropools.schedule ~pool_name:"send_legs" 
       (send_packet_out ~deadline output) |> ignore
    done; 
  done;;

let log s = 
  Printf.printf "%s\n" s;
  Stdlib.(flush stdout);;


let n_packets_const = 200

let workload ~n_mixers () = 
  let indicators = 
    Array.init n_mixers 
      (fun _ -> 
        let finished = Atomic.make false in 
        Micropools.schedule ~pool_name:"mixers" 
        (fun () -> start_mixer 
          ~n_packets:n_packets_const
          ~n_legs:n_legs_const (); 
          Atomic.set finished true); 
        finished) 
  in 
  Array.iter (fun finished -> 
    while not (Atomic.get finished) do () done) indicators;;

let benchmark ~n_mixers (module Sched : Schedulr.Scheduler.S) = 
  Reporting.Success.init 128;
  Micropools.schedule ~pool_size:20 ~pool_name:"mixers" (fun () -> ());
  Micropools.schedule ~pool_size:30 ~pool_name:"send_legs" (fun () -> ());
  Micropools.schedule ~pool_size:50 ~pool_name:"receive_legs" (fun () -> ());
  Printf.printf "{\"data\":[\n";
  let skip = 2 in
  let total = 15 in
  for i = 0 to total do 
    if i > skip then
    Printf.printf "{\"iteration\":%d," i;
    Unix.sleepf 0.1;
    Reporting.Success.unsafe_zero_out ();
    workload ~n_mixers ();
    let breach_count = Reporting.Success.unsafe_sum () in 
    Unix.sleepf 0.1;
    if i > skip 
    then Printf.printf "\"breaches-rate\":%f}" 
      (Int.to_float (breach_count / n_mixers / n_legs_const) /. 
      (Int.to_float n_packets_const));
    if skip < i && i < total
    then Printf.printf ",\n";
    Stdlib.flush_all ();
  done;
  Printf.printf "\n]}\n";
  Stdlib.flush_all ();;


let () =
  let usage_msg = "benchmark -scheduler (FIFO|LIFO)" in 
  let anon_fun _ = failwith "no anon parameters expected" in
  let scheduler = ref "" in
  let num_of_spawners = ref 0 in 
  let speclist =
    [("-scheduler", Arg.Set_string scheduler, "set scheduler algo");
    ("-num-of-spawners", Arg.Set_int num_of_spawners, "set num of spawners")] 
  in 
  Arg.parse speclist anon_fun usage_msg;
  let scheduler_module = Flags.parse_sched scheduler in 
  assert (0 < !num_of_spawners);
  let n_mixers = !num_of_spawners in 
  benchmark ~n_mixers scheduler_module;;
