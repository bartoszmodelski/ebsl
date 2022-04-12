let get_time () = (Reporting.Fast_clock.now () |> Core.Int63.to_int_exn)


let total_wait = Atomic.make 0
let _done = Atomic.make 0;;
let total_calls = 5000000

let finish start_time = 
  let end_time = get_time () in 
  let diff = end_time - start_time in 
  let hist = Reporting.Histogram.Per_thread.local_get_hist () in 
  Reporting.Histogram.add_val_log hist diff;     
  Atomic.incr _done;;

let rec slow_task start_time n = 
  match n with 
  | 0 -> finish start_time
  | n -> 
    Digestif.SHA1.digest_string (Random.bits () |> Int.to_string) 
    |> Sys.opaque_identity 
    |> ignore;
    Micropools.schedule (fun () -> slow_task start_time (n-1));;

let fast_task slow_pool start_time = 
  if Random.int 10000 > 0
  then 
    finish start_time 
  else  
    Micropools.schedule ?pool_name:slow_pool (fun () -> slow_task start_time 2000);;


let bench mode =
  let slow_pool = 
    match mode with 
    | `Pools -> 
      Micropools.schedule ~pool_size:2 ~pool_name:"a" (fun () -> ());  
      Micropools.schedule ~pool_size:1 ~pool_name:"b" (fun () -> ());    
      Some "b"
    | `Single -> 
      Micropools.schedule ~pool_size:3 ~pool_name:"a" (fun () -> ());
      None 
  in
  Unix.sleep 1;
  (* bench *)
  for _ = 1 to total_calls do 
    let start_time = get_time () in 
    Micropools.schedule ~pool_name:"a" (fun () -> 
      fast_task slow_pool start_time)
  done;
  while Atomic.get _done < total_calls do () done;
  Printf.printf "done\n";
  Stdlib.flush_all ();;


let () =
  Reporting.Histogram.Per_thread.init ~size:48 55;
  let usage_msg = "benchmark -mode (pools|single)" in 
  let mode = ref "" in
  let speclist =
    [("-mode", Arg.Set_string mode, "set mode")] 
  in 
  Arg.parse speclist Flags.anon_fun usage_msg;
  let mode =
      match !mode with 
      | "pools" -> `Pools
      | "single" -> `Single
      | _ -> assert false
  in 
  bench mode; 
  (*Schedulr.Histogram.Per_thread.dump_each ();*)
  let merged = Reporting.Histogram.Per_thread.all () in 
  Reporting.Histogram.dump merged;
  let f v = 
    Reporting.Histogram.quantile ~quantile:v merged
  in
  Printf.printf ".5: %d, .99: %d, .999: %d, 9995: %d\n" 
    (f 0.5) (f 0.99) (f 0.999) (f 0.9995);;