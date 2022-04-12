(* gettime has to be served via vdso for these benchmarks to work *)

let realtime_clock = 
  match Core.Unix.Clock.gettime with 
  | Error _ -> assert false 
  | Ok v -> v;;

let time () = realtime_clock Core.Unix.Clock.Realtime
let time_test () = time ()

let to_int v = 
  match Base.Int63.to_int v with 
  | None -> assert false 
  | Some v -> v

let total_iters = 100_000
let benchmark () =
  let time_start = to_int (time ()) in 
  for _ = 0 to total_iters do
    ignore (Sys.opaque_identity (time_test ()))
  done;
  let time_end = to_int (time ()) in 
  let diff = time_end - time_start in 
  let per_call = (Int.to_float diff) /. (Int.to_float total_iters) in 
  Printf.printf "total %dns, per call %fns\n" diff per_call;
  if per_call > 50. 
  then 
    Printf.printf "timer seems slow :( check if handled by vdso\n" 
  else 
    Printf.printf "looks ok?\n";
  Stdlib.flush_all ();
  ();;

benchmark () 