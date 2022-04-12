(* gettime has to be served via vdso for these benchmarks to work *)

let total_iters = 100_000
let benchmark () =
  let s = Digestif.SHA1.digest_string (Random.bits () |> Int.to_string) 
    |> Digestif.SHA1.to_hex in 
  let time_start = Reporting.Fast_clock.now () in 
  for _ = 0 to total_iters do
    ignore (Sys.opaque_identity (Digestif.SHA1.digest_string s))
  done;
  let time_end = Reporting.Fast_clock.now () in
  let diff = Core.Int63.(to_int_exn (time_end - time_start)) in 
  let per_call = (Int.to_float diff) /. (Int.to_float total_iters) in 
  Printf.printf "total %dns, per call %fns\n" diff per_call;
  Stdlib.flush_all ();
  ();;

benchmark () 