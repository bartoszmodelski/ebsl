open Schedulr.Scheduler
open Schedulr.Instance
let test_1 () = 
  FIFO.init 3 ~f:(fun () ->
    let a = schedule (fun () -> 
      Unix.sleep 1; "a") in 
    let b = schedule (fun () -> 
      Unix.sleep 1; "b") in
    Printf.printf "scheduled\n"; 
    Printf.printf "- %s %s\n" (await a) (await b);
    Printf.printf "done\n";
    Stdlib.flush_all ();
    ()) |> ignore;
  Unix.sleep 2;
  Printf.printf "exiting\n";
  Stdlib.flush_all ();;

let test_2 () = 
  LIFO.init 10 ~f:(fun () ->
    let rec fib n = 
      match n with  
      | 0 | 1 -> n
      | _ -> 
        let a = schedule (fun () -> fib (n - 1)) in 
        let b = schedule (fun () -> fib (n - 2)) in 
        (await a + await b) 
    in
    Printf.printf "result: %d\n" (fib 13);
    Stdlib.flush_all ()) |> ignore;
  let _a = Stdlib.read_line () in
  Printf.printf "exiting\n";
  Stdlib.flush_all ();;

test_2 ()