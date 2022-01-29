let test_1 () = 
  Scheduler.init 3 ~f:(fun () ->
    let a = Scheduler.schedule (fun () -> 
      Unix.sleep 1; "a") in 
    let b = Scheduler.schedule (fun () -> 
      Unix.sleep 1; "b") in
    Printf.printf "scheduled\n"; 
    Printf.printf "- %s %s\n" (Scheduler.await a) (Scheduler.await b);
    Printf.printf "done\n";
    Stdlib.flush_all ();
    ());
  Unix.sleep 2;
  Printf.printf "exiting\n";
  Stdlib.flush_all ();;

let test_2 () = 
  Scheduler.init 5 ~f:(fun () ->
    let rec fib n = 
      match n with  
      | 0 | 1 -> n
      | _ -> 
        let a = Scheduler.schedule (fun () -> fib (n - 1)) in 
        let b = Scheduler.schedule (fun () -> fib (n - 2)) in 
        Scheduler.(await a + await b) 
    in
    Printf.printf "result: %d\n" (fib 16);
    Stdlib.flush_all ());
  let _a = Stdlib.read_line () in
  Printf.printf "exiting\n";
  Stdlib.flush_all ();;

test_2 ()