
let log s = 
  Printf.printf "%s\n" s;
  Stdlib.flush_all ();;

let test () =
  for _ = 1 to 10 do
    Micropools.schedule 
      ~pool_name:"a" 
      (fun () -> 
        Micropools.schedule 
          ~pool_name:"b"
          (fun () -> 
            log "-"
          ));
  done;
  log "done";
  Unix.sleep 15;;

test ()