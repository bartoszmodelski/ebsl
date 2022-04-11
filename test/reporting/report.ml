module Row = struct 
  type t = {
    num : int;
    runtime : int; 
  }
  let init ~num ~runtime = {num; runtime}

  let json {num; runtime} =
    Printf.sprintf "{num:%d, runtime:%d}" num runtime;; 

end

type t = {
  name : String.t;
  params : String.t; 
  rows : Row.t List.t ref; 
}

let init ~name ~params =
  {name; params; rows = ref []};;

let log_execution ({rows; _} : t) runtime = 
  let num = List.length !rows in 
  let row = Row.init ~num ~runtime in 
  rows := row :: !rows;;

let json {name; params; rows} = 
  let rows = 
    let rows_rev = List.rev !rows in
    String.concat "," (List.map Row.json rows_rev) 
  in
  Printf.sprintf "{name:'%s',params:'%s',rows:[%s]}" name params rows;; 


let%expect_test "" =
  let t = init ~name:"bench_name" ~params:"-C 5000 -sched FIFO" in 
  Printf.printf "%s" (json t);
  [%expect {| {name:'bench_name',params:'-C 5000 -sched FIFO',rows:[]} |}];
  log_execution t 100;
  log_execution t 110;
  Printf.printf "%s" (json t);
  [%expect {| {name:'bench_name',params:'-C 5000 -sched FIFO',rows:[{num:0, runtime:100},{num:1, runtime:110}]} |}];;

