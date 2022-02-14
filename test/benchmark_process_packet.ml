let buffers =
  let buffers = 
    List.init 10 (fun _ -> 
      Buffer.create 0)
  in 
  List.iteri (fun buff_i buffer -> 
    List.init (200 + buff_i * 30) (fun i -> 
      let c = 
        if i mod (20 + buff_i) = 0 && 0 < i && i < 300 
        then '\n' else '-'
      in
      Buffer.add_char buffer c) |> ignore) 
      buffers;
  buffers;;
let rec find_spaces buffer index bound already_found =
  let index = index + 1 in 
  if index >= bound 
  then 
    already_found 
  else 
    let current = 
      if Buffer.nth buffer index = '\n'        
      then ([index]) else []
    in 
    find_spaces buffer index bound (current @ already_found)

    
let smoke_test = 
  assert (
    List.length (find_spaces (List.nth buffers 1) 0 230 []) = 10);
  assert (
    List.length (find_spaces (List.nth buffers 5) 0 230 []) = 9);;
    

let log s = 
  Printf.printf "%s\n" s;
  Stdlib.(flush stdout);;

let total_executions = 10
let finished = Atomic.make 0
let rec run_processor ~copy_out ~n () = 
  if n < 0 
  then 
    (let count = Atomic.fetch_and_add finished 1 in 
    if count >= total_executions - 1 then
    log "done!")
  else ( 
    let len = List.length buffers in
    let index = n mod len in 
    let buffer = 
      let source = List.nth buffers index in 
      if copy_out 
      then (
        let b = (Buffer.create 0) in 
        Buffer.add_buffer b source;
        b)
      else 
        source 
    in
    let len = Buffer.length buffer in 
    let f from to_ = 
      find_spaces buffer from to_ [] 
      |> Sys.opaque_identity 
      |> ignore
    in  
    f 0 (len/3);
    f (len/3) (2*len/3);
    f (2*len/3) (len - 1);
    Stdlib.(flush stdout);
    Schedulr.Scheduler.schedule (run_processor ~copy_out ~n:(n-1)) 
    |> ignore)

let benchmark () = 
  Schedulr.Scheduler.FIFO.init 3 ~f:(fun () ->  
    for _i = 1 to total_executions do 
      Schedulr.Scheduler.schedule (run_processor ~copy_out:true ~n:200_000)
      |> ignore
    done;
    while Atomic.get finished < total_executions do 
      Schedulr.Scheduler.yield ()
    done;
    log "exiting";
    Stdlib.exit 0);;

benchmark ()