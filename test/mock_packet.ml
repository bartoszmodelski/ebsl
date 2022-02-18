
let _ = Printexc.record_backtrace true

let premade_buffers =
  let buffers = 
    List.init 10 (fun _ -> 
      Buffer.create 0)
  in 
  List.iteri (fun buff_i buffer -> 
    List.init (2000 + buff_i * 30) (fun i -> 
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
    find_spaces buffer index bound (current @ already_found);;

let get_by_index n ~copy_out =
  let len = List.length premade_buffers in
  let index = n mod len in 
  let source = List.nth premade_buffers index in 
  if copy_out 
  then (
    let b = (Buffer.create 0) in 
    Buffer.add_buffer b source;
    b)
  else 
    source;;
  
