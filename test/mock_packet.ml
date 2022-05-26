let premade_buffers =
  List.init 100 (fun buff_i ->
    let len = 1000 + buff_i * 1 in 
    let buffer = Buffer.create len in 
    for i = 0 to len do
      let c = 
        if i mod (20 + buff_i) = 0 && 0 < i && i < 300 
        then '\n' else '-'
      in
      Buffer.add_char buffer c;
    done;
    buffer) 
  ;;
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
    Sys.opaque_identity source;;
  
let get_rand () = 
  let source = List.nth premade_buffers 
    (Random.int (List.length premade_buffers)) 
  in 
  Buffer.to_bytes source
