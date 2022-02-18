type t = int Array.t 

let init ?(size=32) () = Array.init size (fun _ -> 0)

let log_val t value =
  if value < 1 then 
    () 
  else 
    (let size = (Array.length t - 1) in
    let index = min (Core.Int.floor_log2 value) size in
    let current_value = Array.get t index in
    Array.set t index (current_value + 1));;

let zero_out t = 
  Array.fill t 0 (Array.length t) 0;;

let merge l =
  assert (List.length l > 0);
  let required_len = Array.length (List.nth l 0) in
  let output = Array.init required_len (fun _ -> 0) in  
  List.iter (fun t -> 
    assert (Array.length t = required_len);
    Array.iteri (fun i v -> 
      let current_sum = Array.get output i in
      Array.set output i (current_sum + v)) t) l;
  output;;

let dump t = 
  Array.to_list t 
  |> List.map Int.to_string
  |> String.concat ","
  |> Printf.printf "latency:[%s]\n"


let test = 
  let t1 = init () in
  log_val t1 1;
  log_val t1 3; 
  let t2 = init () in
  log_val t2 3;
  log_val t2 9; 
  let t3 = init () in
  log_val t3 3;
  let final = merge [t1; t2; t3] in  
  let nth = Array.get final in 
  assert (nth 0 = 1); 
  assert (nth 1 = 3); 
  assert (nth 2 = 0); 
  assert (nth 3 = 1); 
  assert (nth 4 = 0);;