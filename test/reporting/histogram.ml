type t = int Array.t 

let init ?(size=32) () = Array.init size (fun _ -> 0)

let add_val_log t value =
  if value < 1 
  then () 
  else 
    (let size = (Array.length t - 1) in
    let index = min (Core.Int.floor_log2 value) size in
    let current_value = Array.get t index in
    Array.set t index (current_value + 1));;


let add_val t value =
  let current_value = Array.get t value in
  Array.set t value (current_value + 1);;

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
  |> List.map (Printf.sprintf "%6d")
  |> String.concat ","
  |> Printf.printf "latency:[%s]\n"

let quantile ~quantile t = 
  let total = 
    Array.fold_right 
      (fun element  curr_sum -> element + curr_sum) t 0 
  in
  let index = ref 0 in 
  let array_index = ref 0 in 
  let target_index = 
    (Int.to_float total) *. quantile
  in 
  while Int.to_float !index < target_index do 
    index := !index + (Array.get t !array_index);
    array_index := !array_index + 1;
  done;
  !array_index - 1
;;

let test = 
  let t1 = init () in
  add_val_log t1 1;
  add_val_log t1 3; 
  let t2 = init () in
  add_val_log t2 3;
  add_val_log t2 9; 
  let t3 = init () in
  add_val_log t3 3;
  let final = merge [t1; t2; t3] in  
  let nth = Array.get final in 
  assert (nth 0 = 1); 
  assert (nth 1 = 3); 
  assert (nth 2 = 0); 
  assert (nth 3 = 1); 
  assert (nth 4 = 0);;

let test_quantile_1 = 
  let t = init () in 
  for i = 0 to 119 do
    add_val t (i/4);
  done;
  assert (quantile ~quantile:0.5 t == 14);
  assert (quantile ~quantile:0.01 t == 0);
  assert (quantile ~quantile:0.99 t == 29);; 


let test_quantile_2 = 
  let t = init () in 
  for _ = 0 to 30 do
    add_val t 0;
  done;
  add_val t 10;
  assert (quantile ~quantile:0.99 t == 10);; 

module Per_thread = struct 
  let current_index = Atomic.make 0
  let array = ref (Array.make 0 (init ()));; 

  let init ?size count = 
    array := Array.init count (fun _ -> init ?size ());; 

  let key = Domain.DLS.new_key (fun () ->
    let index = Atomic.fetch_and_add current_index 1 in 
    if index >= (Array.length !array) 
    then 
      (Printf.printf "hist index exceeded initializaiton\n"; 
      Stdlib.flush_all ();
      assert false);
    index);;

  let local_get_hist () = 
    Array.get !array (Domain.DLS.get key);;

  let all () =
    Array.to_list !array |> merge;;

  let zero_out () = 
    Array.iter zero_out !array;;

  let dump_each () = 
    Array.iter dump !array 
end