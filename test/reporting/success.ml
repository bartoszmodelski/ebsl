
let current_index = Atomic.make 0
let array = ref (Array.init 0 (fun _ -> ref 0));; 

let init count = 
  array := Array.init count (fun _ -> ref 0);; 

let key = Domain.DLS.new_key (fun () ->
  let index = Atomic.fetch_and_add current_index 1 in 
  if index >= (Array.length !array) 
  then 
    (Printf.printf "hist index exceeded initializaiton\n"; 
    Stdlib.flush_all ();
    assert false);
  index);;

  
let local_incr () =
  let cell = Array.get !array (Domain.DLS.get key) in 
  cell := !cell + 1;;

let unsafe_sum () =
  Array.fold_right 
  (fun element curr_sum -> !element + curr_sum) !array 0 
;;

let unsafe_zero_out () = 
  Array.iter (fun cell -> cell := 0) !array;;
