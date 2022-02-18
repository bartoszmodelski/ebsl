open! Schedulr
module Atomic = Dscheck.TracedAtomic

let top ({top; _} : 'a Stack.t) = 
  Atomic.get top

let bottom ({bottom; _} : 'a Stack.t) = 
  Atomic.get bottom

let total_checked = ref 0 
let _create_test_1 () =
  Random.self_init ();
  let stack = Stack.init () in 
  let other_stack = Stack.init () in 
  Atomic.spawn (fun () ->
    Stack.local_push stack 13 |> ignore;
    Stack.local_pop stack |> ignore);
  Atomic.spawn (fun () ->
    Stack.steal ~from:stack ~to_local:other_stack |> ignore);
  Atomic.final (fun () ->     
    total_checked := !total_checked + 1;
    Atomic.check (fun () -> 
      (*
        This set of conditions looks odd at first: 
        - If the steal did not happen, pop happened thus 
        both stacks have top 0 
        - If the steal did happen, first stack had it bottom shifted
        and the second its top incremented      
      *)
      (top stack = 0 && top other_stack = 0) ||
      (top stack = 1 && top other_stack = 1)
    ));;
  
let _create_test_2 () =
  Random.self_init ();
  let stack = Stack.init () in 
  let other_stack = Stack.init () in 
  Atomic.spawn (fun () ->
    Stack.local_push stack 13 |> ignore;
    Stack.local_replace_with_a_random_item stack 17 |> ignore; 
    Stack.local_pop stack |> ignore);
  Atomic.spawn (fun () ->
    Stack.steal ~from:stack ~to_local:other_stack |> ignore); 
  Atomic.final (fun () ->     
    total_checked := !total_checked + 1;
    Atomic.check (fun () -> 
      (*
        This set of conditions looks odd at first: 
        - If the steal did not happen, pop happened thus 
        both stacks have top 0 
        - If the steal did happen, first stack had it bottom shifted
        and the second its top incremented      
      *)
      (top stack = 0 && top other_stack = 0) ||
      (top stack = 1 && top other_stack = 1)
    ));;
      

let () =
  Atomic.trace ~depth_limit:200 _create_test_2;
  Printf.printf "Total checked: %d\n" (!total_checked);;

  