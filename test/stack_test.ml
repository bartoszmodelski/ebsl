open! Schedulr
module Atomic = Dscheck.TracedAtomic


let top ({top; _} : 'a Stack.t) = 
  Atomic.get top

let bottom ({bottom; _} : 'a Stack.t) = 
  Atomic.get bottom

let nth ({array; _} : 'a Stack.t) n = 
  Atomic.get (Array.get array n)
  
let test_simple = 
  let stack = Stack.init () in 
  (* push 1 *)
  assert (Stack.local_push stack 1);
  assert (top stack = 1);
  assert (nth stack 0 = Some 1);
  (* push 2 *)
  assert (Stack.local_push stack 2);
  assert (top stack = 2);
  assert (nth stack 1 = Some 2);
  (* pop 2 *)
  assert (Stack.local_pop stack = Some 2);
  assert (top stack = 1);
  assert (nth stack 1 = None);
  (* pop 1 *)
  assert (Stack.local_pop stack = Some 1);
  assert (top stack = 0);
  assert (nth stack 0 = None);
  (* pop inexistent *)
  assert (Stack.local_pop stack = None);
  assert (top stack = 0);;

let test_overload = 
  let stack = Stack.init ~size_exponent:1 () in 
  assert (Stack.local_push stack 1);
  assert (Stack.local_push stack 2);
  assert (not (Stack.local_push stack 3));
  assert (nth stack 0 = Some 1);
  assert (nth stack 1 = Some 2);
  assert (top stack = 2);;

let test_steal = 
  let other_stack = Stack.init () in 
  let my_stack = Stack.init () in 
  assert (Stack.local_push other_stack 1);
  assert (Stack.local_push other_stack 2);
  assert (Stack.local_push other_stack 3);
  assert (Stack.local_push other_stack 4);
  assert (Stack.steal ~from:other_stack ~to_local:my_stack () = 2);
  assert (Stack.local_pop my_stack = Some 2);
  assert (Stack.local_pop my_stack = Some 1);
  assert (Stack.steal ~from:other_stack ~to_local:my_stack () = 1);
  assert (Stack.local_pop my_stack = Some 3);
  assert (Stack.steal ~from:other_stack ~to_local:my_stack () = 1);
  assert (Stack.local_pop my_stack = Some 4);
  assert (Stack.steal ~from:other_stack ~to_local:my_stack () = 0);
  assert (top other_stack = 4);
  assert (bottom other_stack = 4);
  assert (top my_stack = 0);
  assert (bottom my_stack = 0);;

let test_pop_wraparound = (*todo*) ()
let test_steal_wraparound = (*todo*) ()

let load_test_with_stealer = 
  Random.self_init ();
  let stack = Stack.init () in 
  let main_domain = Domain.spawn (fun () ->  
    let pushed = ref 0 in  
    let popped = ref 0 in 
    let sum = ref 0 in 
    for i = 0 to 10_000_000 do
      if Random.int 20 < 10 then 
        (match Stack.local_push stack i with 
        | true -> pushed := !pushed + 1 
        | false -> sum := !sum + i)
      else (
        match Stack.local_replace_with_a_random_item stack i with 
        | None -> sum := !sum + i
        | Some v -> sum := !sum + v); 
      if Random.int 2 = 1 then 
        match Stack.local_pop stack with 
        | Some v -> 
          popped := !popped + 1;
          sum := !sum + v;
        | None -> ()
    done;
    (* drain *)
    let keep_going = ref true in 
    while !keep_going do
      match Stack.local_pop stack with 
      | Some v -> 
        popped := !popped + 1;
        sum := !sum + v
      | None -> 
        keep_going := false 
    done;
    (!pushed,!popped,!sum))
  in
  let f () = 
    Domain.spawn (fun () -> 
    let local_stack = Stack.init () in  
    let stolen = ref 0 in  
    let popped = ref 0 in 
    let sum = ref 0 in 
    for _ = 1 to 1_000_000 do
      let count = Stack.steal ~from:stack ~to_local:local_stack () in
      stolen := !stolen + count;
      let keep_going = ref true in 
      while !keep_going do
        match Stack.local_pop local_stack with 
        | Some v -> 
          popped := !popped + 1;
          sum := !sum + v
        | None -> 
          keep_going := false 
      done;
    done;
    assert (top local_stack = bottom local_stack);
    assert (!stolen = !popped);
    !stolen, !sum)
  in
  let thieves = List.init 15 (fun _ -> f ()) in
  let (pushed, popped, sum) = Domain.join main_domain in
  let thieves_results = List.map Domain.join thieves in 
  let stolen = (thieves_results 
    |> List.map (fun (v, _) -> v) 
    |> List.fold_right Int.add) 0 in
  let stolen_sum = (thieves_results 
    |> List.map (fun (_, v) -> v) 
    |> List.fold_right Int.add) 0 in 
  assert (top stack = bottom stack);
  assert (pushed = popped + stolen);
  Printf.printf "Expected size: %d, actual size: %d, stolen: %d, sum %d\n" 
    pushed popped stolen (sum + stolen_sum);;