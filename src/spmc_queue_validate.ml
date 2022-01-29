open! Spmc_queue

let total_checked = ref 0

let create_test () =
  let queue = Spmc_queue.init ~size_pow:5 () in
  let queue_2 = Spmc_queue.init ~size_pow:5 () in
  Atomic.spawn (fun () -> 
    assert (Spmc_queue.local_enqueue queue "");
    assert (Spmc_queue.local_enqueue queue "");
    assert (Option.is_some (Spmc_queue.local_dequeue queue)));
  Atomic.spawn (fun () -> 
    while not (Spmc_queue.steal queue ~local_queue:queue_2) do () done);
  Atomic.final (fun () ->
    total_checked := !total_checked + 1;
    let ({head; tail; _} : string t) = queue in
    let head_value = Atomic.get head in
    let tail_value = Atomic.get tail in
    (*Atomic.check (fun () -> 
      Array.for_all (fun v -> Atomic.get v = empty_cell) array);*)
    Atomic.check (fun () -> 
      head_value = tail_value));;
  
let () =
  Atomic.trace ~depth_limit:32 create_test;
  Printf.printf "Total checked: %d\n" (!total_checked);;