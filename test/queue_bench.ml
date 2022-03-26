
let bench ~num_of_threads ~item_count (module Queue : Datastructures.Queue_intf.S) = 
  Random.init 0;
  let spawned = Atomic.make 0 in 
  let _done = Atomic.make 0 in 
  let started = Atomic.make false in 
  let per_thread_item_count = item_count / num_of_threads in 
  let queue = Queue.init () in
  let domains = 
    List.init num_of_threads (fun i -> 
      Domain.spawn (fun () -> 
        Atomic.incr spawned; 
        while not (Atomic.get started) do 
          () 
        done;
        for _ = 0 to per_thread_item_count do 
          if i mod 2 == 0 then 
            Queue.enqueue queue 0
          else
            while Option.is_none (Queue.dequeue queue) do () done;
        done;
        Atomic.incr _done))
  in
  while Atomic.get spawned < num_of_threads do () done;
  let before = Schedulr.Fast_clock.now () in 
  Atomic.set started true; 
  while Atomic.get _done < num_of_threads do () done; 
  let after = Schedulr.Fast_clock.now () in 
  List.iter Domain.join domains;
  Base.Int63.(to_int64(after-before)) 
;;


let () =
  let time = 
    bench ~num_of_threads:2 ~item_count:200_000 (module Datastructures.Mpmc_queue)
  in
  let time = Int64.div time 1_000_000L in 
  Printf.printf "time: %Ld ms" time;;