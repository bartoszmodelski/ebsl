
let total_rounds = 30
let bench ~num_of_domains ~item_count (module Queue : Datastructures.Queue_intf.S) = 
  assert (num_of_domains mod 2 == 0);
  Random.init 0;
  let spawned = Atomic.make 0 in 
  let _done = Atomic.make 0 in 
  let current_round = Atomic.make (-1) in 
  let per_thread_item_count = item_count / num_of_domains in 
  let queue = Queue.init () in
  let domains = 
    List.init num_of_domains (fun i -> 
      Domain.spawn (fun () -> 
        Atomic.incr spawned; 
        let round = ref 0 in 
        while !round < total_rounds do 
          while (Atomic.get current_round) < !round do () done;
          for _ = 0 to per_thread_item_count do 
            if i mod 2 == 0 then 
              Queue.enqueue queue 0
            else
              while Option.is_none (Queue.dequeue queue) do () done;
          done;
          Atomic.incr _done;
          round := !round + 1;
        done))
  in
  while Atomic.get spawned < num_of_domains do () done;
  let times = ref [] in 
  while (Atomic.get current_round)+1 < total_rounds do 
    Unix.sleepf 0.001;
    let before = Fast_clock.now () in 
    Atomic.incr current_round; 
    while Atomic.get _done < num_of_domains do () done; 
    let after = Fast_clock.now () in
    Atomic.set _done 0;
    let diff =  Base.Int63.(to_int64(after-before)) in 
    times := diff :: !times
  done;
  Atomic.incr current_round;
  List.iter Domain.join domains;
  !times;;


let () =
  let usage_msg = "benchmark -queue (LF|LOCK) -num-of-domains <num>" in 
  let anon_fun _ = failwith "no anon parameters expected" in
  let queue = ref "" in
  let num_of_domains = ref 0 in 
  let num_of_queues = ref 0 in 
  let speclist =
    [("-queue", Arg.Set_string queue, "set scheduler algo");
    ("-num-of-domains", Arg.Set_int num_of_domains, "set num of domains");
    ("-num-of-queues", Arg.Set_int num_of_queues, "set num of queues (LFM only)")] 
  in 
  Arg.parse speclist anon_fun usage_msg;
  let queue_module =
      match !queue with 
      | "LF" -> (module Datastructures.Mpmc_queue : Datastructures.Queue_intf.S)
      | "LOCK" -> (module Datastructures.Lock_queue)
      | "LFM" -> 
        assert (!num_of_queues > 0);
        (module Datastructures.Multi_mpmc_queue.Make(struct let num_of_queues = !num_of_queues end))
      | _ -> assert false
  in 
  assert (0 < !num_of_domains && !num_of_domains < 512);
  let num_of_domains = !num_of_domains in
  let times = 
    bench ~num_of_domains ~item_count:1_000_000 queue_module
  in
  let times = 
    List.map (fun v -> Int64.div v 1_000_000L) times 
    |> List.map (Int64.to_string) |> String.concat ","  
  in
  Printf.printf "[%s]\n" times;;