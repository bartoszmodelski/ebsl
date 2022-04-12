let schedule f = 
  let before = Fast_clock.now () in 
  fun () -> 
    let v = f () in 
    let after = Fast_clock.now () in 
    let diff = Core.Int63.(to_int_exn (after - before)) in 
    let histogram = Histogram.Per_thread.local_get_hist () in 
    Histogram.add_val_log histogram diff;
    v

let histogram () =
  let v = Histogram.Per_thread.all () in 
  Histogram.Per_thread.zero_out (); 
  v;; 

let init count = 
  Histogram.Per_thread.init count 