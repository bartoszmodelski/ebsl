
module Global = struct 
  type 'a t = 'a Spsc_queue.t Lock_queue.t 

  let init () = Lock_queue.init ~size_exponent:20 () 
end

module Local = struct 
  type 'a t = {
    send: 'a Spsc_queue.t Array.t ref; 
    send_index: int ref; 
    receive: 'a Spsc_queue.t Array.t ref;
    receive_index: int ref;
    global_queue: 'a Spsc_queue.t Lock_queue.t;
  }

  let init global_queue = { 
      send = ref (Array.init 0 (fun _ -> Spsc_queue.init ()));
      send_index = ref 0;
      receive = ref (Array.init 0 (fun _ -> Spsc_queue.init ()));
      receive_index = ref 0;
      global_queue;
    }

  let enqueue {send; send_index; global_queue; _} element = 
    let inserted = ref false in 
    let len = Array.length !send in 
    let iter_until = !send_index + len in 
    while not !inserted && !send_index < iter_until do
      if 
        Spsc_queue.enqueue
          (Array.get !send (!send_index mod len))
          element
      then 
        inserted := true;
      send_index := !send_index + 1;
    done;
    if not !inserted then 
      (let new_queue = Spsc_queue.init () in 
      let new_send = 
        Array.init 
        (len + 1)
        (fun i -> if i == len then new_queue else Array.get !send i) 
      in
      send := new_send;
      assert (Spsc_queue.enqueue new_queue element);
      Lock_queue.enqueue global_queue new_queue);;

  let dequeue {receive; receive_index; global_queue; _} = 
    let item = ref None in 
    let len = Array.length !receive in 
    let iter_until = !receive_index + len in 
    while Option.is_none !item && !receive_index < iter_until do 
      let q = Array.get !receive (!receive_index mod len) in 
      item := Spsc_queue.dequeue q;
      receive_index := !receive_index + 1;      
    done;
    if Option.is_some !item 
    then !item 
    else (
      match Lock_queue.dequeue global_queue with 
      | None -> None 
      | Some q -> 
        let new_receive = 
          Array.init 
          (len + 1)
          (fun i -> if i == len then q else Array.get !receive i) 
        in
        receive := new_receive;
        Spsc_queue.dequeue q);;
end 