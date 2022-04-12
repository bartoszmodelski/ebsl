module Make(M : sig 
  val num_of_queues : int
end) : Queue_intf.S = struct
  type 'a t = 'a Mpmc_queue.t Array.t

  let init ?size_exponent () = 
    Array.init M.num_of_queues (fun _ -> Mpmc_queue.init ?size_exponent ());;

  let enqueue t item = 
    let index = Random.int (Array.length t) in 
    Mpmc_queue.enqueue (Array.get t index) item 

  let dequeue t = 
    let length = Array.length t in 
    let shift = Random.int length in 
    let rec f index = 
      if index == length then 
        None 
      else 
        (let queue = Array.get t ((index + shift) mod length) in 
        match Mpmc_queue.dequeue queue with
        | Some v -> Some v 
        | None -> f (index + 1))
    in  
    f 0;;
end
