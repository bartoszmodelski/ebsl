type t = int Array.t 

let init ?(size=32) () = Array.init size (fun _ -> 0)

let log_val t value =
  let size = (Array.length t - 1) in
  let index = max (Core.Int.floor_log2 value) size in
  let current_value = Array.get t index in
  Array.set t index (current_value + 1)
