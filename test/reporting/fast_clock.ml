
(* Can we somehow check that gettime gets served via vDSO 
  (and return errors from get otherwise)?
  
  Perhaps, we could quickly bench it on startup. Calls should
  take less than 50ns. 
*)

let realtime = 
  match Core.Unix.Clock.gettime with 
  | Error _ -> assert false 
  | Ok v -> v

let now () = realtime Core.Unix.Clock.Monotonic
