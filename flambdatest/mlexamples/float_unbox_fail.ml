(* The env_extension of continuation is not cleared after removing unused
   parameters, which may leave some names free at toplevel because theyre are
   removed as parameters but not from the env_extension. *)
let[@inline always] f () =
  let r = ref 0. in
  while Sys.opaque_identity true do
    r := !r +. 1.
  done;
  !r +. 42.

let _ = (f[@inline]) ()

