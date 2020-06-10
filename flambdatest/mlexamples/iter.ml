
let phys_equal = ( == )

module Header : sig
  type t
  val with_iteration_2 : t -> 'a -> 'b -> ('a -> 'b -> 'c) -> 'c
end = struct
  type t = unit
  let with_iteration_2 _ _ _ _ = Sys.opaque_identity (assert false)
end

module Elt : sig
  type 'a t
  val header : 'a t -> Header.t
  val value : 'a t -> 'a
  val next : 'a t -> 'a t
end = struct
  type 'a t = unit
  let header _ = Sys.opaque_identity (assert false)
  let value _ = Sys.opaque_identity (assert false)
  let next _ = Sys.opaque_identity (assert false)
end


type 'a t = 'a Elt.t option ref

let rec iter_loop first f elt =
  f (Elt.value elt);
  let next = Elt.next elt in
  if not (phys_equal next first) then iter_loop first f next
;;

let iter t ~f =
  match !t with
  | None -> ()
  | Some first ->
    Header.with_iteration_2 (Elt.header first) first f (fun first f ->
      iter_loop first f first)

