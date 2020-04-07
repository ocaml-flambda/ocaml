(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Union-find-like structure for keeping track of equivalence classes,
    used for alias resolution in the typing environment, with support for
    associating orderings to aliases of canonical elements. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (E : sig
  (* CR mshinwell: Add _intf.ml file. *)
  type t
  include Identifiable.S with type t := t

end) : sig
  type t

  val print : Format.formatter -> t -> unit

  val invariant : t -> unit

  val empty : t

  type add_result = private {
    t : t;
    canonical_element : E.t;
    alias_of : E.t;
  }

  val add
     : t
    -> E.t
    -> Binding_time.With_name_mode.t
    -> E.t
    -> Binding_time.With_name_mode.t
    -> add_result

  (** [get_canonical_element] returns [None] only when the
      [min_order_within_equiv_class] cannot be satisfied. *)
  val get_canonical_element_exn
     : t
    -> E.t
    -> Name_mode.t
    -> min_name_mode:Name_mode.t
    -> E.t

  (** [get_aliases] always returns the supplied element in the result set. *)
  val get_aliases : t -> E.t -> E.Set.t
end
