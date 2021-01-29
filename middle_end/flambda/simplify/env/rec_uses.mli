(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Chambart and Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2021--2021 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Rec uses

    This modules aims at tracking use of mainly variables
    (maybe other things later on ?), with the aim of:
    - removing unused parameters of *recursive* continuations
    - moving allocations out of the hot path of recursive
      continuations (e.g. the allocation of a float that was unboxed
       by simplify)
*)

type t
(** The type tracking the uses of variables (passed through the downwards
    accumulator.
    This contains a stack to track in which continuation's handlers
    the downwards acc currently is. *)

val print : Format.formatter -> t -> unit
(** Print to a formatter. *)


(* {2 Creation and updates} *)

val empty : t
(** Empty uses *)

val stack_cont : Continuation.t -> Variable.t list -> t -> t
(** Add a new continuation on the stack. Used when entering a
    continuation handler. *)

val unstack_cont : Continuation.t -> t -> t
(** Pop the current top of the stack. Used when exiting the current
    contionuation handler. *)

val add_used_in_current_handler : Name_occurrences.t -> t -> t
(** Add name occurrences used in the body of the current continuation's
    handler, **excluding** uses in apply_cont expressions, which are tracked
    separately. *)

val add_apply_cont_args : Continuation.t -> Name_occurrences.t list -> t -> t
(** Add, for the current continuation handler, uses for an apply cont of the
    given continuation with given arguments occurrences. *)


(* {2 Analysis} *)

val analyze : t -> unit
(** Analyze the rec uses.
    TODO: have a more useful return type. *)


