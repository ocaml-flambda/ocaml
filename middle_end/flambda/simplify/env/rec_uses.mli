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

type t

(** Print to a formatter. *)
val print : Format.formatter -> t -> unit

(** Empty uses *)
val empty : t

val stack_cont : Continuation.t -> Variable.t list -> t -> t

val unstack_cont : Continuation.t -> t -> t

val add_used_in_current_handler : Name_occurrences.t -> t -> t

val add_apply_cont_args : Continuation.t -> Name_occurrences.t list -> t -> t


