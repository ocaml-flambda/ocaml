(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Typing environment extensions: typing environment levels that are
    surrounded by a binder that captures defined names as existentials. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

include Expr_std.S with type t := t

include Contains_ids.S with type t := t

val fold
   : equation:(Name.t -> Type_grammar.t -> 'a -> 'a)
  -> t
  -> 'a
  -> 'a

val empty : unit -> t

val is_empty : t -> bool

val one_equation : Name.t -> Type_grammar.t -> t

val add_or_replace_equation : t -> Name.t -> Type_grammar.t -> t

val meet : Meet_env.t -> t -> t -> t Or_bottom.t

val meet_using_typing_env : Typing_env.t -> t -> t -> t Or_bottom.t

(* Note: [join] takes a [Meet_env.t] argument on purpose: Typing_env_extensions
   are created for meets, and this function is here for case where a meet
   produces a disjunction, in which case the extensions must be joined together.
*)
val join : Meet_env.t -> t -> t -> t

module With_extra_variables : sig
  type t

  val print : Format.formatter -> t -> unit

  val fold
     : variable:(Variable.t -> Flambda_kind.t -> 'a -> 'a)
    -> equation:(Name.t -> Type_grammar.t -> 'a -> 'a)
    -> t
    -> 'a
    -> 'a

  val empty : unit -> t

  val add_definition
     : t
    -> Variable.t
    -> Flambda_kind.t
    -> Type_grammar.t
    -> t

  val add_or_replace_equation : t -> Name.t -> Type_grammar.t -> t
end
