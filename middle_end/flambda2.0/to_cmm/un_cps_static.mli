(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Translation of statically-allocated constants to Cmm. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import

exception Unreachable_code
(* This exception is raise when code is encountered that is unused
    but hasn't been removed by flambda2. This typically happens for
    code of functions for which newer version have been generated and
    used instead, but that flambda2 didn't have the needed precision to
    remove in its one pass. Un_cps can detect such unused code when it
    finds closure ids with no associated offset.
    If it occurs in a function's body, the the function declaration
    is ignored, i.e. no cmm is generated for that function. *)

val static_const
  : Un_cps_env.t
  -> Un_cps_result.t
  -> params_and_body:(
        Un_cps_env.t
     -> string
     -> Flambda.Function_params_and_body.t
     -> Cmm.fundecl)
  -> Let_symbol.Bound_symbols.t
  -> Static_const.t
  -> Un_cps_env.t * Un_cps_result.t * Cmm.expression option
(** Translate a static constant bound by a let_symbol, and return a
    new rsult containing the translation. Takes a [~params_and_body]
    function as argument to translate a function's body, and thus
    avoid a cyclic dependency between {Un_cps} and this module.
    If the [~params_and_body] function raises [Unreachable_code], then the
    function whose body's translation raise the exception will be ignored
    (i.e. no cmm code will be mitted for the function body). If such a
    function (or rather the function's code symbol) is used in the code,
    this should trigger a linking error because of a symbol not found. *)
