(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

type const_ctor_num =
  | Zero
  | At_least_one

type unboxing_decision =
  | Unique_tag_and_size of {
      tag : Tag.Scannable.t;
      fields : block_fields;
    }
  | Variant of {
      constant_constructors : const_ctor_num;
      fields_by_tag : block_fields Tag.Scannable.Map.t;
    }
  | Closure_single_entry (* of { TODO } *)
  | Number of Flambda_kind.Naked_number_kind.t * Variable.t

and block_fields = (Variable.t * decision) list

and decision =
  | Do_not_unbox
  | Unbox of unboxing_decision


type decisions = (KP.t * decision) list



let max_unboxing_depth = 2

let rec make_decision denv ~depth param param_type =
  if depth > max_unboxing_depth then
    denv, Do_not_unbox
  else
    match T.prove_unique_tag_and_size (DE.typing_env denv) param_type with
    | Proved (tag, size) ->
      let field_kind, field_base_name =
        if Tag.equal tag Tag.double_array_tag
        then K.naked_float, "unboxed_float_field"
        else K.value, "unboxed_field"
      in
      let field_vars =
        List.init (Targetint.OCaml.to_int size)
          (fun i -> Variable.create (Printf.sprintf "%s_%d" field_base_name i))
      in
      let type_of_var v = Flambda_type.alias_type_of field_kind (Simple.var v) in
      let shape =
        Flambda_type.immutable_block ~is_unique:false tag ~field_kind
          ~fields:(List.map type_of_var field_vars)
      in
      assert false (* TODO *)
    | _ ->
      assert false (* TODO *)



