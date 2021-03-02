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
module Const = Reg_width_things.Const (* TODO: add this in Simplify_import ? *)


(* Typedefs for unboxing decisions *)
(* ******************************* *)

type const_ctor_num =
  | Zero
  | At_least_one

type unboxing_decision =
  | Unique_tag_and_size of {
      tag : Tag.t;
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


(* Printing *)
(* ******** *)

let print_decision ppf = function
  | Do_not_unbox -> Format.fprintf ppf "do_not_unbox"
  | Unbox _dec -> Format.fprintf ppf "unbox"

let print ppf l =
  let pp_sep = Format.pp_print_space in
  let aux ppf (param, decision) =
    Format.fprintf ppf "@[<hov 1>(%a@ %a)@]"
      KP.print param print_decision decision
  in
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Format.pp_print_list ~pp_sep aux) l


(* Helpers *)
(* ******* *)

module Float = struct

  type unboxed_canonical_alias =
    | Unreachable
    | Found of Simple.t
    | Not_found of {
        typing_env : TE.t;
        arg_type_at_use : T.t;
      }

  let get_unboxed_canonical_alias arg_type_at_use typing_env_at_use =
    let field_var = Variable.create "field_at_use" in
    let field_simple = Simple.var field_var in
    let field_name =
      Name_in_binding_pos.create (Name.var field_var) Name_mode.in_types
    in
    let field_kind = K.naked_float in
    let shape = T.boxed_float_alias_to ~naked_float:field_var in
    let tenv = TE.add_definition typing_env_at_use field_name field_kind in
    match T.meet tenv arg_type_at_use shape with
    | Bottom ->
      (* Here, the Bottom indicates that the code path ending with the apply_cont
         cannot be actually reached, i.e. the apply_cont (and the branch) is dead
         code.
         For now we generate a correct dummy value, which we know will not be used,
         so it's mainly here to make the kind checker happy. *)
      Unreachable
    | Ok (_, env_extension) ->
      let tenv = TE.add_env_extension tenv env_extension in
      begin match TE.get_canonical_simple_exn tenv field_simple
                    ~min_name_mode:Name_mode.normal with
      | simple ->
        assert (not (Simple.equal simple field_simple));
        Found simple
      | exception Not_found ->
        Not_found { typing_env = tenv; arg_type_at_use; }
      end

  let extra_arg_of_canonical_alias = function
    | Found simple ->
      EPA.Extra_arg.Already_in_scope simple
    | Unreachable ->
      EPA.Extra_arg.Already_in_scope (
        Simple.const (Const.naked_float Numbers.Float_by_bit_pattern.zero)
      )
    | Not_found { typing_env; arg_type_at_use; } ->
      let var = Variable.create "field_at_use" in
      begin match TE.get_alias_then_canonical_simple_exn typing_env
                    ~min_name_mode:Name_mode.normal arg_type_at_use with
      | simple ->
        let prim = Flambda_primitive.(Unary (Unbox_number Naked_float, simple)) in
        EPA.Extra_arg.New_let_binding (var, prim)
      | exception Not_found ->
        Misc.fatal_errorf "No canonical alias was found for the initial \
                           argument when unboxing it"
      end
end

(* Unfold a type into a decision tree *)
(* ********************************** *)

let max_unboxing_depth = 2

let make_optimist_decision ~depth tenv param_type : decision =
  if depth > max_unboxing_depth then Do_not_unbox
  else
    match T.prove_unique_tag_and_size tenv param_type with
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
      let tenv =
        List.fold_left (fun acc var ->
          let name = Name_in_binding_pos.create (Name.var var) Name_mode.normal in
          TE.add_definition acc name field_kind
        ) tenv field_vars
      in
      let field_types = List.map type_of_var field_vars in
      let shape =
        Flambda_type.immutable_block ~is_unique:false tag
          ~field_kind ~fields:field_types
      in
      let env_extension =
        match T.meet tenv param_type shape with
        | Ok (_, env_extension) -> env_extension
        | Bottom ->
          Misc.fatal_errorf "Meet failed whereas prove previously succeeded"
      in
      let tenv = TE.add_env_extension tenv env_extension in
      let fields =
        List.map2 (fun var var_type ->
          var, make_optimist_decision ~depth:(depth + 1) tenv var_type
        ) field_vars field_types
      in
      Unbox (Unique_tag_and_size { tag; fields; })
    | Wrong_kind | Invalid | Unknown ->
      begin match T.prove_is_a_boxed_float tenv param_type with
      | Proved () ->
        let naked_float = Variable.create "unboxed_float" in
        Unbox (Number (K.Naked_number_kind.Naked_float, naked_float))
      | Wrong_kind | Invalid | Unknown -> Do_not_unbox
      end


(* Decision tree -> actual typing env *)
(* ********************************** *)

let rec denv_of_decision denv param_var decision param_type : DE.t =
  match decision with
  | Do_not_unbox -> denv
  | Unbox Unique_tag_and_size { tag; fields; } ->
    let field_kind =
      if Tag.equal tag Tag.double_array_tag then K.naked_float else K.value
    in
    let denv =
      List.fold_left (fun acc (var, _) ->
        let v = Var_in_binding_pos.create var Name_mode.normal in
        DE.define_variable acc v field_kind
      ) denv fields
    in
    let type_of_var (v, _) = Flambda_type.alias_type_of field_kind (Simple.var v) in
    let field_types = List.map type_of_var fields in
    let shape =
      Flambda_type.immutable_block ~is_unique:false tag
        ~field_kind ~fields:field_types
    in
    let env_extension =
      match T.meet (DE.typing_env denv) param_type shape with
      | Ok (ty, env_extension) ->
        TEE.add_or_replace_equation env_extension (Name.var param_var) ty
      | Bottom ->
        Misc.fatal_errorf "Meet failed whereas prove previously succeeded"
    in
    let denv = DE.extend_typing_environment denv env_extension in
    List.fold_left2 (fun acc field_type (field_var, field_decision) ->
      denv_of_decision acc field_var field_decision field_type
    ) denv field_types fields
  | Unbox Number (Naked_float, naked_float) ->
    let naked_float_name =
      Var_in_binding_pos.create naked_float Name_mode.normal
    in
    let shape = T.boxed_float_alias_to ~naked_float in
    let naked_float_kind = K.naked_float in
    let denv = DE.define_variable denv naked_float_name naked_float_kind in
    let env_extension =
      match T.meet (DE.typing_env denv) param_type shape with
      | Ok (ty, env_extension) ->
        TEE.add_or_replace_equation env_extension (Name.var param_var) ty
      | Bottom ->
        Misc.fatal_errorf "Meet failed whereas prove previously succeeded"
    in
    DE.extend_typing_environment denv env_extension
  | _ -> assert false



(*
let make_decision denv ~arg_type_by_use_id ~depth param param_type =

      begin try
        let new_arg_types_by_use_id =

        Apply_cont_rewrite_id.Map.map (fun _ (typing_env_at_use, arg_type_at_use) ->
          if meet_succeds then
            ()
          else raise Exit
          )
        in






    | Wrong_kind | Invalid | Unknown ->
      begin match T.prove_is_a_boxed_float (DE.typing_env denv) param_type with
      | Proved () ->
        let unboxed_version_available_at_all_call_sites =
          Apply_cont_rewrite_id.Map.for_all (
            fun _ (typing_env_at_use, arg_type_at_use) ->
              match Float.get_unboxed_canonical_alias arg_type_at_use typing_env_at_use with
              | Found _ | Unreachable -> true
              | Not_found _ -> false
          ) arg_type_by_use_id
        in
        if not unboxed_version_available_at_all_call_sites then begin
          denv, Do_not_unbox
        end else begin
        end
      | Wrong_kind | Invalid | Unknown -> denv, Do_not_unbox
      end
*)

(* Decision making *)
(* *************** *)

let make_decisions
      ~(arg_types_by_use_id : (TE.t * T.t) Apply_cont_rewrite_id.Map.t list)
      denv params params_types : DE.t * decisions =
  let denv, rev_decisions =
    List.fold_left (fun (denv, rev_decisions) (param, (param_type, _arg_type_by_use_id)) ->
      let optimist = make_optimist_decision ~depth:0 (DE.typing_env denv) param_type in
      let denv = denv_of_decision denv (KP.var param) optimist param_type in
      (denv, optimist :: rev_decisions)
    ) (denv, []) (List.combine params (List.combine params_types arg_types_by_use_id))
  in
  let decisions = List.combine params (List.rev rev_decisions) in
  denv, decisions


(* Compute the extra args *)
(* ********************** *)

let compute_extra_params_and_args_for_one_decision
      arg_type_by_use_id (_param, decision) :
  (KP.t * EPA.Extra_arg.t Apply_cont_rewrite_id.Map.t) list =
  match decision with
  | Do_not_unbox -> []
  | Unbox Unique_tag_and_size { tag = _; fields = _; } ->
    assert false
  | Unbox Number (Naked_float, unboxed_float) ->
    let extra_param = KP.create unboxed_float Flambda_kind.With_subkind.naked_float in
    let extra_args =
      Apply_cont_rewrite_id.Map.map (fun (typing_env_at_use, arg_type_at_use) ->
        Float.get_unboxed_canonical_alias arg_type_at_use typing_env_at_use
        |> Float.extra_arg_of_canonical_alias
      ) arg_type_by_use_id
    in
    [extra_param, extra_args]
  | _ ->
    assert false

let compute_extra_params_and_args
  ~arg_types_by_use_id decisions existing_extra_params_and_args =
  let extras =
    List.map2 compute_extra_params_and_args_for_one_decision
      arg_types_by_use_id decisions
  in
  List.fold_left (
    List.fold_left (fun acc (extra_param, extra_args) ->
      Continuation_extra_params_and_args.add acc ~extra_param ~extra_args
    )) existing_extra_params_and_args extras

