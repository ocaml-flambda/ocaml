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

  let default_simple simple =
    let var = Variable.create "unboxed_float" in
    let prim = Flambda_primitive.(Unary (Unbox_number Naked_float, simple)) in
    EPA.Extra_arg.New_let_binding (var, prim)

  let make_simple_of_proof float_simple p =
    match (p : _ T.proof) with
    | Proved simple ->
      EPA.Extra_arg.Already_in_scope simple
    | Invalid ->
      EPA.Extra_arg.Already_in_scope (
        Simple.const (Const.naked_float Numbers.Float_by_bit_pattern.zero)
      )
    | Unknown ->
      default_simple float_simple

end

module Field = struct

  let default_simple bak block_simple field =
    let var = Variable.create "field_at_use" in
    let field_const = Simple.const (Const.tagged_immediate field) in
    let prim =
      Flambda_primitive.(
        Binary (Block_load (bak, Immutable), block_simple, field_const)
      )
    in
    EPA.Extra_arg.New_let_binding (var, prim)

  let make_simple_of_proof bak block_simple field p =
    match (p : _ T.proof) with
    | Proved simple -> EPA.Extra_arg.Already_in_scope simple
    | Invalid -> EPA.Extra_arg.Already_in_scope (Simple.const Const.const_zero)
    | Unknown -> default_simple bak block_simple field

end

(* Unfold a type into a decision tree *)
(* ********************************** *)

let max_unboxing_depth = 2

let rec make_optimist_decision ~depth tenv param_type : decision =
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

let rec denv_of_decision denv param_var decision : DE.t =
  match decision with
  | Do_not_unbox -> denv
  | Unbox Unique_tag_and_size { tag; fields; } ->
    let param_type = T.alias_type_of K.value (Simple.var param_var) in
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
      | Ok (_ty, env_extension) -> env_extension
      | Bottom ->
        Misc.fatal_errorf "Meet failed whereas prove previously succeeded"
    in
    let denv = DE.extend_typing_environment denv env_extension in
    List.fold_left (fun acc (field_var, field_decision) ->
      denv_of_decision acc field_var field_decision
    ) denv fields
  | Unbox Number (Naked_float, naked_float) ->
    let param_type = T.alias_type_of K.value (Simple.var param_var) in
    let naked_float_name =
      Var_in_binding_pos.create naked_float Name_mode.normal
    in
    let shape = T.boxed_float_alias_to ~naked_float in
    let naked_float_kind = K.naked_float in
    let denv = DE.define_variable denv naked_float_name naked_float_kind in
    let env_extension =
      match T.meet (DE.typing_env denv) param_type shape with
      | Ok (_ty, env_extension) -> env_extension
      | Bottom ->
        Misc.fatal_errorf "Meet failed whereas prove previously succeeded"
    in
    DE.extend_typing_environment denv env_extension
  | _ -> assert false


(* Decision making *)
(* *************** *)

let make_decisions
      ~(arg_types_by_use_id : (TE.t * T.t) Apply_cont_rewrite_id.Map.t list)
      denv params params_types : DE.t * decisions =
  let denv, rev_decisions =
    List.fold_left (fun (denv, rev_decisions) (param, (param_type, _arg_type_by_use_id)) ->
      let optimist = make_optimist_decision ~depth:0 (DE.typing_env denv) param_type in
      let denv = denv_of_decision denv (KP.var param) optimist in
      (denv, optimist :: rev_decisions)
    ) (denv, []) (List.combine params (List.combine params_types arg_types_by_use_id))
  in
  Format.eprintf "typing env after unboxing:@\n%a@." TE.print (DE.typing_env denv);
  let decisions = List.combine params (List.rev rev_decisions) in
  denv, decisions


(* Compute the extra args *)
(* ********************** *)

let rec compute_extra_args_for_one_decision_and_use
      extra_args typing_env_at_use extra_arg_at_use decision =
  match decision with
  | Do_not_unbox -> extra_args
  | Unbox Unique_tag_and_size { tag; fields; } ->
    let size = Or_unknown.Known (Targetint.OCaml.of_int (List.length fields)) in
    let bak : Flambda_primitive.Block_access_kind.t =
      if Tag.equal tag Tag.double_array_tag then
        Naked_floats { size; }
      else
        Values {
          size;
          tag = Option.get (Tag.Scannable.of_tag tag);
          field_kind = Any_value;
        }
    in
    let extra_args, _ =
      List.fold_left (fun (extra_args, field_nth) (_, field_decision) ->
        let new_extra_arg =
          match (extra_arg_at_use : EPA.Extra_arg.t) with
          | Already_in_scope arg_at_use ->
            let arg_type = T.alias_type_of K.value arg_at_use in
            let proof =
              T.prove_block_field_simple typing_env_at_use
                ~min_name_mode:Name_mode.normal arg_type field_nth
            in
            Field.make_simple_of_proof bak arg_at_use field_nth proof
          | New_let_binding (var, _prim) ->
            let arg_at_use = Simple.var var in
            Field.default_simple bak arg_at_use field_nth
        in
        let extra_args =
          compute_extra_args_for_one_decision_and_use
            extra_args typing_env_at_use new_extra_arg field_decision
        in
        (new_extra_arg :: extra_args, Target_imm.(add one field_nth))
      ) (extra_args, Target_imm.zero)  fields
    in
    extra_args
  | Unbox Number (Naked_float, _unboxed_float) ->
    let new_extra_arg =
      match (extra_arg_at_use : EPA.Extra_arg.t) with
      | Already_in_scope arg_at_use ->
        let arg_type = T.alias_type_of K.value arg_at_use in
        let proof =
          T.prove_unboxed_float_simple typing_env_at_use
            ~min_name_mode:Name_mode.normal arg_type
        in
        Float.make_simple_of_proof arg_at_use proof
      | New_let_binding (var, _prim) ->
        let arg_at_use = Simple.var var in
        Float.default_simple arg_at_use
    in
    new_extra_arg :: extra_args
  | _ -> assert false

let compute_extra_params decision =
  let rec aux extra_params = function
    | Do_not_unbox -> extra_params
    | Unbox Unique_tag_and_size { tag; fields; } ->
      List.fold_left (fun extra_params (field_var, field_decision) ->
        let kind =
          if Tag.equal Tag.double_array_tag tag then
            K.With_subkind.naked_float
          else
            K.With_subkind.any_value
        in
        let extra_param = KP.create field_var kind in
        let extra_params = aux extra_params field_decision in
        extra_param :: extra_params
      ) extra_params fields
    | Unbox Number (Naked_float, unboxed_float) ->
      let extra_param = KP.create unboxed_float Flambda_kind.With_subkind.naked_float in
      extra_param :: extra_params
    | _ -> assert false
  in
  aux [] decision

let compute_extra_params_and_args_for_one_decision arg_type_by_use_id = function
  | Do_not_unbox -> None
  | decision ->
    let extra_params = compute_extra_params decision in
    let extra_args =
      Apply_cont_rewrite_id.Map.map (fun (typing_env_at_use, arg_type_at_use) ->
        match TE.get_alias_then_canonical_simple_exn typing_env_at_use
                ~min_name_mode:Name_mode.normal arg_type_at_use with
        | simple ->
          compute_extra_args_for_one_decision_and_use [] typing_env_at_use
            (EPA.Extra_arg.Already_in_scope simple) decision
        | exception Not_found ->
          Format.eprintf "arg_type_at_use: %a@." T.print arg_type_at_use;
          Misc.fatal_errorf "No canonical alias was found for the initial \
                             argument when unboxing it"
      ) arg_type_by_use_id
    in
    Some (extra_params, extra_args)

let compute_extra_params_and_args
  ~arg_types_by_use_id decisions existing_extra_params_and_args =
  let extras =
    List.map2 (fun arg_type_by_use_id (_, decision) ->
      compute_extra_params_and_args_for_one_decision arg_type_by_use_id decision
    ) arg_types_by_use_id decisions
  in
  List.fold_left (fun acc -> function
    | None -> acc
    | Some (extra_params, extra_args) ->
      Continuation_extra_params_and_args.add_list acc ~extra_params ~extra_args
  ) existing_extra_params_and_args extras

