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

(* TODO remove this *)
[@@@ocaml.warning "-26-27"]

open! Simplify_import
module Const = Reg_width_things.Const (* TODO: add this in Simplify_import ? *)


(* Typedefs for unboxing decisions *)
(* ******************************* *)

type unboxing_decision =
  | Unique_tag_and_size of {
      tag : Tag.t;
      fields : block_fields;
    }
  | Variant of {
      tag : Variable.t;
      constant_constructors : const_ctors;
      fields_by_tag : block_fields Tag.Scannable.Map.t;
    }
  | Closure_single_entry (* of { TODO } *)
  | Number of Flambda_kind.Naked_number_kind.t * Variable.t

and block_fields = (Variable.t * decision) list

and const_ctors =
  | Zero
  | At_least_one of {
      is_int : Variable.t;
      ctor : decision;
    }

and decision =
  | Do_not_unbox
  | Unbox of unboxing_decision

type decisions = (KP.t * decision) list


(* Printing *)
(* ******** *)

let rec print_decision ppf = function
  | Do_not_unbox -> Format.fprintf ppf "do_not_unbox"
  | Unbox Unique_tag_and_size { tag; fields; } ->
    Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(tag@ %a)@]@ \
      @[<hov 1>(fields@ %a)@]\
      )@]"
      Tag.print tag
      print_block_fields fields
  | Unbox Variant { tag; constant_constructors; fields_by_tag; } ->
    Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(tag@ %a)@]@ \
      @[<hov 1>(constant_constructors@ %a)@]@ \
      @[<hov 1>(fields_by_tag@ %a)@]\
      )@]"
      Variable.print tag
      print_const_ctor_num constant_constructors
      (Tag.Scannable.Map.print print_block_fields) fields_by_tag
  | Unbox Closure_single_entry ->
    Format.fprintf ppf "@[<hov 1>(closure_single_entry)@]"
  | Unbox Number (kind, var) ->
    Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(var@ %a)@]@ \
      @[<hov 1>(kind@ %a)@]\
    )@]"
    Variable.print var
    Flambda_kind.Naked_number_kind.print kind

and print_var_decision ppf (var, decision) =
    Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(var@ %a)@]@ \
      @[<hov 1>(decision@ %a)@]\
      )@]"
      Variable.print var
      print_decision decision

and print_block_fields ppf l =
  let pp_sep = Format.pp_print_space in
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Format.pp_print_list ~pp_sep print_var_decision) l

and print_const_ctor_num ppf = function
  | Zero -> Format.fprintf ppf "zero"
  | At_least_one { is_int; ctor; } ->
    Format.fprintf ppf "@[<hov 1>(at_least_one@ \
      @[<hov 1>(is_int@ %a)@]@ \
      @[<hov 1>(ctor@ %a)@]\
      )@]"
      Variable.print is_int
      print_decision ctor


let print ppf l =
  let pp_sep = Format.pp_print_space in
  let aux ppf (param, decision) =
    Format.fprintf ppf "@[<hov 1>(%a@ %a)@]"
      KP.print param print_decision decision
  in
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Format.pp_print_list ~pp_sep aux) l



(* Unboxers *)
(* ******** *)

type number_decider = {
  param_name : string;
  kind : Flambda_kind.Naked_number_kind.t;
  prove_is_a_boxed_number : TE.t -> T.t -> unit T.proof_allowing_kind_mismatch;
}

type unboxer = {
  var_name : string;
  invalid_const : Const.t;
  unboxing_prim : Simple.t -> Flambda_primitive.t;
  prove_simple :
    TE.t -> min_name_mode:Name_mode.t -> T.t -> Simple.t T.proof;
}

module Immediate = struct

  let decider = {
    param_name = "naked_immediate";
    kind = K.Naked_number_kind.Naked_immediate;
    prove_is_a_boxed_number = T.prove_is_a_tagged_immediate;
  }

  let unboxing_prim simple =
    Flambda_primitive.(Unary (Unbox_number Untagged_immediate, simple))

  let unboxer = {
    var_name = "naked_immediate";
    invalid_const = Const.naked_immediate Target_imm.zero;
    unboxing_prim;
    prove_simple = T.prove_untagged_int_simple;
  }

end

module Float = struct

  let decider = {
    param_name = "unboxed_float";
    kind = K.Naked_number_kind.Naked_float;
    prove_is_a_boxed_number = T.prove_is_a_boxed_float;
  }

  let unboxing_prim simple =
    Flambda_primitive.(Unary (Unbox_number Naked_float, simple))

  let unboxer = {
    var_name = "unboxed_float";
    invalid_const = Const.naked_float Numbers.Float_by_bit_pattern.zero;
    unboxing_prim;
    prove_simple = T.prove_unboxed_float_simple;
  }

end

module Int32 = struct

  let decider = {
    param_name = "unboxed_int32";
    kind = K.Naked_number_kind.Naked_int32;
    prove_is_a_boxed_number = T.prove_is_a_boxed_int32;
  }

  let unboxing_prim simple =
    Flambda_primitive.(Unary (Unbox_number Naked_int32, simple))

  let unboxer = {
    var_name = "unboxed_int32";
    invalid_const = Const.naked_int32 Int32.zero;
    unboxing_prim;
    prove_simple = T.prove_unboxed_int32_simple;
  }

end

module Int64 = struct

  let decider = {
    param_name = "unboxed_int64";
    kind = K.Naked_number_kind.Naked_int64;
    prove_is_a_boxed_number = T.prove_is_a_boxed_int64;
  }

  let unboxing_prim simple =
    Flambda_primitive.(Unary (Unbox_number Naked_int64, simple))

  let unboxer = {
    var_name = "unboxed_int64";
    invalid_const = Const.naked_int64 Int64.zero;
    unboxing_prim;
    prove_simple = T.prove_unboxed_int64_simple;
  }

end

module Nativeint = struct

  let decider = {
    param_name = "unboxed_nativeint";
    kind = K.Naked_number_kind.Naked_nativeint;
    prove_is_a_boxed_number = T.prove_is_a_boxed_nativeint;
  }

  let unboxing_prim simple =
    Flambda_primitive.(Unary (Unbox_number Naked_nativeint, simple))

  let unboxer = {
    var_name = "unboxed_nativeint";
    invalid_const = Const.naked_nativeint Targetint.zero;
    unboxing_prim;
    prove_simple = T.prove_unboxed_nativeint_simple;
  }

end

module Field = struct

  let unboxing_prim bak field_nth block_simple =
    let field_const = Simple.const (Const.tagged_immediate field_nth) in
    Flambda_primitive.(
      Binary (Block_load (bak, Immutable), block_simple, field_const)
    )

  let unboxer bak field_nth = {
    var_name = "field_at_use";
    invalid_const = Const.const_zero;
    unboxing_prim = unboxing_prim bak field_nth;
    prove_simple = (fun tenv ~min_name_mode t ->
      T.prove_block_field_simple tenv ~min_name_mode t field_nth);
  }

end

(* Unfold a type into a decision tree *)
(* ********************************** *)

let max_unboxing_depth = 42

let make_optimistic_const_ctor () =
  let is_int = Variable.create "is_int" in
  let unboxed_const_ctor = Variable.create "unboxed_const_ctor" in
  let decision = Unbox (Number (Naked_immediate, unboxed_const_ctor)) in
  let ctor = decision in
  At_least_one { is_int; ctor; }

let make_optimistic_number_decision tenv param_type decider : decision option =
  match decider.prove_is_a_boxed_number tenv param_type with
  | Proved () ->
    let naked_number = Variable.create decider.param_name in
    Some (Unbox (Number (decider.kind, naked_number)))
  | Wrong_kind | Invalid | Unknown ->
    None

let decide tenv param_type deciders : decision option =
  List.find_map (make_optimistic_number_decision tenv param_type) deciders

let deciders = [
  Immediate.decider;
  Float.decider;
  Int32.decider;
  Int64.decider;
  Nativeint.decider;
]

let rec make_optimist_decision ~depth tenv param_type : decision =
  match decide tenv param_type deciders with
  | Some decision -> decision
  | None ->
    if depth > max_unboxing_depth then Do_not_unbox
    else match T.prove_unique_tag_and_size tenv param_type with
      | Proved (tag, size) ->
        let fields = make_optimistic_fields ~depth tenv param_type tag size in
        Unbox (Unique_tag_and_size { tag; fields; })
      | Wrong_kind | Invalid | Unknown ->
        match T.prove_variant_like tenv param_type with
        | Proved { const_ctors; non_const_ctors_with_sizes; } ->
          let tag = Variable.create "tag" in
          let constant_constructors =
            match const_ctors with
            | Unknown -> make_optimistic_const_ctor ()
            | Known set ->
              if Target_imm.Set.is_empty set then Zero
              else make_optimistic_const_ctor ()
          in
          let fields_by_tag =
            Tag.Scannable.Map.mapi (fun scannable_tag size ->
              let tag = Tag.Scannable.to_tag scannable_tag in
              make_optimistic_fields ~depth tenv param_type tag size
            ) non_const_ctors_with_sizes
          in
          Unbox (Variant { tag; constant_constructors; fields_by_tag; })
        | Wrong_kind | Invalid | Unknown -> Do_not_unbox

and make_optimistic_fields ~depth tenv param_type (tag : Tag.t) size : block_fields =
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
  fields


(* Decision tree -> actual typing env *)
(* ********************************** *)

let add_equation_on_var denv var shape =
  let kind = T.kind shape in
  let var_type = T.alias_type_of kind (Simple.var var) in
  match T.meet (DE.typing_env denv) var_type shape with
  | Ok (_ty, env_extension) ->
    DE.extend_typing_environment denv env_extension
  | Bottom ->
    Misc.fatal_errorf "Meet failed whereas prove previously succeeded"

let denv_of_number_decision naked_kind shape param_var naked_var denv : DE.t =
  let naked_name =
    Var_in_binding_pos.create naked_var Name_mode.normal
  in
  let denv = DE.define_variable denv naked_name naked_kind in
  add_equation_on_var denv param_var shape

let rec denv_of_decision denv param_var decision : DE.t =
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
    let denv = add_equation_on_var denv param_var shape in
    List.fold_left (fun acc (field_var, field_decision) ->
      denv_of_decision acc field_var field_decision
    ) denv fields
  | Unbox Variant { tag; constant_constructors; fields_by_tag; } ->
    (* Adapt the denv for the tag *)
    let tag_v = Var_in_binding_pos.create tag Name_mode.normal in
    let denv = DE.define_variable denv tag_v K.naked_immediate in
    let denv =
      DE.add_equation_on_variable denv tag
        (T.get_tag_for_block ~block:(Simple.var param_var))
    in
    let get_tag_prim =
      P.Eligible_for_cse.create_exn (Unary (Get_tag, Simple.var param_var))
    in
    let denv = DE.add_cse denv get_tag_prim ~bound_to:(Simple.var tag) in

    (* Same thing for is_int *)
    let denv =
      match constant_constructors with
      | Zero -> denv
      | At_least_one { is_int; _ } ->
        let is_int_v = Var_in_binding_pos.create is_int Name_mode.normal in
        let denv = DE.define_variable denv is_int_v K.naked_immediate in
        let denv =
          DE.add_equation_on_variable denv is_int
            (T.is_int_for_scrutinee ~scrutinee:(Simple.var param_var))
        in
        let is_int_prim =
          P.Eligible_for_cse.create_exn (Unary (Is_int, Simple.var param_var))
        in
        let denv = DE.add_cse denv is_int_prim ~bound_to:(Simple.var is_int) in
        denv
    in

    (* next *)
    let denv =
      match constant_constructors with
      | Zero -> denv
      | At_least_one { ctor = Do_not_unbox; _ } -> denv
      | At_least_one { ctor = Unbox Number (Naked_immediate, ctor_var); _ } ->
        let v = Var_in_binding_pos.create ctor_var Name_mode.normal in
        let denv = DE.define_variable denv v K.value in
        ignore denv;
        assert false
      | At_least_one { ctor = _; _ } ->
        assert false
    in
    let const_ctors = assert false in
    let denv =
      Tag.Scannable.Map.fold (fun _ block_fields denv ->
        List.fold_left (fun acc (var, _) ->
          let v = Var_in_binding_pos.create var Name_mode.normal in
          DE.define_variable acc v K.value
        ) denv block_fields
      ) fields_by_tag denv
    in
    let non_const_ctors : T.t list Tag.Scannable.Map.t =
      Tag.Scannable.Map.map (fun block_fields ->
        let field_kind = K.value in
        let type_of_var (v, _) =
          Flambda_type.alias_type_of field_kind (Simple.var v)
        in
        List.map type_of_var block_fields
      ) fields_by_tag
    in
    let shape = T.variant ~const_ctors ~non_const_ctors in
    let denv = add_equation_on_var denv param_var shape in

    (* recursion *)
    let denv =
      match constant_constructors with
      | Zero -> denv
      | At_least_one { ctor = _; _ } ->
        assert false
    in
    Tag.Scannable.Map.fold (fun _ block_fields denv ->
      List.fold_left (fun denv (var, decision) ->
        denv_of_decision denv var decision
      ) denv block_fields
    ) fields_by_tag denv
  | Unbox Number (Naked_immediate, naked_immediate) ->
    let shape = T.tagged_immediate_alias_to ~naked_immediate in
    denv_of_number_decision K.naked_immediate shape
      param_var naked_immediate denv
  | Unbox Number (Naked_float, naked_float) ->
    let shape = T.boxed_float_alias_to ~naked_float in
    denv_of_number_decision K.naked_float shape
      param_var naked_float denv
  | Unbox Number (Naked_int32, naked_int32) ->
    let shape = T.boxed_int32_alias_to ~naked_int32 in
    denv_of_number_decision K.naked_int32 shape
      param_var naked_int32 denv
  | Unbox Number (Naked_int64, naked_int64) ->
    let shape = T.boxed_int64_alias_to ~naked_int64 in
    denv_of_number_decision K.naked_int64 shape
      param_var naked_int64 denv
  | Unbox Number (Naked_nativeint, naked_nativeint) ->
    let shape = T.boxed_nativeint_alias_to ~naked_nativeint in
    denv_of_number_decision K.naked_nativeint shape
      param_var naked_nativeint denv
  | _ -> assert false


(* Decision making *)
(* *************** *)

let make_decisions
      ~continuation_is_recursive:_
      ~(arg_types_by_use_id : (TE.t * T.t) Apply_cont_rewrite_id.Map.t list)
      denv params params_types : DE.t * decisions =
  let denv, rev_decisions =
    List.fold_left (fun (denv, rev_decisions) (param, (param_type, _arg_type_by_use_id)) ->
      let optimist = make_optimist_decision ~depth:0 (DE.typing_env denv) param_type in
      let denv = denv_of_decision denv (KP.var param) optimist in
      (denv, optimist :: rev_decisions)
    ) (denv, []) (List.combine params (List.combine params_types arg_types_by_use_id))
  in
  let decisions = List.combine params (List.rev rev_decisions) in
  denv, decisions


(* Compute the extra args *)
(* ********************** *)

type unboxed_arg =
  | Available of Simple.t
  | Generated of Variable.t
  | Added_by_wrapper_at_rewrite_use of { nth_arg : int; }

let arg_being_unboxed_of_extra_arg extra_arg =
  match (extra_arg : EPA.Extra_arg.t) with
  | Already_in_scope simple -> Available simple
  | New_let_binding (var, _)
  | New_let_binding_with_named_args (var, _) -> Generated var

let extra_arg_of_arg_being_unboxed unboxer typing_env_at_use arg_being_unboxed =
  match arg_being_unboxed with
  | Available arg_at_use ->
    let arg_type = T.alias_type_of K.value arg_at_use in
    begin match unboxer.prove_simple typing_env_at_use arg_type
                  ~min_name_mode:Name_mode.normal with
    | Proved simple ->
      EPA.Extra_arg.Already_in_scope simple
    | Invalid ->
      EPA.Extra_arg.Already_in_scope (Simple.const unboxer.invalid_const)
    | Unknown ->
      let var = Variable.create unboxer.var_name in
      let prim = unboxer.unboxing_prim arg_at_use in
      EPA.Extra_arg.New_let_binding (var, prim)
    end
  | Generated var ->
    let arg_at_use = Simple.var var in
    let var = Variable.create unboxer.var_name in
    let prim = unboxer.unboxing_prim arg_at_use in
    EPA.Extra_arg.New_let_binding (var, prim)
  | Added_by_wrapper_at_rewrite_use { nth_arg; } ->
    let var = Variable.create "unboxed_field" in
    EPA.Extra_arg.New_let_binding_with_named_args (var, (fun args ->
      let arg_simple = List.nth args nth_arg in
      unboxer.unboxing_prim arg_simple
    ))


let rec compute_extra_args_for_one_decision_and_use
          extra_args typing_env_at_use arg_being_unboxed decision :
  EPA.Extra_arg.t list =
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
        let unboxer = Field.unboxer bak field_nth in
        let new_extra_arg =
          extra_arg_of_arg_being_unboxed unboxer
            typing_env_at_use arg_being_unboxed
        in
        let extra_args =
          compute_extra_args_for_one_decision_and_use
            (new_extra_arg :: extra_args) typing_env_at_use
            (arg_being_unboxed_of_extra_arg new_extra_arg) field_decision
        in
        (extra_args, Target_imm.(add one field_nth))
      ) (extra_args, Target_imm.zero)  fields
    in
    extra_args
  | Unbox Variant { tag; constant_constructors; fields_by_tag; } ->
    let simple_being_unboxed =
      match arg_being_unboxed with
      | Available simple -> simple
      | Generated var -> Simple.var var
      | Added_by_wrapper_at_rewrite_use _ ->
        Misc.fatal_errorf
          "Should have been filtered out earlier during filter_decision."
    in
    let arg_type = T.alias_type_of K.value simple_being_unboxed in
    begin match T.prove_variant_like typing_env_at_use arg_type with
    | Wrong_kind ->
      Misc.fatal_errorf "Kind error while unboxing a variant"
    | Unknown ->
      Misc.fatal_errorf "This case should have been filtered out before: \
                         in the recursive case because uinboxing variants is forbidden, or \
                         in the non-recursive case, the join of the types at use should have \
                         prevent unboxing of the parameter."
    | Invalid ->
      (* Invalid here means that the apply_cont is unreachable, i.e. the args
         we generated will never be actually used at runtime, so the values of
         the args do not matter, they are here to make the kind checker happy. *)
      let mk x = EPA.Extra_arg.Already_in_scope x in
      let tag_extra_arg = mk Simple.untagged_const_zero in
      let const_ctor_extra_args =
        match constant_constructors with
        | Zero -> []
        | At_least_one { is_int = _; ctor = ctor_decision; } ->
          let ctor_var = assert false in
          let is_int_extra_arg = mk Simple.untagged_const_zero in
          let ctor_extra_simple = Simple.const_zero in
          let ctor_extra_arg = mk ctor_extra_simple in
          let unboxed_ctor_extra_args =
            compute_extra_args_for_one_decision_and_use []
              typing_env_at_use (Available ctor_extra_simple) ctor_decision
          in
          is_int_extra_arg :: ctor_extra_arg :: unboxed_ctor_extra_args
      in
      let extra_args =
        Tag.Scannable.Map.fold (fun _ block_fields extra_args ->
          List.fold_left (fun extra_args (_, field_decision) ->
            let new_extra_arg = mk Simple.const_zero in
            compute_extra_args_for_one_decision_and_use
              (new_extra_arg :: extra_args) typing_env_at_use
              (arg_being_unboxed_of_extra_arg new_extra_arg) field_decision
          ) extra_args block_fields
        ) fields_by_tag extra_args
      in
      tag_extra_arg :: (const_ctor_extra_args @ extra_args)
    | Proved { const_ctors; non_const_ctors_with_sizes; } ->
      let mk x = EPA.Extra_arg.Already_in_scope x in
      let are_there_constant_constructors =
        match const_ctors with
        | Unknown -> true
        | Known set -> not (Target_imm.Set.is_empty set)
      in
      let are_there_non_constant_constructors =
        not (Tag.Scannable.Map.is_empty non_const_ctors_with_sizes)
      in
      begin match are_there_constant_constructors,
                  are_there_non_constant_constructors with
      | false, false ->
        Misc.fatal_errorf "This should have been Invalid and not Proved"
      | true, true ->
        Misc.fatal_errorf "This should have been filtered out during filter_decisions."
      | true, false ->
        let tag_extra_arg = mk Simple.untagged_const_zero in
        let const_ctor_extra_args =
          match constant_constructors with
          | Zero -> assert false
          | At_least_one { is_int = _; ctor = ctor_decision; } ->
            let is_int_extra_arg = mk Simple.untagged_const_true in
            let const_ctor_extra_arg = mk simple_being_unboxed in
            let unboxed_const_ctor_extra_args =
              compute_extra_args_for_one_decision_and_use []
              typing_env_at_use (Available simple_being_unboxed) ctor_decision
            in
            is_int_extra_arg :: const_ctor_extra_arg :: unboxed_const_ctor_extra_args
        in
        let extra_args =
          Tag.Scannable.Map.fold (fun _ block_fields extra_args ->
            List.fold_left (fun extra_args (_, field_decision) ->
              let new_extra_arg = mk Simple.const_zero in
              compute_extra_args_for_one_decision_and_use
                (new_extra_arg :: extra_args) typing_env_at_use
                (arg_being_unboxed_of_extra_arg new_extra_arg) field_decision
            ) extra_args block_fields
          ) fields_by_tag extra_args
        in
        tag_extra_arg :: (const_ctor_extra_args @ extra_args)
      | false, true ->
        let const_ctor_extra_args =
          match constant_constructors with
          | Zero -> []
          | At_least_one { is_int = _; ctor = ctor_decision; } ->
            let ctor_var = assert false in
            let is_int_extra_arg = mk Simple.untagged_const_zero in
            let ctor_extra_simple = Simple.const_zero in
            let ctor_extra_arg = mk ctor_extra_simple in
            let unboxed_ctor_extra_args =
              compute_extra_args_for_one_decision_and_use []
                typing_env_at_use (Available ctor_extra_simple) ctor_decision
            in
            is_int_extra_arg :: ctor_extra_arg :: unboxed_ctor_extra_args
        in
        let tag_at_use_site =
          match Tag.Scannable.Map.get_singleton non_const_ctors_with_sizes with
          | None ->
            Misc.fatal_errorf "This case should have been filtered out by filter_decisions"
          | Some (tag, _) -> tag
        in
        let tag_extra_arg =
          tag_at_use_site
          |> Tag.Scannable.to_targetint
          |> Targetint.OCaml.of_targetint
          |> Const.untagged_const_int
          |> Simple.const
          |> mk
        in
        let extra_args =
          Tag.Scannable.Map.fold (fun tag_decision block_fields extra_args ->
            let size = List.length block_fields in
            let bak : Flambda_primitive.Block_access_kind.t =
              Values {
                size = Known (Targetint.OCaml.of_int size);
                tag = tag_decision;
                field_kind = Any_value;
              }
            in
            let extra_args, _ =
              List.fold_left (fun (extra_args, field_nth) (_, field_decision) ->
                if Tag.Scannable.equal tag_at_use_site tag_decision then begin
                  let unboxer = Field.unboxer bak field_nth in
                  let new_extra_arg =
                    extra_arg_of_arg_being_unboxed unboxer
                      typing_env_at_use arg_being_unboxed
                  in
                  let extra_args =
                    compute_extra_args_for_one_decision_and_use
                      (new_extra_arg :: extra_args) typing_env_at_use
                      (arg_being_unboxed_of_extra_arg new_extra_arg) field_decision
                  in
                  (extra_args, Target_imm.(add one field_nth))
                end else begin
                  let new_extra_arg = mk Simple.const_zero in
                  let extra_args =
                    compute_extra_args_for_one_decision_and_use
                      (new_extra_arg :: extra_args) typing_env_at_use
                      (arg_being_unboxed_of_extra_arg new_extra_arg) field_decision
                  in
                  (extra_args, Target_imm.(add one field_nth))
                end
              ) (extra_args, Target_imm.zero) block_fields
            in
            extra_args
          ) fields_by_tag extra_args
        in
        tag_extra_arg :: (const_ctor_extra_args @ extra_args)
      end
    end
  | Unbox Number (Naked_float, _unboxed_float) ->
    let new_extra_arg =
      extra_arg_of_arg_being_unboxed Float.unboxer
        typing_env_at_use arg_being_unboxed
    in
    new_extra_arg :: extra_args
  | Unbox Number (Naked_int32, _unboxed_int32) ->
    let new_extra_arg =
      extra_arg_of_arg_being_unboxed Int32.unboxer
        typing_env_at_use arg_being_unboxed
    in
    new_extra_arg :: extra_args
  | Unbox Number (Naked_int64, _unboxed_int64) ->
    let new_extra_arg =
      extra_arg_of_arg_being_unboxed Int64.unboxer
        typing_env_at_use arg_being_unboxed
    in
    new_extra_arg :: extra_args
  | Unbox Number (Naked_nativeint, _unboxed_nativeint) ->
    let new_extra_arg =
      extra_arg_of_arg_being_unboxed Nativeint.unboxer
        typing_env_at_use arg_being_unboxed
    in
    new_extra_arg :: extra_args
  | Unbox Number (Naked_immediate, _naked_immediate) ->
    let new_extra_arg =
      extra_arg_of_arg_being_unboxed Immediate.unboxer
        typing_env_at_use arg_being_unboxed
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
        aux (extra_param :: extra_params) field_decision
      ) extra_params fields
    | Unbox Variant { tag; constant_constructors; fields_by_tag; } ->
      (* order of params : *)
      let tag_extra_param = KP.create tag K.With_subkind.naked_immediate in
      let cstrs_extra_params =
        match constant_constructors with
        | Zero -> []
        | At_least_one { is_int; ctor = ctor_decision; } ->
          let ctor_var = assert false in
          let is_int_extra_param = KP.create is_int K.With_subkind.naked_immediate in
          let ctor_extra_param = KP.create ctor_var K.With_subkind.any_value in
          let unboxed_ctor_extra_params = aux [] ctor_decision in
          is_int_extra_param :: ctor_extra_param :: unboxed_ctor_extra_params
      in
      let extra_params =
        Tag.Scannable.Map.fold (fun _ block_fields extra_params ->
          List.fold_left (fun extra_params (field_var, field_decision) ->
            let extra_param = KP.create field_var K.With_subkind.any_value in
            aux (extra_param :: extra_params) field_decision
          ) extra_params block_fields
        ) fields_by_tag extra_params
      in
      tag_extra_param :: (cstrs_extra_params @ extra_params)
    | Unbox Number (naked_number_kind, unboxed_number) ->
      (* CR pchambart: This is seriously too contrived for what it's doing.
         Flambda_kind.With_subkind needs a function for doing that *)
      let boxable_number_kind =
        K.Boxable_number.of_naked_number_kind naked_number_kind
      in
      let kind = K.Boxable_number.to_kind boxable_number_kind in
      let kind_with_subkind =
        K.With_subkind.create kind K.With_subkind.Subkind.Anything
      in
      let extra_param = KP.create unboxed_number kind_with_subkind in
      extra_param :: extra_params
    | _ -> assert false
  in
  aux [] decision

let compute_extra_params_and_args_for_one_decision nth_arg arg_type_by_use_id = function
  | Do_not_unbox -> None
  | decision ->
    let extra_params = compute_extra_params decision in
    let extra_args =
      Apply_cont_rewrite_id.Map.map (fun (typing_env_at_use, arg_type_at_use) ->
        match TE.get_alias_then_canonical_simple_exn typing_env_at_use
                ~min_name_mode:Name_mode.normal arg_type_at_use with
        | simple ->
          compute_extra_args_for_one_decision_and_use [] typing_env_at_use
            (Available simple) decision
        | exception Not_found ->
          compute_extra_args_for_one_decision_and_use [] typing_env_at_use
            (Added_by_wrapper_at_rewrite_use { nth_arg; }) decision
      ) arg_type_by_use_id
    in
    Some (extra_params, extra_args)

let compute_extra_params_and_args
      ~arg_types_by_use_id decisions existing_extra_params_and_args =
  let nth = ref ~-1 in
  let extras =
    List.map2 (fun arg_type_by_use_id (_, decision) ->
      incr nth;
      compute_extra_params_and_args_for_one_decision !nth arg_type_by_use_id decision
    ) arg_types_by_use_id decisions
  in
  List.fold_left (fun acc -> function
    | None -> acc
    | Some (extra_params, extra_args) ->
      Continuation_extra_params_and_args.add_list acc ~extra_params ~extra_args
  ) existing_extra_params_and_args extras

