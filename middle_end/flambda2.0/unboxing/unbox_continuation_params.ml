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

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

(* CR mshinwell: Add a command-line flag. *)
let max_unboxing_depth = 1

module type Unboxing_spec = sig
  module Index : sig
    include Identifiable.S
    val to_string : t -> string
  end

  module Info : sig type t end

  val var_name : string

  (* CR mshinwell: rename [fields] *)
  val make_boxed_value : Info.t -> fields:T.t Index.Map.t -> T.t

  val make_boxed_value_accommodating
     : Info.t
    -> Index.t
    -> index_var:Variable.t
    -> T.t

  val project_field : Info.t -> block:Simple.t -> index:Index.t -> P.t
end

module Make (U : Unboxing_spec) = struct
  module Index = U.Index

  let unbox_one_field_of_one_parameter info ~extra_param ~index
        ~arg_types_by_use_id =
    let param_kind = KP.kind extra_param in
    let field_var = Variable.create "field_at_use" in
    let field_name =
      Name_in_binding_pos.create (Name.var field_var) Name_mode.in_types
    in
    let shape =
      U.make_boxed_value_accommodating info index ~index_var:field_var
    in
    (* Don't unbox parameters unless, at all use sites, there is a
       non-irrelevant [Simple] available for the corresponding field of the
       block. *)
    Apply_cont_rewrite_id.Map.fold
      (fun id (typing_env_at_use, arg_type_at_use)
           (extra_args, field_types_by_id) ->
        let env_extension =
          let result_var =
            Var_in_binding_pos.create field_var Name_mode.normal
          in
          T.meet_shape typing_env_at_use arg_type_at_use
            ~shape ~result_var ~result_kind:param_kind
        in
        let field = Simple.var field_var in
        match env_extension with
        | Bottom ->
          let field_types_by_id =
            Apply_cont_rewrite_id.Map.add id
              (typing_env_at_use, T.bottom param_kind)
              field_types_by_id
          in
          None, field_types_by_id
        | Ok env_extension ->
          let typing_env_at_use =
            TE.add_definition typing_env_at_use field_name param_kind
          in
          let typing_env_at_use =
            TE.add_env_extension typing_env_at_use ~env_extension
          in
          let field_type = T.alias_type_of param_kind field in
          let field_types_by_id =
            Apply_cont_rewrite_id.Map.add id
              (typing_env_at_use, field_type)
              field_types_by_id
          in
          (* CR mshinwell: hoist extra_args None check *)
          match extra_args with
          | None -> None, field_types_by_id
          | Some extra_args ->
            let canonical_simple, kind' =
              TE.get_canonical_simple_with_kind typing_env_at_use
                ~min_name_mode:Name_mode.normal
                field
            in
            assert (Flambda_kind.equal param_kind kind');
            (* CR-someday pchambart: This shouldn't add another load if
               there is already one in the list of parameters.  That is,
                 apply_cont k (a, b) a
                 where k x y
               should become:
                 apply_cont k (a, b) a b
                 where k x y b'
               not:
                 apply_cont k (a, b) a a b
                 where k x y a' b'
               mshinwell: We never add any loads now, but might in the future.
            *)
            match canonical_simple with
            | Bottom | Ok None ->
              None, field_types_by_id
            | Ok (Some simple) ->
              if Simple.equal simple field then None, field_types_by_id
              else
                let extra_arg : EA.t = Already_in_scope simple in
                let extra_args =
                  Apply_cont_rewrite_id.Map.add id extra_arg extra_args
                in
                Some extra_args, field_types_by_id)
      arg_types_by_use_id
      (Some Apply_cont_rewrite_id.Map.empty, Apply_cont_rewrite_id.Map.empty)

  let unbox_fields_of_one_parameter info ~new_param_vars ~arg_types_by_use_id
        extra_params_and_args =
    let param_types, all_field_types_by_id_rev, extra_params_and_args =
      Index.Map.fold
        (fun index extra_param
             (param_types_rev, all_field_types_by_id_rev,
              extra_params_and_args) ->
          let extra_args, field_types_by_id =
            unbox_one_field_of_one_parameter info ~extra_param ~index
              ~arg_types_by_use_id
          in
          let param_type =
            let param_kind = KP.kind extra_param in
            match extra_args with
            | None -> T.unknown param_kind
            | Some _ -> T.alias_type_of param_kind (KP.simple extra_param)
          in
          let all_field_types_by_id_rev =
            field_types_by_id :: all_field_types_by_id_rev
          in
          match extra_args with
          | None ->
            Index.Map.add index param_type param_types_rev,
              all_field_types_by_id_rev, extra_params_and_args
          | Some extra_args ->
            let extra_params_and_args =
              EPA.add extra_params_and_args ~extra_param ~extra_args
            in
            Index.Map.add index param_type param_types_rev,
              all_field_types_by_id_rev, extra_params_and_args)
        new_param_vars
        (Index.Map.empty, [], extra_params_and_args)
    in
    param_types, List.rev all_field_types_by_id_rev,
      extra_params_and_args

  let unbox_one_parameter typing_env ~depth ~arg_types_by_use_id ~param_type
        extra_params_and_args ~unbox_value info indexes kind =
    let new_param_vars =
      Index.Set.fold (fun index new_param_vars ->
          let name =
            Format.asprintf "%s_%s" U.var_name (Index.to_string index)
          in
          let var = Variable.create name in
          let param = KP.create (Parameter.wrap var) kind in
          Index.Map.add index param new_param_vars)
        indexes
        Index.Map.empty
    in
    let fields, all_field_types_by_id, extra_params_and_args =
      unbox_fields_of_one_parameter info ~new_param_vars ~arg_types_by_use_id
        extra_params_and_args
    in
    let block_type = U.make_boxed_value info ~fields in
    let typing_env =
      TE.add_definitions_of_params typing_env
        ~params:(Index.Map.data new_param_vars)
    in
    match T.meet typing_env block_type param_type with
    | Bottom ->
      Misc.fatal_errorf "[meet] between %a and %a should not have \
         failed.  Typing env:@ %a"
        T.print block_type
        T.print param_type
        TE.print typing_env
    | Ok (param_type, env_extension) ->
      (* CR mshinwell: We can remove [New_let_binding] if we are always going to
         restrict ourselves to types where the field components are known
         [Simple]s. *)
      let typing_env = TE.add_env_extension typing_env ~env_extension in
      assert (Index.Map.cardinal fields = List.length all_field_types_by_id);
      let typing_env, extra_params_and_args =
        List.fold_left2
          (fun (typing_env, extra_params_and_args) 
               field_type field_types_by_id ->
            (* For any field of kind [Value] of the parameter being unboxed,
               then attempt to unbox its contents too. *)
            let field_kind = T.kind field_type in
            if not (K.equal field_kind K.value) then
              typing_env, extra_params_and_args
            else begin
              let typing_env, _, extra_params_and_args =
                unbox_value typing_env ~depth:(depth + 1)
                  ~arg_types_by_use_id:field_types_by_id
                  ~param_type:field_type
                  extra_params_and_args
              in
              typing_env, extra_params_and_args
            end)
          (typing_env, extra_params_and_args)
          (Index.Map.data fields) all_field_types_by_id
      in
      typing_env, param_type, extra_params_and_args
end

module Tags_and_sizes : sig
  type t = private
    | Block of {
        tag : Tag.t;
        size : Targetint.OCaml.t;
        field_kind : K.t;
      }
    | Variant of T.variant

  val block : Tag.t -> size:Targetint.OCaml.t -> field_kind:K.t -> t
  val variant : T.variant -> t

  val is_variant : t -> bool

  val unboxed_param_kind : t -> K.t

  val must_be_a_block : t -> f:(Tag.t -> size:Targetint.OCaml.t -> 'a) -> 'a

  val max_size : t -> Targetint.OCaml.t
end = struct
  type t =
    | Block of {
        tag : Tag.t;
        size : Targetint.OCaml.t;
        field_kind : K.t;
      }
    | Variant of T.variant

  let block tag ~size ~field_kind =
    Block {
      tag;
      size;
      field_kind;
    }

  let variant v = Variant v

  let is_variant t =
    match t with
    | Block _ -> false
    | Variant _ -> true

  let unboxed_param_kind t =
    match t with
    | Block { kind; _ } -> kind
    | Variant _ -> K.value

  let must_be_a_block t ~f =
    match t with
    | Block { tag; size; _ } -> f tag ~size
    | Variant _ -> Misc.fatal_error "Not a block"

  let max_size t =
    match t with
    | Block { size; _ } -> size
    | Variant v ->
      Tag.Map.fold (fun _tag size max_size ->
          if Targetint.OCaml.compare size max_size > 0 then size
          else max_size)
        v.non_const_ctors_with_sizes
        Targetint.OCaml.zero
end

(*
Blocks:
  b f0 ... fn

  Shape
  b : [0: =f0; ...; =fn]

  No CSE.

Variants:
  v is_int tag f0 ... fn

  n is the maximum index for any tag

  Type assignments
  is_int : Is_int (v)
  tag : Get_tag (v)

  Shape
  v : =0
    | =1
    | [0: =f0; ...; = fn]
    | [1: =f0]
    | ...

  CSE
  Unary (Is_int v) = is_int
  Unary (Get_tag v) = tag
*)

module Block_of_values_spec : Unboxing_spec
  with module Info = Tag
  with module Index = Targetint.OCaml =
struct
  module Index = Targetint.OCaml
  module Info = Tag

  let var_name = "unboxed"

  let make_boxed_value tag ~fields =
    let fields = Index.Map.data fields in
    T.immutable_block tag ~field_kind:Flambda_kind.value ~fields

  let make_boxed_value_accommodating _tag index ~index_var =
    (* CR mshinwell: Should create the type using the tag too. *)
    T.immutable_block_with_size_at_least
      ~n:(Targetint.OCaml.add index Targetint.OCaml.one)
      ~field_kind:Flambda_kind.value
      ~field_n_minus_one:index_var

  let project_field _tag ~block ~index =
    let index = Simple.const_int index in
    P.Binary (Block_load (Block (Value Anything), Immutable), block, index)
end

module Variants_spec : Unboxing_spec
  with module Info = Tags_and_sizes
  with module Index = Targetint.OCaml =
struct
  module Index = Targetint.OCaml
  module Info = Tags_and_sizes

  let var_name = "unboxed"

  let make_boxed_value tags_and_sizes ~fields =
    let fields = Index.Map.data fields in
    T.immutable_block tag ~field_kind:Flambda_kind.value ~fields

  let make_boxed_value_accommodating _tags_and_sizes index ~index_var =
    (* CR mshinwell: Should create the type using the tag too. *)
    T.immutable_block_with_size_at_least
      ~n:(Targetint.OCaml.add index Targetint.OCaml.one)
      ~field_kind:Flambda_kind.value
      ~field_n_minus_one:index_var

  let project_field _tags_and_sizes ~block ~index =
    let index = Simple.const_int index in
    P.Binary (Block_load (Block (Value Anything), Immutable), block, index)
end

module Block_of_naked_floats_spec : Unboxing_spec
  with module Info = Tag
  with module Index = Targetint.OCaml =
struct
  module Index = Targetint.OCaml
  module Info = Tag

  let var_name = "unboxed"

  let make_boxed_value tag ~fields =
    let fields = Index.Map.data fields in
    T.immutable_block tag ~field_kind:Flambda_kind.naked_float ~fields

  let make_boxed_value_accommodating _tag index ~index_var =
    (* CR mshinwell: Should create the type using the tag too. *)
    T.immutable_block_with_size_at_least
      ~n:(Targetint.OCaml.add index Targetint.OCaml.one)
      ~field_kind:Flambda_kind.naked_float
      ~field_n_minus_one:index_var

  let project_field _tag ~block ~index =
    let index = Simple.const_int index in
    P.Binary (Block_load (Block Naked_float, Immutable), block, index)
end

module Closures_info = struct
  type t = {
    closure_id : Closure_id.t;
    closures_entry : T.Closures_entry.t;
  }
end

module Closures_spec : Unboxing_spec
  with module Info = Closures_info
  with module Index = Var_within_closure =
struct
  module Index = Var_within_closure
  module Info = Closures_info

  let var_name = "unboxed_clos_var"

  let make_boxed_value (info : Info.t) ~fields:closure_vars =
    let closures_entry = info.closures_entry in
    let all_function_decls_in_set =
      T.Closures_entry.function_decl_types closures_entry
    in
    let all_closures_in_set = T.Closures_entry.closure_types closures_entry in
    T.exactly_this_closure info.closure_id
      ~all_function_decls_in_set
      ~all_closures_in_set
      ~all_closure_vars_in_set:closure_vars

  let make_boxed_value_accommodating _info closure_var ~index_var =
    T.closure_with_at_least_this_closure_var closure_var
      ~closure_element_var:index_var

(*
    match Var_within_closure.Set.max_elt_opt closure_vars with
    | None -> make_boxed_value info ~fields:Var_within_closure.Map.empty
    | Some closure_element ->
      T.closure_with_at_least_these_closure_vars closure_vars
        ~closure_element
        ~closure_element_var:highest_index_var
*)

  let project_field (info : Info.t) ~block:closure ~index:closure_var =
    P.Unary (Project_var {
        project_from = info.closure_id;
        var = closure_var;
        },
      closure)
end

module Make_unboxed_number_spec (N : sig
  val name : string
  val var_name : string

  val tag : Tag.t

  val unboxed_kind : K.t
  val boxable_number_kind : K.Boxable_number.t

  val box : T.t -> T.t
end) = struct
  module Index = Targetint.OCaml
  module Info = Tag

  let var_name = N.var_name

  let make_boxed_value tag ~fields =
    assert (Tag.equal tag N.tag);
    let fields = Index.Map.data fields in
    match fields with
    | [field] -> N.box field
    | _ -> Misc.fatal_errorf "Boxed %ss only have one field" N.name

  let make_boxed_value_accommodating _tag index ~index_var =
    (* CR mshinwell: Should create the type using the tag too. *)
    if not (Targetint.OCaml.equal index Targetint.OCaml.zero) then begin
       Misc.fatal_errorf "Boxed %ss only have one field" N.name
    end;
    N.box (T.alias_type_of N.unboxed_kind (Simple.var index_var))

  let project_field _tag ~block ~index:_ =
    P.Unary (Unbox_number N.boxable_number_kind, block)
end

module Immediate_spec : Unboxing_spec
  with module Info = Tag
  with module Index = Targetint.OCaml =
Make_unboxed_number_spec (struct
  let name = "immediate"
  let var_name = "untagged_imm"
  let tag = Tag.zero  (* CR mshinwell: make optional *)
  let unboxed_kind = K.naked_immediate
  let boxable_number_kind = K.Boxable_number.Untagged_immediate
  let box = T.tag_immediate
end)

module Float_spec : Unboxing_spec
  with module Info = Tag
  with module Index = Targetint.OCaml =
Make_unboxed_number_spec (struct
  let name = "float"
  let var_name = "unboxed_float"
  let tag = Tag.double_tag
  let unboxed_kind = K.naked_float
  let boxable_number_kind = K.Boxable_number.Naked_float
  let box = T.box_float
end)

module Int32_spec : Unboxing_spec
  with module Info = Tag
  with module Index = Targetint.OCaml =
Make_unboxed_number_spec (struct
  let name = "int32"
  let var_name = "unboxed_int32"
  let tag = Tag.custom_tag
  let unboxed_kind = K.naked_int32
  let boxable_number_kind = K.Boxable_number.Naked_int32
  let box = T.box_int32
end)

module Int64_spec : Unboxing_spec
  with module Info = Tag
  with module Index = Targetint.OCaml =
Make_unboxed_number_spec (struct
  let name = "int64"
  let var_name = "unboxed_int64"
  let tag = Tag.custom_tag
  let unboxed_kind = K.naked_int64
  let boxable_number_kind = K.Boxable_number.Naked_int64
  let box = T.box_int64
end)

module Nativeint_spec : Unboxing_spec
  with module Info = Tag
  with module Index = Targetint.OCaml =
Make_unboxed_number_spec (struct
  let name = "nativeint"
  let var_name = "unboxed_nativeint"
  let tag = Tag.custom_tag
  let unboxed_kind = K.naked_nativeint
  let boxable_number_kind = K.Boxable_number.Naked_nativeint
  let box = T.box_nativeint
end)

module Blocks_of_values = Make (Block_of_values_spec)
module Blocks_of_naked_floats = Make (Block_of_naked_floats_spec)
module Closures = Make (Closures_spec)
module Immediates = Make (Immediate_spec)
module Floats = Make (Float_spec)
module Int32s = Make (Int32_spec)
module Int64s = Make (Int64_spec)
module Nativeints = Make (Nativeint_spec)

let unboxed_number_decisions = [
  T.prove_is_a_tagged_immediate, Immediates.unbox_one_parameter,
    Tag.zero, K.naked_immediate;
  T.prove_is_a_boxed_float, Floats.unbox_one_parameter,
    Tag.double_tag, K.naked_float;
  T.prove_is_a_boxed_int32, Int32s.unbox_one_parameter,
    Tag.custom_tag, K.naked_int32;
  T.prove_is_a_boxed_int64, Int64s.unbox_one_parameter,
    Tag.custom_tag, K.naked_int64;
  T.prove_is_a_boxed_nativeint, Nativeints.unbox_one_parameter,
    Tag.custom_tag, K.naked_nativeint;
]

let rec make_unboxing_decision typing_env ~depth ~arg_types_by_use_id
      ~param_type extra_params_and_args =
  if depth > max_unboxing_depth then
    typing_env, param_type, extra_params_and_args
  else
    match T.prove_unique_tag_and_size typing_env param_type with
    | Proved (tag, size) ->
      let indexes =
        let size = Targetint.OCaml.to_int size in
        Targetint.OCaml.Set.of_list
          (List.init size (fun index -> Targetint.OCaml.of_int index))
      in
      (* If the fields have kind [Naked_float] then the [tag] will always
         be [Tag.double_array_tag].  See [Row_like.For_blocks]. *)
      if Tag.equal tag Tag.double_array_tag then
        Blocks_of_naked_floats.unbox_one_parameter typing_env ~depth
          ~arg_types_by_use_id ~param_type extra_params_and_args
          ~unbox_value:make_unboxing_decision tag indexes K.naked_float
      else
        begin match Tag.Scannable.of_tag tag with
        | Some _ ->
          Blocks_of_values.unbox_one_parameter typing_env ~depth
            ~arg_types_by_use_id ~param_type extra_params_and_args
            ~unbox_value:make_unboxing_decision tag indexes K.value
        | None ->
          Misc.fatal_errorf "Block that is not of tag [Double_array_tag] \
              and yet also not scannable:@ %a@ in env:@ %a"
            T.print param_type
            TE.print typing_env
        end
    | Wrong_kind | Invalid | Unknown ->
      (* CR mshinwell: This variant case should be able to subsume the
         "block of values" case. *)
      match T.prove_variant typing_env param_type with
      | Proved variant ->
        let info = Tags_and_sizes.of_variant variant in
        let indexes =
          let size = Tags_and_sizes.max_size info in
          Targetint.OCaml.Set.of_list
            (List.init size (fun index -> Targetint.OCaml.of_int index))
        in
        Variants.unbox_one_parameter typing_env ~depth
          ~arg_types_by_use_id ~param_type extra_params_and_args
          ~unbox_value:make_unboxing_decision info indexes K.value
      | Wrong_kind | Invalid | Unknown ->
        match T.prove_single_closures_entry' typing_env param_type with
        | Proved (closure_id, closures_entry, Ok _func_decl_type) ->
          let closure_var_types =
            T.Closures_entry.closure_var_types closures_entry
          in
          let info : Closures_spec.Info.t =
            { closure_id;
              closures_entry;
            }
          in
          let closure_vars = Var_within_closure.Map.keys closure_var_types in
          Closures.unbox_one_parameter typing_env ~depth
            ~arg_types_by_use_id ~param_type extra_params_and_args
            ~unbox_value:make_unboxing_decision info closure_vars K.value
        | Proved (_, _, (Unknown | Bottom)) | Wrong_kind | Invalid | Unknown ->
          let rec try_unboxing = function
            | [] -> typing_env, param_type, extra_params_and_args
            | (prover, unboxer, tag, kind) :: decisions ->
              let proof : _ T.proof_allowing_kind_mismatch =
                prover typing_env param_type
              in
              match proof with
              | Proved () ->
                let indexes =
                  Targetint.OCaml.Set.singleton Targetint.OCaml.zero
                in
                unboxer typing_env ~depth ~arg_types_by_use_id ~param_type
                  extra_params_and_args ~unbox_value:make_unboxing_decision
                  tag indexes kind
              | Wrong_kind | Invalid | Unknown -> try_unboxing decisions
          in
          try_unboxing unboxed_number_decisions

let make_unboxing_decisions typing_env ~arg_types_by_use_id ~params ~param_types
      extra_params_and_args =
  assert (List.compare_lengths params param_types = 0);
  let typing_env, param_types_rev, extra_params_and_args =
    List.fold_left (fun (typing_env, param_types_rev, extra_params_and_args)
              (arg_types_by_use_id, param_type) ->
        let typing_env, param_type, extra_params_and_args =
          make_unboxing_decision typing_env ~depth:0 ~arg_types_by_use_id
            ~param_type extra_params_and_args
        in
        typing_env, param_type :: param_types_rev, extra_params_and_args)
      (typing_env, [], extra_params_and_args)
      (List.combine arg_types_by_use_id param_types)
  in
  let typing_env =
    TE.add_equations_on_params typing_env
      ~params ~param_types:(List.rev param_types_rev)
  in
  typing_env, extra_params_and_args
