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

type t = {
  defined_vars : Flambda_kind.t Variable.Map.t;
  binding_times : Variable.Set.t Binding_time.Map.t;
  equations : Type_grammar.t Name.Map.t;
  symbol_projections : Symbol_projection.t Variable.Map.t;
}

let defined_vars t = t.defined_vars

let defined_names t =
  Name.set_of_var_set (Variable.Map.keys t.defined_vars)

(*
let defines_name_but_no_equations t name =
  match Name.to_var name with
  | None -> false
  | Some var ->
    Variable.Map.mem var t.defined_vars
      && not (Name.Map.mem name t.equations)
*)

let print_with_cache ~cache ppf
      { defined_vars; binding_times = _; equations;
        symbol_projections = _; } =
  (* CR mshinwell: print symbol projections along with tidying up this
     function *)
  let print_equations ppf equations =
    let equations = Name.Map.bindings equations in
    match equations with
    | [] -> Format.pp_print_string ppf "()"
    | _::_ ->
      Format.pp_print_string ppf "(";
      Format.pp_print_list ~pp_sep:Format.pp_print_space
        (fun ppf (name, ty) ->
          Format.fprintf ppf
            "@[<hov 1>%a@ :@ %a@]"
            Name.print name
            (Type_grammar.print_with_cache ~cache) ty)
        ppf equations;
      Format.pp_print_string ppf ")"
  in
  (* CR mshinwell: Print [defined_vars] when not called from
     [Typing_env.print] *)
  if Variable.Map.is_empty defined_vars then
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(equations@ @[<v 1>%a@])@])\
        @]"
      print_equations equations
  else
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(defined_vars@ @[<hov 1>%a@])@]@ \
        @[<hov 1>(equations@ @[<v 1>%a@])@]@ \
        )@]"
      Variable.Set.print (Variable.Map.keys defined_vars) (* XXX *)
      print_equations equations

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let invariant _t = ()

let fold_on_defined_vars f t init =
  Binding_time.Map.fold (fun _bt vars acc ->
      Variable.Set.fold (fun var acc ->
          let kind = Variable.Map.find var t.defined_vars in
          f var kind acc)
        vars
        acc)
    t.binding_times
    init

let apply_name_permutation
      ({ defined_vars; binding_times; equations; symbol_projections; }
        as t)
      perm =
  let defined_vars_changed = ref false in
  let defined_vars' =
    Variable.Map.fold (fun var kind defined_vars ->
        let var' = Name_permutation.apply_variable perm var in
        if not (var == var') then begin
          defined_vars_changed := true
        end;
        Variable.Map.add var' kind defined_vars)
      defined_vars
      Variable.Map.empty
  in
  let binding_times' =
    if !defined_vars_changed then
      Binding_time.Map.fold (fun binding_time vars binding_times ->
          let vars' =
            Variable.Set.map (fun var ->
              Name_permutation.apply_variable perm var)
              vars
          in
          Binding_time.Map.add binding_time vars' binding_times)
        binding_times
        Binding_time.Map.empty
    else
      binding_times
  in
  let equations_changed = ref false in
  let equations' =
    Name.Map.fold (fun name typ equations ->
        let name' = Name_permutation.apply_name perm name in
        let typ' = Type_grammar.apply_name_permutation typ perm in
        if not (name == name' && typ == typ') then begin
          equations_changed := true
        end;
        Name.Map.add name' typ' equations)
      equations
      Name.Map.empty
  in
  let cse_changed = ref false in
  let cse' =
    Flambda_primitive.Eligible_for_cse.Map.fold (fun prim simple cse' ->
        let simple' = Simple.apply_name_permutation simple perm in
        let prim' =
          Flambda_primitive.Eligible_for_cse.apply_name_permutation prim perm
        in
        if (not (simple == simple')) || (not (prim == prim')) then begin
          cse_changed := true
        end;
        Flambda_primitive.Eligible_for_cse.Map.add prim' simple' cse')
      cse
      Flambda_primitive.Eligible_for_cse.Map.empty
  in
  let symbol_projections_changed = ref false in
  let symbol_projections' =
    Variable.Map.fold (fun var projection symbol_projections ->
        let var' = Name_permutation.apply_variable perm var in
        let projection' =
          Symbol_projection.apply_name_permutation projection perm
        in
        if not (var == var') ||
           not (projection == projection') then begin
          symbol_projections_changed := true
        end;
        Variable.Map.add var' projection' symbol_projections)
      symbol_projections
      Variable.Map.empty
  in
  if (not !defined_vars_changed)
    && (not !equations_changed)
    && (not !cse_changed)
    && (not !symbol_projections_changed)
  then t
  else
    { defined_vars = defined_vars';
      binding_times = binding_times';
      equations = equations';
      cse = cse';
      symbol_projections = symbol_projections';
    }

let free_names
      { defined_vars; binding_times = _; equations; symbol_projections; } =
  let free_names_defined_vars =
    Name_occurrences.create_variables (Variable.Map.keys defined_vars)
      Name_mode.in_types
  in
  let free_names =
    Name.Map.fold (fun name typ free_names ->
        let free_names' =
          Name_occurrences.add_name (Type_grammar.free_names typ)
            name Name_mode.in_types
        in
        Name_occurrences.union free_names free_names')
      equations
      free_names_defined_vars
  in
  Variable.Map.fold (fun _var proj free_names ->
      Name_occurrences.union free_names
        (Symbol_projection.free_names proj))
    symbol_projections
    free_names

let empty () =
  { defined_vars = Variable.Map.empty;
    binding_times = Binding_time.Map.empty;
    equations = Name.Map.empty;
    symbol_projections = Variable.Map.empty;
  }

let is_empty
      { defined_vars; binding_times; equations;
        symbol_projections; } =
  Variable.Map.is_empty defined_vars
    && Binding_time.Map.is_empty binding_times
    && Name.Map.is_empty equations
    && Variable.Map.is_empty symbol_projections

let equations t = t.equations

let symbol_projections t = t.symbol_projections

let add_symbol_projection t var proj =
  let symbol_projections =
    Variable.Map.add var proj t.symbol_projections
  in
  { t with symbol_projections; }

let add_definition t var kind binding_time =
  if !Clflags.flambda_invariant_checks
    && Variable.Map.mem var t.defined_vars
  then begin
    Misc.fatal_errorf "Environment extension already binds variable %a:@ %a"
      Variable.print var
      print t
  end;
  let binding_times =
    let vars =
      match Binding_time.Map.find binding_time t.binding_times with
      | exception Not_found ->
        Variable.Set.singleton var
      | prev_vars ->
        Variable.Set.add var prev_vars
    in
    Binding_time.Map.add binding_time vars t.binding_times
  in
  { t with
    defined_vars = Variable.Map.add var kind t.defined_vars;
    binding_times;
  }

let equation_is_directly_recursive name ty =
  match Type_grammar.get_alias_exn ty with
  | exception Not_found -> false
  | simple ->
    Simple.pattern_match simple
      ~name:(fun name' -> Name.equal name name')
      ~const:(fun _ -> false)

let check_equation t name ty =
  if !Clflags.flambda_invariant_checks then begin
    if equation_is_directly_recursive name ty then begin
      Misc.fatal_errorf "Directly recursive equation@ %a = %a@ \
          disallowed (Typing_env_level):@ %a"
        Name.print name
        Type_grammar.print ty
        print t
    end
  end

let one_equation name ty =
  check_equation (empty ()) name ty;
  { defined_vars = Variable.Map.empty;
    binding_times = Binding_time.Map.empty;
    equations = Name.Map.singleton name ty;
    symbol_projections = Variable.Map.empty;
  }

let add_or_replace_equation t name ty =
  check_equation t name ty;
  if Type_grammar.is_obviously_unknown ty then
    { t with
      equations = Name.Map.remove name t.equations;
    }
  else
    { t with
      equations = Name.Map.add name ty t.equations;
    }

let concat (t1 : t) (t2 : t) =
  let defined_vars =
    Variable.Map.union (fun var _data1 _data2 ->
        Misc.fatal_errorf "Cannot concatenate levels that have overlapping \
            defined variables (e.g. %a):@ %a@ and@ %a"
          Variable.print var
          print t1
          print t2)
      t1.defined_vars
      t2.defined_vars
  in
  let binding_times =
    Binding_time.Map.union (fun _binding_time vars1 vars2 ->
      (* CR vlaviron: Technically this is feasible, as we can allow several
         variables with the same binding time, but it should only come from
         joins; concat arguments should always have disjoint binding time
         domains *)
        Misc.fatal_errorf "Cannot concatenate levels that have variables \
            with overlapping binding times (e.g. %a and %a):@ %a@ and@ %a"
          Variable.Set.print vars1
          Variable.Set.print vars2
          print t1
          print t2)
      t1.binding_times
      t2.binding_times
  in
  let equations =
    Name.Map.union (fun _ _ty1 ty2 -> Some ty2) t1.equations t2.equations
  in
  let symbol_projections =
    Variable.Map.union (fun _var _proj1 proj2 -> Some proj2)
      t1.symbol_projections
      t2.symbol_projections
  in
  { defined_vars;
    binding_times;
    equations;
    symbol_projections;
  }

let meet_equation0 env t name typ =
  check_equation t name typ;
  let meet_typ, env_extension =
    match Name.Map.find name t.equations with
    | exception Not_found -> typ, Typing_env_extension.empty ()
    | existing_typ -> Type_grammar.meet' env typ existing_typ
  in
  let env =
    let typing_env =
      let typing_env = Meet_env.env env in
      Typing_env.add_equation
        (Typing_env.add_env_extension typing_env env_extension)
        name typ
    in
    Meet_env.with_typing_env env typing_env
  in
  (* CR mshinwell: This special case needs further thinking about *)
  (* When meeting recursive types we can end up attempting to add
     equations "x : =x". *)
  let equations =
    if equation_is_directly_recursive name meet_typ then t.equations
    else Name.Map.add (* replace *) name meet_typ t.equations
  in
  let equations =
    Typing_env_extension.pattern_match env_extension ~f:(fun t_from_meet ->
      if not (Variable.Map.is_empty t_from_meet.defined_vars) then begin
        Misc.fatal_errorf "Didn't expect [defined_vars] in:@ %a"
          print t_from_meet
      end;
      Name.Map.fold (fun name typ equations ->
          check_equation t name typ;
          Name.Map.add (* replace *) name typ equations)
        t_from_meet.equations
        equations)
  in
  let t = { t with equations; } in
  t, env

let meet_equation env t name typ =
  try meet_equation0 env t name typ
  with Misc.Fatal_error ->
    if !Clflags.flambda_context_on_error then begin
      Format.eprintf "\n%sContext is:%s meeting equation %a : %a@ in \
          level@ %a@ and environment@ %a\n"
        (Flambda_colours.error ())
        (Flambda_colours.normal ())
        Name.print name
        Type_grammar.print typ
        print t
        Typing_env.print (Meet_env.env env)
    end;
    raise Misc.Fatal_error

let meet0 env (t1 : t) (t2 : t) =
  (* Format.eprintf "Typing_env_level.meet:@ t1=%a@ t2=%a@." print t1 print t2; *)
  let defined_vars =
    Variable.Map.union (fun var kind1 kind2 ->
        if Flambda_kind.equal kind1 kind2 then Some kind1
        else
          Misc.fatal_errorf "Cannot meet levels that have overlapping \
              defined variables (e.g. %a) that disagree on kind and/or \
              binding time:@ %a@ and@ %a"
            Variable.print var
            print t1
            print t2)
      t1.defined_vars
      t2.defined_vars
  in
  let binding_times =
    Binding_time.Map.union (fun _bt vars1 vars2 ->
        Some (Variable.Set.union vars1 vars2))
      t1.binding_times
      t2.binding_times
  in
  let env =
    let typing_env =
      (* Iterating on binding_times ensures that the resulting typing env
         is compatible with both inputs regarding binding times.
         When several variables have the same binding time, we assume they
         come from distinct contexts and that their relative ordering does not
         matter. *)
      Binding_time.Map.fold (fun _bt vars typing_env ->
          Variable.Set.fold (fun var typing_env ->
              let kind = Variable.Map.find var defined_vars in
              let name =
                Name_in_binding_pos.create (Name.var var) Name_mode.in_types
              in
              Typing_env.add_definition typing_env name kind)
            vars
            typing_env)
        binding_times
        (Meet_env.env env)
    in
    Meet_env.with_typing_env env typing_env
  in
  let t =
    { (empty ()) with
      defined_vars;
      binding_times;
    }
  in
  let t, env =
    Name.Map.fold (fun name ty (t, env) -> meet_equation env t name ty)
      t1.equations
      (t, env)
  in
  let t, _env =
    Name.Map.fold (fun name ty (t, env) -> meet_equation env t name ty)
      t2.equations
      (t, env)
  in
  let symbol_projections =
    Variable.Map.union (fun _ proj1 proj2 ->
        (* CR vlaviron:
           I'm not sure whether this can come up at all, but
           if proj1 and proj2 are different then it means the corresponding
           variable can be accessed through two different projections.
           I think it would be safe to use any of them, but forgetting
           the projection is guaranteed to be sound and I think it is
           better for debugging problems if the meet function is kept
           symmetrical. *)
        if Symbol_projection.equal proj1 proj2 then Some proj1
        else None)
      t1.symbol_projections t2.symbol_projections
  in
  { t with
    symbol_projections;
  }

let meet env t1 t2 =
  (* Care: the domains of [t1] and [t2] are treated as contravariant.
     As such, since this is [meet], we perform unions on the domains.
     So if one of them is bottom, the result of meeting it with any other
     level is that level, not bottom. *)
  if is_empty t1 then t2
  else if is_empty t2 then t1
  else meet0 env t1 t2

let extend env t1 ~ext:t2 =
  let defined_vars =
    Variable.Map.fold (fun var kind defined_vars ->
        match Variable.Map.find_opt var defined_vars with
        | None -> Variable.Map.add var kind defined_vars
        | Some kind2 ->
          if Flambda_kind.equal kind kind2 then defined_vars
          else
            Misc.fatal_errorf "Cannot meet levels that have overlapping \
                defined variables (e.g. %a) that disagree on kind and/or \
                binding time:@ %a@ and@ %a"
              Variable.print var
              print t1
              print t2)
      t2.defined_vars
      t1.defined_vars
  in
  let binding_times =
    Binding_time.Map.fold (fun bt vars binding_times ->
        match Binding_time.Map.find_opt bt binding_times with
        | None -> Binding_time.Map.add bt vars binding_times
        | Some vars2 ->
          let vars = Variable.Set.union vars vars2 in
          Binding_time.Map.add bt vars binding_times)
      t2.binding_times
      t1.binding_times
  in
  let rec add_equation name typ equations =
    match Name.Map.find name equations with
    | exception Not_found ->
      if equation_is_directly_recursive name typ then equations
      else Name.Map.add name typ equations
    | existing_typ ->
      let meet_typ, extension = Type_grammar.meet' env typ existing_typ in
      if equation_is_directly_recursive name meet_typ then equations
      else
        let equations = Name.Map.add name meet_typ equations in
        Typing_env_extension.pattern_match extension ~f:(fun t ->
          (* Sanity check *)
          if not (Variable.Map.is_empty t.defined_vars) then begin
            Misc.fatal_errorf "Didn't expect [defined_vars] in:@ %a"
              print t
          end;
          Name.Map.fold add_equation t.equations equations)
  in
  let equations = Name.Map.fold add_equation t2.equations t1.equations in
  let symbol_projections =
    Variable.Map.fold (fun var proj symbol_projections ->
        match Variable.Map.find var symbol_projections with
        | exception Not_found -> Variable.Map.add var proj symbol_projections
        | existing_proj ->
          if Symbol_projection.equal proj existing_proj then symbol_projections
          else Variable.Map.remove var symbol_projections)
      t2.symbol_projections
      t1.symbol_projections
  in
  { defined_vars;
    binding_times;
    equations;
    symbol_projections;
  }

let join_types ~env_at_fork envs_with_levels ~extra_lifted_consts_in_use_envs =
  (* Add all the variables defined by the branches as existentials to the
     [env_at_fork].
     Any such variable will be given type [Unknown] on a branch where it
     was not originally present.
     Iterating on [level.binding_times] instead of [level.defined_vars] ensures
     consistency of binding time order in the branches and the result. *)
  let env_at_fork =
    List.fold_left (fun env_at_fork (_, _, _, level) ->
        Binding_time.Map.fold (fun _ vars env ->
            Variable.Set.fold (fun var env ->
                if Typing_env.mem env (Name.var var) then env
                else
                  let kind = Variable.Map.find var level.defined_vars in
                  Typing_env.add_definition env
                    (Name_in_binding_pos.var
                       (Var_in_binding_pos.create var Name_mode.in_types))
                    kind)
              vars
              env)
          level.binding_times
          env_at_fork)
      env_at_fork
      envs_with_levels
  in
  (* Now fold over the levels doing the actual join operation on equations. *)
  ListLabels.fold_left envs_with_levels
    ~init:(env_at_fork, Name.Map.empty, true)
    ~f:(fun (join_env, joined_types, is_first_join) (env_at_use, _, _, t) ->
      let join_env =
        Code_age_relation.union (Typing_env.code_age_relation join_env)
          (Typing_env.code_age_relation env_at_use)
        |> Typing_env.with_code_age_relation join_env
      in
      let next_join_env = ref join_env in
      let join_types name joined_ty use_ty =
        (* CR mshinwell for vlaviron: Looks like [Typing_env.mem] needs
           fixing with respect to names from other units with their
           .cmx missing (c.f. testsuite/tests/lib-dynlink-native/). *)
        let same_unit =
          Compilation_unit.equal (Name.compilation_unit name)
            (Compilation_unit.get_current_exn ())
        in
        if same_unit && not (Typing_env.mem env_at_fork name) then begin
          Misc.fatal_errorf "Name %a not defined in [env_at_fork]:@ %a"
            Name.print name
            Typing_env.print env_at_fork
        end;
        let is_lifted_const_symbol =
          match Name.must_be_symbol_opt name with
          | None -> false
          | Some symbol ->
            Symbol.Set.mem symbol extra_lifted_consts_in_use_envs
        in
        (* If [name] is that of a lifted constant symbol generated during one
           of the levels, then ignore it.  [Simplify_expr] will already have
           made its type suitable for [env_at_fork] and inserted it into that
           environment. *)
        if is_lifted_const_symbol then None
        else
          let joined_ty =
            match joined_ty, use_ty with
            | None, Some use_ty ->
              (* In this case, we haven't yet got a joined type for [name]. *)
              let left_ty =
                (* If this is the first level in the join, we just need to
                   make the type suitable for the joined environment, so we
                   use [Bottom] to avoid losing precision... *)
                if is_first_join then Type_grammar.bottom_like use_ty
                (* ...but if this is not the first level in the join, then we
                   need to get the best type we can for [name] which will be
                   valid on all of the previous paths.  This is the type of
                   [name] in the original [env_at_fork] (passed to [join],
                   below) save that if [name] was undefined there, we can only
                   give [Unknown] (or [Bottom] for a symbol).  Since in [join]
                   below we define all of the existentials and introduced
                   lifted constant symbols in the fork environment, we can
                   actually always just look the type up there, without needing
                   to case split. *)
                else
                  let expected_kind = Some (Type_grammar.kind use_ty) in
                  Typing_env.find env_at_fork name expected_kind
              in
              (* Recall: the order of environments matters for [join].
                 Also note that we use [env_at_fork] not [env_at_use] for
                 the right-hand environment.  This is done because there may
                 be names in types in [env_at_fork] that are not defined in
                 [env_at_use] -- see the comment in [check_join_inputs]
                 below. *)
              Type_grammar.join ~bound_name:name
                env_at_fork
                ~left_env:join_env ~left_ty
                ~right_env:env_at_fork ~right_ty:use_ty
            | Some joined_ty, None ->
              (* There is no equation, at all (not even saying "unknown"), on
                 the current level for [name].  However we have seen an
                 equation for [name] on a previous level.  We need to get the
                 best type we can for [name] on the current level, from
                 [env_at_fork], similarly to the previous case. *)
              assert (not is_first_join);
              let expected_kind = Some (Type_grammar.kind joined_ty) in
              let right_ty = Typing_env.find env_at_fork name expected_kind in
              Type_grammar.join ~bound_name:name
                env_at_fork
                ~left_env:join_env ~left_ty:joined_ty
                ~right_env:env_at_fork ~right_ty
            | Some joined_ty, Some use_ty ->
              (* This is the straightforward case, where we have already
                 started computing a joined type for [name], and there is an
                 equation for [name] on the current level. *)
              assert (not is_first_join);
              Type_grammar.join ~bound_name:name
                env_at_fork
                ~left_env:join_env ~left_ty:joined_ty
                ~right_env:env_at_use ~right_ty:use_ty
            | None, None -> assert false
          in
          next_join_env :=
            Typing_env.add_equation !next_join_env name joined_ty;
          Some joined_ty
      in
      let joined_types = Name.Map.merge join_types joined_types t.equations in
      !next_join_env, joined_types, false)
  |> fun (_, joined_types, _) ->
  joined_types

let construct_joined_level envs_with_levels ~env_at_fork ~allowed
      ~joined_types =
  let defined_vars, binding_times =
    List.fold_left (fun (defined_vars, binding_times)
                     (_env_at_use, _id, _use_kind, t) ->
        let defined_vars_this_level =
          Variable.Map.filter (fun var _ ->
              Name_occurrences.mem_var allowed var)
            t.defined_vars
        in
        let defined_vars =
          Variable.Map.union (fun var kind1 kind2 ->
              if Flambda_kind.equal kind1 kind2 then Some kind1
              else
                Misc.fatal_errorf "Cannot join levels that disagree on the kind \
                    of [defined_vars] (%a and %a for %a)"
                  Flambda_kind.print kind1
                  Flambda_kind.print kind2
                  Variable.print var)
            defined_vars
            defined_vars_this_level
        in
        let binding_times_this_level =
          Binding_time.Map.filter_map
            (fun _ vars ->
              let vars =
                Variable.Set.filter (fun var ->
                    Name_occurrences.mem_var allowed var)
                  vars
              in
              if Variable.Set.is_empty vars then None
              else Some vars)
            t.binding_times
        in
        let binding_times =
          Binding_time.Map.union (fun _bt vars1 vars2 ->
              Some (Variable.Set.union vars1 vars2))
            binding_times
            binding_times_this_level
        in
        (defined_vars, binding_times))
      (Variable.Map.empty, Binding_time.Map.empty)
      envs_with_levels
  in
  let equations =
    Name.Map.filter (fun name _ty -> Name_occurrences.mem_name allowed name)
      joined_types
  in
  let symbol_projections =
    List.fold_left (fun symbol_projections (_env_at_use, _id, _use_kind, t) ->
        let projs_this_level =
          Variable.Map.filter (fun var _ ->
              let name = Name.var var in
              Typing_env.mem ~min_name_mode:Name_mode.normal env_at_fork name
                || Name_occurrences.mem_name allowed name)
            t.symbol_projections
        in
        Variable.Map.union (fun _var proj1 proj2 ->
            if Symbol_projection.equal proj1 proj2 then Some proj1
            else None)
          symbol_projections
          projs_this_level)
      Variable.Map.empty
      envs_with_levels
  in
  { defined_vars;
    binding_times;
    equations;
    symbol_projections;
  }

let check_join_inputs ~env_at_fork _envs_with_levels ~params
      ~extra_lifted_consts_in_use_envs =
  (* It might seem as if every name defined in [env_at_fork], with the
     exception of the lifted constant symbols, should occur in every
     use environment.  However this is not the case: the introduction of
     the lifted constants into [env_at_fork] in [Simplify_expr] may have
     produced [In_types] variables (from [make_suitable_for_environment])
     that will not be present in any use environment. *)
  List.iter (fun param ->
      if not (Typing_env.mem env_at_fork (Kinded_parameter.name param))
      then begin
        Misc.fatal_errorf "Parameter %a not defined in [env_at_fork] at join"
          Kinded_parameter.print param
      end)
    params;
  Symbol.Set.iter (fun symbol ->
      if not (Typing_env.mem env_at_fork (Name.symbol symbol)) then begin
        Misc.fatal_errorf "Symbol %a, which is a new lifted constant that \
            arose during the simplification of the continuation's body, is \
            not defined in the [env_at_fork] when calling [join]"
          Symbol.print symbol
      end)
    extra_lifted_consts_in_use_envs

let join ~env_at_fork envs_with_levels ~params
      ~extra_lifted_consts_in_use_envs ~extra_allowed_names:allowed =
  check_join_inputs ~env_at_fork envs_with_levels ~params
    ~extra_lifted_consts_in_use_envs;
  (* Calculate the joined types of all the names involved. *)
  let joined_types =
    join_types ~env_at_fork envs_with_levels ~extra_lifted_consts_in_use_envs
  in
  (* Next calculate which equations (describing joined types) to propagate to
     the join point.  (Recall that the environment at the fork point includes
     the parameters of the continuation being called at the join. We wish to
     ensure that information in the types of these parameters is not lost.)
     - Equations on names defined in the environment at the fork point are
     always propagated.
     - Definitions of, and equations on, names that occur free on the
     right-hand sides of the propagated equations are also themselves
     propagated. The definition of any such propagated name (i.e. one that
     does not occur in the environment at the fork point) will be made
     existential. *)
  (* CR vlaviron: We need to compute the free names of joined_types,
     we can't use a typing environment *)
  let free_names_transitive typ =
    let rec free_names_transitive0 typ ~result =
      let free_names = Type_grammar.free_names typ in
      let to_traverse = Name_occurrences.diff free_names result in
      Name_occurrences.fold_names to_traverse
        ~init:result
        ~f:(fun result name ->
          let result =
            Name_occurrences.add_name result name Name_mode.in_types
          in
          match Name.Map.find name joined_types with
          | exception Not_found -> result
          | typ ->
            free_names_transitive0 typ ~result)
    in
    free_names_transitive0 typ ~result:Name_occurrences.empty
  in
  let allowed =
    Name.Map.fold (fun name ty allowed ->
        if Typing_env.mem env_at_fork name
          || Name.is_symbol name
        then
          Name_occurrences.add_name
            (Name_occurrences.union allowed
              (free_names_transitive ty))
            name Name_mode.in_types
        else
          allowed)
      joined_types
      allowed
  in
  let allowed =
    Symbol.Set.fold (fun symbol allowed ->
        Name_occurrences.add_symbol allowed symbol Name_mode.in_types)
      extra_lifted_consts_in_use_envs
      allowed
  in
  (* Having calculated which equations to propagate, the resulting level can
     now be constructed. *)
  construct_joined_level envs_with_levels ~env_at_fork ~allowed ~joined_types

let n_way_join ~env_at_fork envs_with_levels ~params
      ~extra_lifted_consts_in_use_envs ~extra_allowed_names =
  match envs_with_levels with
  | [] -> empty ()
  | envs_with_levels ->
    join ~env_at_fork envs_with_levels ~params ~extra_lifted_consts_in_use_envs
      ~extra_allowed_names

let all_ids_for_export t =
  let variables = Variable.Map.keys t.defined_vars in
  let ids = Ids_for_export.create ~variables () in
  let equation name ty ids =
    let ids =
      Ids_for_export.union ids
        (Type_grammar.all_ids_for_export ty)
    in
    Ids_for_export.add_name ids name
  in
  let ids = Name.Map.fold equation t.equations ids in
  let symbol_projection var proj ids =
    let ids =
      Ids_for_export.union ids (Symbol_projection.all_ids_for_export proj)
    in
    Ids_for_export.add_variable ids var
  in
  Variable.Map.fold symbol_projection t.symbol_projections ids

let import _import_map _t =
  Misc.fatal_error "Import not implemented on Typing_env_level"
