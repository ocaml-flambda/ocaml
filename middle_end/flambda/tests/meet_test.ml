module K = Flambda_kind
module T = Flambda_type
module TE = T.Typing_env
module TEE = T.Typing_env_extension

let resolver _ = None
let get_imported_names () = Name.Set.empty

let test_meet_chains_two_vars () =
  let env = TE.create ~resolver ~get_imported_names in
  let var1 = Variable.create "var1" in
  let var1' = Var_in_binding_pos.create var1 Name_mode.normal in
  let env =
    TE.add_definition env (Name_in_binding_pos.var var1') K.value
  in
  let env =
    TE.add_equation env (Name.var var1)
      (T.immutable_block ~is_unique:false Tag.zero ~field_kind:K.value
        ~fields:[T.any_tagged_immediate ()])
  in
  let var2 = Variable.create "var2" in
  let var2' = Var_in_binding_pos.create var2 Name_mode.normal in
  let env =
    TE.add_definition env (Name_in_binding_pos.var var2') K.value
  in
  let first_type_for_var2 = T.alias_type_of K.value (Simple.var var1) in
  let env = TE.add_equation env (Name.var var2) first_type_for_var2 in
  let symbol =
    Symbol.create (Compilation_unit.get_current_exn ())
      (Linkage_name.create "my_symbol")
  in
  let env =
    TE.add_definition env (Name_in_binding_pos.symbol symbol) K.value
  in
  Format.eprintf "Initial situation:@ %a\n%!" TE.print env;
  let new_type_for_var2 = T.alias_type_of K.value (Simple.symbol symbol) in
  Format.eprintf "New knowledge:@ %a : %a\n%!"
    Variable.print var2
    T.print new_type_for_var2;
  match T.meet env first_type_for_var2 new_type_for_var2 with
  | Bottom -> assert false
  | Ok (meet_ty, env_extension) ->
    Format.eprintf "Env extension:@ %a\n%!" TEE.print env_extension;
    let env = TE.add_env_extension env env_extension in
    let env = TE.add_equation env (Name.var var2) meet_ty in
    Format.eprintf "Final situation:@ %a\n%!" TE.print env

let test_meet_chains_three_vars () =
  let env = TE.create ~resolver ~get_imported_names in
  let var1 = Variable.create "var1" in
  let var1' = Var_in_binding_pos.create var1 Name_mode.normal in
  let env =
    TE.add_definition env (Name_in_binding_pos.var var1') K.value
  in
  let env =
    TE.add_equation env (Name.var var1)
      (T.immutable_block ~is_unique:false Tag.zero ~field_kind:K.value
        ~fields:[T.any_tagged_immediate ()])
  in
  let var2 = Variable.create "var2" in
  let var2' = Var_in_binding_pos.create var2 Name_mode.normal in
  let env =
    TE.add_definition env (Name_in_binding_pos.var var2') K.value
  in
  let first_type_for_var2 = T.alias_type_of K.value (Simple.var var1) in
  let env = TE.add_equation env (Name.var var2) first_type_for_var2 in
  let var3 = Variable.create "var3" in
  let var3' = Var_in_binding_pos.create var3 Name_mode.normal in
  let env =
    TE.add_definition env (Name_in_binding_pos.var var3') K.value
  in
  let first_type_for_var3 = T.alias_type_of K.value (Simple.var var2) in
  let env = TE.add_equation env (Name.var var3) first_type_for_var3 in
  let symbol =
    Symbol.create (Compilation_unit.get_current_exn ())
      (Linkage_name.create "my_symbol")
  in
  let env =
    TE.add_definition env (Name_in_binding_pos.symbol symbol) K.value
  in
  Format.eprintf "Initial situation:@ %a\n%!" TE.print env;
  let new_type_for_var3 = T.alias_type_of K.value (Simple.symbol symbol) in
  Format.eprintf "New knowledge:@ %a : %a\n%!"
    Variable.print var3
    T.print new_type_for_var3;
  match T.meet env first_type_for_var3 new_type_for_var3 with
  | Bottom -> assert false
  | Ok (meet_ty, env_extension) ->
    Format.eprintf "Env extension:@ %a\n%!" TEE.print env_extension;
    let env = TE.add_env_extension env env_extension in
    let env = TE.add_equation env (Name.var var3) meet_ty in
    Format.eprintf "Final situation:@ %a\n%!" TE.print env

let meet_variants_don't_loose_aliases () =
  let env = TE.create ~resolver ~get_imported_names in
  let define env v =
      let v' = Var_in_binding_pos.create v Name_mode.normal in
      TE.add_definition env (Name_in_binding_pos.var v') K.value
  in
  let defines env l = List.fold_left define env l in
  let vx = Variable.create "x" in
  let vy = Variable.create "y" in
  let va = Variable.create "a" in
  let vb = Variable.create "b" in
  let v_variant = Variable.create "variant" in
  let env = defines env [vx; vy; va; vb; v_variant] in


  let const_ctors = T.bottom K.naked_immediate in
  let ty1 =
    let non_const_ctors =
      Tag.Scannable.Map.of_list [
        Tag.Scannable.create_exn 0, [ T.alias_type_of K.value (Simple.var vx) ];
        Tag.Scannable.create_exn 1, [ T.alias_type_of K.value (Simple.var vy) ];
      ]
    in
    T.variant ~const_ctors ~non_const_ctors in
  let ty2 =
    let non_const_ctors =
      Tag.Scannable.Map.of_list [
        Tag.Scannable.create_exn 0, [ T.alias_type_of K.value (Simple.var va) ];
        Tag.Scannable.create_exn 1, [ T.alias_type_of K.value (Simple.var vb) ];
      ]
    in
    T.variant ~const_ctors ~non_const_ctors in
  match T.meet env ty1 ty2 with
  | Bottom -> assert false
  | Ok (meet_ty, env_extension) ->
    Format.eprintf "@[<hov 2>Meet:@ %a@ /\\@ %a =>@ %a +@ %a@]@."
      T.print ty1 T.print ty2
      T.print meet_ty TEE.print env_extension;
    (* Env extension should be empty *)
    let env = TE.add_equation env (Name.var v_variant) meet_ty in
    let t_get_tag = T.get_tag_for_block ~block:(Simple.var v_variant) in
    let t_tag_1 = T.this_naked_immediate Target_imm.one in
    match T.meet env t_get_tag t_tag_1 with
    | Bottom -> assert false
    | Ok (tag_meet_ty, tag_meet_env_extension) ->
      Format.eprintf "t_get_tag: %a@.t_tag: %a@."
        T.print t_get_tag
        T.print t_tag_1;
      Format.eprintf "@[<hov 2>meet:@ %a@]@.@[<hov 2>env_extension:@ %a@]@."
        T.print tag_meet_ty
        TEE.print tag_meet_env_extension

let () =
  let comp_unit =
    Compilation_unit.create (Ident.create_persistent "Meet_test")
      (Linkage_name.create "meet_test")
  in
  Compilation_unit.set_current comp_unit;
  Format.eprintf "MEET CHAINS WITH TWO VARS\n\n%!";
  test_meet_chains_two_vars ();
  Format.eprintf "\nMEET CHAINS WITH THREE VARS\n\n%!";
  test_meet_chains_three_vars ();
  Format.eprintf "@.MEET VARIANT@.@.";
  meet_variants_don't_loose_aliases ()
