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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

let rebuild_one_continuation_handler cont ~at_unit_toplevel
      (recursive : Recursive.t) (cont_handler : CH.t) ~params ~env_extension
      ~(extra_params_and_args : EPA.t) ~is_single_inlinable_use handler uacc
      ~after_rebuild =
  let handler, uacc =
    let params = params @ extra_params_and_args.extra_params in
    (* We might need to place lifted constants now, as they could
       depend on continuation parameters.  As such we must also compute
       the unused parameters after placing any constants! *)
    if (not at_unit_toplevel)
      || List.compare_length_with params 0 = 0
    then handler, uacc
    else
      EB.place_lifted_constants uacc
        Dominator
        ~lifted_constants_from_defining_expr:LCS.empty
        ~lifted_constants_from_body:(UA.lifted_constants uacc)
        ~put_bindings_around_body:(fun uacc ~body -> body, uacc)
        ~body:handler
        ~critical_deps_of_bindings:(KP.List.free_names params)
  in
  let free_names = UA.name_occurrences uacc in
  let uacc, params' =
    (* Removal of unused parameters of recursive continuations is not
       currently supported. *)
    match recursive with
    | Recursive ->
      (* In the recursive case, we have already added an apply_cont_rewrite
         for the recursive continuation to eliminate unused parameters in its
         handler. *)
      begin match UE.find_apply_cont_rewrite (UA.uenv uacc) cont with
      | None ->
        Misc.fatal_errorf "An apply cont rewrite for the recursive continuation \
                          %a should have already been added" Continuation.print cont
      | Some rewrite ->
        let used_params_set = Apply_cont_rewrite.used_params rewrite in
        let used_params =
          List.filter (fun param -> KP.Set.mem param used_params_set) params
        in
        let used_extra_params = Apply_cont_rewrite.extra_params rewrite in
        uacc, used_params @ used_extra_params
      end
    | Non_recursive ->
      (* If the continuation is going to be inlined out, we don't need to
         spend time here calculating unused parameters, since the creation of
         [Let]-expressions around the continuation's handler will do that
         anyway. *)
      let used_extra_params =
        if is_single_inlinable_use then extra_params_and_args.extra_params
        else
          List.filter (fun extra_param ->
            Name_occurrences.mem_var free_names (KP.var extra_param))
            extra_params_and_args.extra_params
      in
      let used_params =
        if is_single_inlinable_use then params
        else
          let first = ref true in
          List.filter (fun param ->
            (* CR mshinwell: We should have a robust means of propagating which
               parameter is the exception bucket.  Then this hack can be
               removed. *)
            if !first && Continuation.is_exn cont then begin
              first := false;
              true
            end else begin
              first := false;
              let tmp = Variable.Set.mem (KP.var param) (UA.used_continuation_params uacc) in
              let occ = Name_occurrences.mem_var free_names (KP.var param) in
              if not tmp && occ then
                  Format.eprintf "(non-rec) rec-unused: %a in %a@."
                    KP.print param Continuation.print cont;
              occ && tmp
            end)
            params
      in
      let rewrite =
        Apply_cont_rewrite.create ~original_params:params
          ~used_params:(KP.Set.of_list used_params)
          ~extra_params:extra_params_and_args.extra_params
          ~extra_args:extra_params_and_args.extra_args
          ~used_extra_params:(KP.Set.of_list used_extra_params)
      in
      let uacc =
        UA.map_uenv uacc ~f:(fun uenv ->
          UE.add_apply_cont_rewrite uenv cont rewrite)
      in
      uacc, used_params @ used_extra_params
  in
  (* TODO: filter the env_extension to only keep equations that refer to
     parameters that are kept/used. *)
  let cont_handler =
    CH.create params' ~handler ~free_names_of_handler:(Known free_names)
      ~is_exn_handler:(CH.is_exn_handler cont_handler)
      ~env_extension
  in
  after_rebuild cont_handler ~params:params' ~handler
    ~free_names_of_handler:free_names uacc

let simplify_one_continuation_handler dacc cont ~at_unit_toplevel recursive
      cont_handler ~params ~env_extension ~handler
      ~is_single_inlinable_use ~down_to_up =
  let dacc = DA.extend_typing_environment dacc env_extension in
  Simplify_expr.simplify_expr dacc handler
    ~down_to_up:(fun dacc ~rebuild ->
      down_to_up dacc ~rebuild:(fun uacc ~extra_params_and_args ~after_rebuild ->
        (* The name occurrences component of this [uacc] is cleared (see
           further down this file) before simplifying a handler.  This is done
           so we can precisely identify the free names of the handler. *)
        assert (Name_occurrences.is_empty (UA.name_occurrences uacc));
        rebuild uacc ~after_rebuild:(fun handler uacc ->
          rebuild_one_continuation_handler cont ~at_unit_toplevel recursive
            cont_handler ~params ~env_extension ~extra_params_and_args
            ~is_single_inlinable_use handler uacc ~after_rebuild)))

let rebuild_non_recursive_let_cont_handler cont
      (uses : Continuation_env_and_param_types.t) ~params ~handler
      ~free_names_of_handler ~is_single_inlinable_use ~is_single_use scope
      (extra_params_and_args : EPA.t) cont_handler uacc ~after_rebuild =
  let uenv = UA.uenv uacc in
  let uenv =
    (* CR mshinwell: Change types so that [free_names_of_handler] only
       needs to be provided in the [Uses] case. *)
    match uses with
    | No_uses -> uenv
    | Uses _ ->
      (* We must make the final decision now as to whether to inline this
         continuation or not; we can't wait until
         [Simplify_apply_cont.rebuild_apply_cont] because we need to decide
         sooner than that whether to keep the [Let_cont] (in order to keep
         free name sets correct). *)
      if is_single_inlinable_use then begin
        (* Note that [Continuation_uses] won't set [is_single_inlinable_use]
           if [cont] is an exception handler. *)
        assert (not (CH.is_exn_handler cont_handler));
        let arity = CH.arity cont_handler in
        (* We pass the parameters and the handler expression, rather than
           the [CH.t], to avoid re-opening the name abstraction. *)
        UE.add_linearly_used_inlinable_continuation uenv cont scope arity
          ~params ~handler ~free_names_of_handler
      end else begin
        match CH.behaviour cont_handler with
        | Unreachable { arity; } ->
          UE.add_unreachable_continuation uenv cont scope arity
        | Alias_for { arity; alias_for; } ->
          UE.add_continuation_alias uenv cont arity ~alias_for
        | Unknown { arity; } ->
          if is_single_use then
            UE.add_continuation_with_handler uenv cont scope arity cont_handler
          else
            UE.add_continuation uenv cont scope arity
      end
  in
  (* The parameters are removed from the free name information as they are no
     longer in scope. *)
  let uacc =
    let name_occurrences =
      ListLabels.fold_left (params @ EPA.extra_params extra_params_and_args)
        ~init:(UA.name_occurrences uacc)
        ~f:(fun name_occurrences param ->
          KP.var param
          |> Name_occurrences.remove_var name_occurrences)
    in
    UA.with_name_occurrences uacc ~name_occurrences
  in
  after_rebuild cont_handler (UA.with_uenv uacc uenv)

let simplify_non_recursive_let_cont_handler ~denv_before_body ~dacc_after_body
      cont params ~env_extension ~(handler : Expr.t) cont_handler ~prior_lifted_constants
      ~inlining_state_at_let_cont ~inlined_debuginfo_at_let_cont
      ~scope ~is_exn_handler ~denv_for_toplevel_check ~unit_toplevel_exn_cont
      ~prior_cont_uses_env ~down_to_up =
  let cont_uses_env = DA.continuation_uses_env dacc_after_body in
  let code_age_relation_after_body =
    TE.code_age_relation (DA.typing_env dacc_after_body)
  in
  let consts_lifted_during_body = DA.get_lifted_constants dacc_after_body in
  let uses =
    CUE.compute_handler_env cont_uses_env cont ~params
      (* CR mshinwell: rename this parameter, the env does not
         have the constants in it now *)
      ~env_at_fork_plus_params_and_consts:denv_before_body
      ~consts_lifted_during_body
      ~code_age_relation_after_body
  in
  let dacc =
    (* CR mshinwell: Improve function names to clarify that this
       function (unlike the function of the same name in [DE])
       does not add to the environment, only to the accumulator. *)
    DA.add_lifted_constants dacc_after_body prior_lifted_constants
  in
  match uses with
  | No_uses ->
    (* Don't simplify the handler if there aren't any uses:
       otherwise, its code will be deleted but any continuation
       usage information collected during its simplification will
       remain. *)
    let cont_uses_env =
      CUE.union prior_cont_uses_env (CUE.remove cont_uses_env cont)
    in
    let dacc = DA.with_continuation_uses_env dacc ~cont_uses_env in
    down_to_up dacc
      ~continuation_has_zero_uses:true
      ~rebuild:(fun uacc ~after_rebuild ->
        rebuild_non_recursive_let_cont_handler cont uses ~params
          ~handler ~free_names_of_handler:Name_occurrences.empty
          ~is_single_inlinable_use:false ~is_single_use:false scope
          EPA.empty cont_handler uacc ~after_rebuild)
  | Uses { handler_env; arg_types_by_use_id; extra_params_and_args;
           is_single_inlinable_use; is_single_use; } ->
    let handler_env, extra_params_and_args =
      (* Unbox the parameters of the continuation if possible.
         Any such unboxing will induce a rewrite (or wrapper) on
         the application sites of the continuation. *)
      match Continuation.sort cont with
      | Normal when is_single_inlinable_use ->
        assert (not is_exn_handler);
        handler_env, extra_params_and_args
      | Normal | Define_root_symbol ->
        assert (not is_exn_handler);
        let param_types =
          TE.find_params (DE.typing_env handler_env) params
        in
        let handler_env, _, extra_params_and_args =
          Unbox_continuation_params.make_unboxing_decisions handler_env
            ~arg_types_by_use_id ~params ~param_types extra_params_and_args
        in
        handler_env, extra_params_and_args
      | Return | Toplevel_return ->
        assert (not is_exn_handler);
        handler_env, extra_params_and_args
      | Exn ->
        assert is_exn_handler;
        handler_env, extra_params_and_args
    in
    let dacc = DA.map_rec_uses dacc ~f:(
      Rec_uses.add_extra_params_and_args cont extra_params_and_args
    ) in
    let at_unit_toplevel =
      (* We try to show that [handler] postdominates [body] (which is done by
         showing that [body] can only return through [cont]) and that if [body]
         raises any exceptions then it only does so to toplevel. If this can be
         shown and we are currently at the toplevel of a compilation unit, the
         handler for the environment can remain marked as toplevel (and suitable
         for "let symbol" bindings); otherwise, it cannot. *)
      DE.at_unit_toplevel denv_for_toplevel_check
        && (not (CH.is_exn_handler cont_handler))
        && Continuation.Set.subset
          (CUE.all_continuations_used cont_uses_env)
          (Continuation.Set.of_list [cont; unit_toplevel_exn_cont])
    in
    let dacc =
      let cont_uses_env =
        CUE.union prior_cont_uses_env (CUE.remove cont_uses_env cont)
      in
      let dacc = DA.with_continuation_uses_env dacc ~cont_uses_env in
      let denv =
        (* Install the environment arising from the join into [dacc].  Note
           that this environment doesn't just contain the joined types; it may
           also contain definitions of code that were produced during
           simplification of the body.  (The [DE] component of [dacc_after_body]
           is discarded since we are now moving into a different scope.) *)
        DE.set_at_unit_toplevel_state handler_env at_unit_toplevel
      in
      let denv =
        if not at_unit_toplevel then denv
        else DE.mark_parameters_as_toplevel denv params
      in
      let denv =
        (* In the case where the continuation is going to be inlined, [denv] is
           basically the use environment, which might have a deeper inlining
           depth increment (e.g. where an [Apply] was inlined, revealing the
           linear inlinable use of the continuation).  We need to make sure the
           handler is simplified using the depth at the [Let_cont]. *)
        DE.set_inlining_state denv inlining_state_at_let_cont
      in
      (* Likewise, the inlined debuginfo may need restoring. *)
      DE.set_inlined_debuginfo denv inlined_debuginfo_at_let_cont
      |> DA.with_denv dacc
    in
    simplify_one_continuation_handler dacc cont ~at_unit_toplevel
      Non_recursive cont_handler ~params ~env_extension ~handler
      ~is_single_inlinable_use ~down_to_up:(fun dacc ~rebuild ->
        down_to_up dacc ~continuation_has_zero_uses:false
          ~rebuild:(fun uacc ~after_rebuild ->
            rebuild uacc ~extra_params_and_args ~after_rebuild:(fun cont_handler ~params
                  ~handler ~free_names_of_handler uacc ->
              rebuild_non_recursive_let_cont_handler cont uses ~params ~handler
                ~free_names_of_handler ~is_single_inlinable_use ~is_single_use
                scope extra_params_and_args cont_handler uacc ~after_rebuild)))

let simplify_non_recursive_let_cont dacc non_rec ~down_to_up =
  let cont_handler = Non_recursive_let_cont_handler.handler non_rec in
  Non_recursive_let_cont_handler.pattern_match non_rec ~f:(fun cont ~body ->
    let denv = DA.denv dacc in
    let denv_for_toplevel_check = denv in
    let unit_toplevel_exn_cont = DE.unit_toplevel_exn_continuation denv in
    let dacc, prior_lifted_constants =
      (* We clear the lifted constants accumulator so that we can easily
         obtain, below, any constants that are generated during the
         simplification of the [body].  We will add these
         [prior_lifted_constants] back into [dacc] later. *)
      DA.get_and_clear_lifted_constants dacc
    in
    let inlining_state_at_let_cont = DE.get_inlining_state (DA.denv dacc) in
    let inlined_debuginfo_at_let_cont =
      DE.get_inlined_debuginfo (DA.denv dacc)
    in
    let scope = DE.get_continuation_scope_level (DA.denv dacc) in
    let is_exn_handler = CH.is_exn_handler cont_handler in
    CH.pattern_match cont_handler ~f:(fun params ~env_extension ~handler ->
      let denv_before_body =
        (* We add the parameters assuming that none of them are at toplevel.
           When we do the toplevel calculation before simplifying the
           handler, we will mark any of the parameters that are in fact at
           toplevel as such. *)
        DE.add_parameters_with_unknown_types (DA.denv dacc) params
          ~at_unit_toplevel:false
      in
      let dacc_for_body =
        DE.increment_continuation_scope_level denv_before_body
        |> DA.with_denv dacc
      in
      let prior_cont_uses_env = DA.continuation_uses_env dacc_for_body in
      let dacc_for_body =
        DA.with_continuation_uses_env dacc_for_body ~cont_uses_env:CUE.empty
      in
      assert (DA.no_lifted_constants dacc_for_body);
      (* First the downwards traversal is done on the body. *)
      Simplify_expr.simplify_expr dacc_for_body body
        ~down_to_up:(fun dacc_after_body ~rebuild:rebuild_body ->
          let dacc_after_body =
            DA.map_rec_uses dacc_after_body ~f:(
              Rec_uses.stack_cont cont (Kinded_parameter.List.vars params)
            )
          in
          (* Then, before the upwards traversal of the body, we do the
             downwards traversal of the handler. *)
          simplify_non_recursive_let_cont_handler ~denv_before_body
            ~dacc_after_body cont params ~env_extension ~handler cont_handler
            ~prior_lifted_constants ~inlining_state_at_let_cont
            ~inlined_debuginfo_at_let_cont ~scope ~is_exn_handler
            ~denv_for_toplevel_check ~unit_toplevel_exn_cont
            ~prior_cont_uses_env
            (* After doing the downwards traversal of the handler, we continue
               the downwards traversal of any surrounding expression (which
               would have to be a [Let_cont]; as such, there's no problem
               with returning the [DE] from the [handler] inside [dacc]
               since it will be replaced by the one from the surrounding
               context). *)
            ~down_to_up:(fun dacc ~continuation_has_zero_uses
                          ~rebuild:rebuild_handler ->
              let dacc = DA.map_rec_uses dacc ~f:(Rec_uses.unstack_cont cont) in
              down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
                let uenv_without_cont = UA.uenv uacc in
                (* Now, on the upwards traversal, the handler is rebuilt.
                   We need to be careful with the free name information
                   returned in [uacc] in two ways:
                   - Observe that linear inlining of the continuation doesn't
                     change the free names of the whole [Let_cont] (so nothing
                     extra to do here).
                   - If the continuation has zero uses, we must not
                     count the free names of the handler, as it will be
                     removed. *)
                let name_occurrences_subsequent_exprs =
                  UA.name_occurrences uacc
                in
                let uacc = UA.clear_name_occurrences uacc in
                rebuild_handler uacc ~after_rebuild:(fun handler uacc ->
                  let name_occurrences_handler =
                    if continuation_has_zero_uses then Name_occurrences.empty
                    else UA.name_occurrences uacc
                  in
                  let uacc = UA.clear_name_occurrences uacc in
                  (* Having rebuilt the handler, we now rebuild the body. *)
                  rebuild_body uacc ~after_rebuild:(fun body uacc ->
                    let name_occurrences_body = UA.name_occurrences uacc in
                    let num_free_occurrences_of_cont_in_body =
                      (* Note that this does not count uses in trap actions. *)
                      Name_occurrences.count_continuation
                        name_occurrences_body
                        cont
                    in
                    let is_applied_with_traps =
                      Name_occurrences.continuation_is_applied_with_traps
                        name_occurrences_body
                        cont
                    in
                    let remove_let_cont_leaving_body =
                      match num_free_occurrences_of_cont_in_body with
                      | Zero -> true
                      | One | More_than_one -> false
                    in
                    (* We are passing back over a binder, so remove the
                       bound continuation from the free name information.
                       Then compute the free names of the whole [Let_cont]. *)
                    let name_occurrences_body =
                      Name_occurrences.remove_continuation
                        name_occurrences_body cont
                    in
                    (* Having rebuilt both the body and handler, the [Let_cont]
                       expression itself is rebuilt -- unless either the
                       continuation had zero uses, in which case we're left
                       with the body; or if the body is just an [Apply_cont]
                       (with no trap action) of [cont], in which case we're
                       left with the handler.
                       The upwards environment of [uacc] is replaced so that
                       out-of-scope continuation bindings do not end up in the
                       accumulator. *)
                    let uacc = UA.with_uenv uacc uenv_without_cont in
                    let expr, uacc =
                      if remove_let_cont_leaving_body then
                        let uacc =
                          let name_occurrences =
                            Name_occurrences.union name_occurrences_body
                              name_occurrences_subsequent_exprs
                          in
                          UA.with_name_occurrences uacc ~name_occurrences
                        in
                        body, uacc
                      else
                        (* CR gbury: we prevent removing the [Let_cont] in cases where
                           the env_extension is not empty, because doing so in such cases
                           would make us forget the information in the env_extension.
                           We could remove the [Let_cont] in such cases if there existed
                           a construction in flambda expressions to represent that. *)
                        let remove_let_cont_leaving_handler =
                          match Expr.descr body with
                          | Apply_cont apply_cont ->
                            if not (Continuation.equal cont
                              (Apply_cont.continuation apply_cont))
                            then None
                            else
                              begin match Apply_cont.args apply_cont with
                              | [] ->
                                if Option.is_none
                                     (Apply_cont.trap_action apply_cont) then
                                  CH.pattern_match handler
                                    ~f:(fun _params ~env_extension ~handler ->
                                      if Flambda_type.Typing_env_extension.is_empty env_extension
                                      then Some handler
                                      else None)
                                else None
                              | _::_ -> None
                              end
                          | Let _ | Apply _ | Switch _ | Invalid _
                          | Let_cont _ -> None
                        in
                        match remove_let_cont_leaving_handler with
                        | Some handler ->
                          let uacc =
                            let name_occurrences =
                              Name_occurrences.union name_occurrences_handler
                                name_occurrences_subsequent_exprs
                            in
                            UA.with_name_occurrences uacc ~name_occurrences
                          in
                          handler, uacc
                        | None ->
                          let uacc =
                            let name_occurrences =
                              Name_occurrences.union name_occurrences_body
                                (Name_occurrences.union name_occurrences_handler
                                  name_occurrences_subsequent_exprs)
                            in
                            UA.with_name_occurrences uacc ~name_occurrences
                          in
                          let expr =
                            Let_cont.create_non_recursive' ~cont handler ~body
                              ~num_free_occurrences_of_cont_in_body:
                                (Known num_free_occurrences_of_cont_in_body)
                              ~is_applied_with_traps
                          in
                          expr, uacc
                    in
                    after_rebuild expr uacc)))))))

let rebuild_recursive_let_cont_handlers cont arity ~original_cont_scope_level
      handler uacc ~after_rebuild =
  let uacc =
    UA.map_uenv uacc ~f:(fun uenv ->
      UE.add_continuation_with_handler uenv cont original_cont_scope_level
        arity handler)
  in
  let handlers = Continuation.Map.singleton cont handler in
  after_rebuild handlers uacc

(* This only takes one handler at present since we don't yet support
   simplification of multiple recursive handlers. *)
let simplify_recursive_let_cont_handlers ~denv_before_body ~dacc_after_body
      cont params ~env_extension ~handler cont_handler ~prior_lifted_constants arity
      ~original_cont_scope_level ~down_to_up =
  let dacc_after_body =
    DA.map_rec_uses dacc_after_body ~f:(
      Rec_uses.stack_cont cont (Kinded_parameter.List.vars params)
    )
  in
  let denv, _arg_types =
    (* XXX These don't have the same scope level as the
        non-recursive case *)
    DE.add_parameters_with_unknown_types'
      ~at_unit_toplevel:false denv_before_body params
  in
  let code_age_relation_after_body =
    TE.code_age_relation (DA.typing_env dacc_after_body)
  in
  let denv =
    DA.get_lifted_constants dacc_after_body
    |> DE.add_lifted_constants denv
  in
  let typing_env =
    TE.with_code_age_relation (DE.typing_env denv)
      code_age_relation_after_body
  in
  let denv = DE.with_typing_env denv typing_env in
  let dacc = DA.with_denv dacc_after_body denv in
  let dacc = DA.add_lifted_constants dacc prior_lifted_constants in
  let dacc = DA.map_denv dacc ~f:DE.set_not_at_unit_toplevel in

  let param_types =
    List.map (fun param ->
      Flambda_type.unknown_with_subkind (KP.kind param)
    ) params
  in
  let arg_types_by_use_id =
    List.map (fun _ ->
      Apply_cont_rewrite_id.Map.empty
    ) params
  in
  let denv =
    DE.map_typing_env (DA.denv dacc) ~f:(fun env ->
      Flambda_type.Typing_env.meet_equations_on_params env ~params ~param_types
    )
  in
  (* For simplicity, we currently make the unboxing decision twice.

     Unbox_continuation_params.make_unboxing_decisions does many things:
     - decide which parameters to unbox (all for which a precise enough kind/type is present)
     - extend the denv with the types for the new unboxed parameters
     - fold over the uses id of the continuation to generate the extra arguments
       to add for the new unboxed parameters. This part is used to generate the
       rewrite for each apply_cont of the continuation being defined.
  
     The first two part can be done at this point because we have enough information;
     however in the recursive case, since we haven't yet seen / went down the handler
     of the continuation, we are missing some uses id for the continuation, hence
     the part about generating the corect rewrites must wait until the handler has been
     traversed down. In order to ensure the second unboxing decisions makes the same
     decision, we curently save the denv at this point.
  *)
  let denv_used_for_second_unboxing_decision = denv in
  let denv, extra_env_extension, extra_params =
    Unbox_continuation_params.make_unboxing_decisions denv
      ~arg_types_by_use_id ~params ~param_types Continuation_extra_params_and_args.empty
  in
  let env_extension =
    let or_bottom =
      Flambda_type.Typing_env_extension.meet_using_typing_env
        (DE.typing_env denv) env_extension extra_env_extension
    in
    match or_bottom with
    | Bottom ->
      (* CR: return unreachable instead of erroring *)
      Misc.fatal_errorf "unreachable continuation handler"
    | Ok env_extension -> env_extension
  in
  let dacc = DA.with_denv dacc denv in
  if Continuation_extra_params_and_args.is_empty extra_params then ()
  else
    Format.eprintf "=== SAUCISSE ===@\nextra_params for %a:@ %a@\nextra_env_extension: %a@\n@."
      Continuation.print cont Continuation_extra_params_and_args.print extra_params
      TEE.print extra_env_extension;
  simplify_one_continuation_handler dacc cont
    ~at_unit_toplevel:false Recursive
    cont_handler ~params ~env_extension ~handler
    ~is_single_inlinable_use:false
    ~down_to_up:(fun dacc ~rebuild:rebuild_handler ->
      let dacc = DA.map_rec_uses dacc ~f:(Rec_uses.unstack_cont cont) in
      let cont_uses_env = DA.continuation_uses_env dacc in
      let arg_types_by_use_id =
        match Continuation.Map.find cont (CUE.get_uses cont_uses_env) with
        | exception Not_found ->
          Misc.fatal_errorf "Recursive continuation has no uses"
        | uses -> Continuation_uses.get_arg_types_by_use_id uses
      in
      (* Because we do the unboxing decisions twice, some care must be taken with
         the names of the extra params: each decision generate fresh names. Since
         we have already commited to the 1st batch of fresh names in the first
         decision, we cannot use the names create by the second decision, so here
         we just take the extra args computed, which include the recursive uses of
         the continuation. This *should* be ok since the extra args should only
         refer to names in the typing env *at the use of the apply_cont* and
         not to the extra_params of the continuation. *)
      let _, _, { Continuation_extra_params_and_args.extra_args; _ } =
        Unbox_continuation_params.make_unboxing_decisions
          denv_used_for_second_unboxing_decision
          ~arg_types_by_use_id ~params ~param_types Continuation_extra_params_and_args.empty
      in
      let extra_params_and_args : Continuation_extra_params_and_args.t =
        { extra_params = extra_params.extra_params; extra_args; }
      in
      let dacc = DA.map_rec_uses dacc ~f:(
        Rec_uses.add_extra_params_and_args cont extra_params_and_args
      ) in
      let cont_uses_env = CUE.remove (DA.continuation_uses_env dacc) cont in
      let dacc = DA.with_continuation_uses_env dacc ~cont_uses_env in
      down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
        let used_continuation_params = UA.used_continuation_params uacc in
        let used_params =
          List.filter (fun param ->
            let b = Variable.Set.mem (KP.var param) used_continuation_params in
            if not b then Format.eprintf "(rec) unused : %a@." KP.print param;
            b
          ) params
          |> KP.Set.of_list
        in
        let used_extra_params =
          List.filter (fun param ->
            let b = Variable.Set.mem (KP.var param) used_continuation_params in
            if not b then Format.eprintf "(rec) unused extra : %a@." KP.print param;
            b
          ) extra_params_and_args.extra_params
          |> KP.Set.of_list
        in
        let rewrite =
          Apply_cont_rewrite.create ~original_params:params ~used_params
            ~extra_params:extra_params_and_args.extra_params
            ~extra_args:extra_params_and_args.extra_args
            ~used_extra_params
        in
        let uacc =
          UA.map_uenv uacc ~f:(fun uenv ->
            UE.add_apply_cont_rewrite uenv cont rewrite)
        in
        let uacc =
          UA.map_uenv uacc ~f:(fun uenv ->
            UE.add_continuation uenv cont original_cont_scope_level arity)
        in
        let name_occurrences_subsequent_exprs =
          UA.name_occurrences uacc
        in
        let uacc = UA.clear_name_occurrences uacc in
        rebuild_handler uacc ~extra_params_and_args ~after_rebuild:(fun cont_handler ~params
              ~handler:_ ~free_names_of_handler:_ uacc ->
          let uacc = UA.add_free_names uacc name_occurrences_subsequent_exprs in
          (* The parameters are removed from the free name information as they
             are no longer in scope. *)
          let uacc =
            let name_occurrences =
              ListLabels.fold_left params
                ~init:(UA.name_occurrences uacc)
                ~f:(fun name_occurrences param ->
                  KP.var param
                  |> Name_occurrences.remove_var name_occurrences)
            in
            UA.with_name_occurrences uacc ~name_occurrences
          in
          rebuild_recursive_let_cont_handlers cont arity
            ~original_cont_scope_level cont_handler uacc ~after_rebuild)))

let rebuild_recursive_let_cont ~body handlers ~uenv_without_cont uacc
      ~after_rebuild : Expr.t * UA.t =
  let uacc = UA.with_uenv uacc uenv_without_cont in
  let expr = Flambda.Let_cont.create_recursive handlers ~body in
  after_rebuild expr uacc

(* CR mshinwell: We should not simplify recursive continuations with no
   entry point -- could loop forever.  (Need to think about this again.) *)
let simplify_recursive_let_cont dacc recs ~down_to_up : Expr.t * UA.t =
  let module CH = Continuation_handler in
  Recursive_let_cont_handlers.pattern_match recs ~f:(fun ~body rec_handlers ->
    assert (not (Continuation_handlers.contains_exn_handler rec_handlers));
    let denv_before_body = DA.denv dacc in
    let original_cont_scope_level =
      DE.get_continuation_scope_level denv_before_body
    in
    let handlers = Continuation_handlers.to_map rec_handlers in
    let cont, cont_handler =
      match Continuation.Map.bindings handlers with
      | [] | _ :: _ :: _ ->
        Misc.fatal_error "Support for simplification of multiply-recursive \
          continuations is not yet implemented"
      | [c] -> c
    in
    CH.pattern_match cont_handler ~f:(fun params ~env_extension ~handler ->
      let arity = KP.List.arity_with_subkinds params in
      let dacc =
        DA.map_denv dacc ~f:DE.increment_continuation_scope_level
      in
      let dacc, prior_lifted_constants =
        (* We clear the lifted constants accumulator so that we can easily
           obtain, below, any constants that are generated during the
           simplification of the [body].  We will add these
           [prior_lifted_constants] back into [dacc] later. *)
        DA.get_and_clear_lifted_constants dacc
      in
      Simplify_expr.simplify_expr dacc body
        ~down_to_up:(fun dacc_after_body ~rebuild:rebuild_body ->
          simplify_recursive_let_cont_handlers ~denv_before_body
            ~dacc_after_body cont params ~env_extension ~handler cont_handler
            ~prior_lifted_constants arity ~original_cont_scope_level
            ~down_to_up:(fun dacc ~rebuild:rebuild_handlers ->
              down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
                let uenv_without_cont = UA.uenv uacc in
                rebuild_handlers uacc ~after_rebuild:(fun handlers uacc ->
                  rebuild_body uacc ~after_rebuild:(fun body uacc ->
                    (* We are passing back over a binder, so remove the
                       bound continuation from the free name information. *)
                    let uacc =
                      let name_occurrences =
                        Name_occurrences.remove_continuation
                          (UA.name_occurrences uacc) cont
                      in
                      UA.with_name_occurrences uacc ~name_occurrences
                    in
                    rebuild_recursive_let_cont ~body handlers
                      ~uenv_without_cont uacc ~after_rebuild)))))))

let simplify_let_cont dacc (let_cont : Let_cont.t) ~down_to_up : Expr.t * UA.t =
  match let_cont with
  | Non_recursive { handler; _ } ->
    simplify_non_recursive_let_cont dacc handler ~down_to_up
  | Recursive handlers ->
    simplify_recursive_let_cont dacc handlers ~down_to_up
