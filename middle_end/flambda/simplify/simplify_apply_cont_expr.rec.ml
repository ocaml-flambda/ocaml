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

let inline_linearly_used_continuation uacc ~create_apply_cont ~params ~handler
      ~free_names_of_handler =
  (* CR mshinwell: With -g, we can end up with continuations that are
     just a sequence of phantom lets then "goto".  These would normally
     be treated as aliases, but of course aren't in this scenario,
     unless the continuations are used linearly. *)
  create_apply_cont ~apply_cont_to_expr:(fun apply_cont ->
    assert (Option.is_none (AC.trap_action apply_cont));
    (* We can't easily call [simplify_expr] on the inlined body since
       [dacc] isn't the correct accumulator and environment any more.
       However there's no need to simplify the inlined body except to
       make use of parameter-to-argument bindings; we just leave them for
       a subsequent round of [Simplify] or [Un_cps] to clean up. *)
    let args = AC.args apply_cont in
    if List.compare_lengths params args <> 0 then begin
      Misc.fatal_errorf "Parameter list@ [%a]@ does not match argument \
          list@ [%a]@ when inlining at [Apply_cont]:@ %a@ \
          Handler to inline:@ %a"
        KP.List.print params
        Simple.List.print args
        Apply_cont.print apply_cont
        Expr.print handler
    end;
    let bindings_outermost_first = List.rev (
      ListLabels.fold_left2 params args ~init:[]
        ~f:(fun acc param arg ->
          (* In the case of a pair (param, arg), 'param' might be an unused
             continuation parameter. The recursive uses analysis being global,
             it will sometimes cause removals that will render the right-hand side
             of the binding (i.e. 'arg') invalid because it will refer to another
             variable that has been eliminated. The recursive use anlaysis rely on
             the fact that bindings where 'param' is unused will be removed.

             However, without the explicit check below, such bindings can sometime
             be kept by the code because the parameter variable is user-visible;
             in those cases, the new bound variable becomes phantom, but the right
             hand side of the binding refers to non-existing variables, which
             yield compilation errors. *)
            let used_continuation_params = UA.used_continuation_params uacc in
            let name_mode =
              if Variable.Set.mem (KP.var param) used_continuation_params
              then Name_mode.normal
              else Name_mode.phantom
            in
            let bound =
              Var_in_binding_pos.create (KP.var param) name_mode
              |> Bindable_let_bound.singleton
            in
            (bound, Simplified_named.reachable (Named.create_simple arg)) :: acc
        )
    )
    in
    let expr, uacc =
      let uacc =
        UA.with_name_occurrences uacc ~name_occurrences:free_names_of_handler
      in
      Expr_builder.make_new_let_bindings uacc ~bindings_outermost_first
        ~body:handler
    in
    expr, UA.name_occurrences uacc)

let rebuild_apply_cont apply_cont ~args ~rewrite_id uacc ~after_rebuild =
  let uenv = UA.uenv uacc in
  let cont = AC.continuation apply_cont in
  let rewrite = UE.find_apply_cont_rewrite uenv cont in
  let cont = UE.resolve_continuation_aliases uenv cont in
  let create_apply_cont ~apply_cont_to_expr =
    (* The function returned by this code accepts another function, which
       will be called with the [Apply_cont] expression after subjecting it
       to any rewrites (e.g. adding or removing parameters).  This gives the
       chance of replacing the [Apply_cont] with something else -- in
       particular an inlined continuation -- before it is wrapped in any
       [Let]-expressions needed as a result of the rewrite. *)
    let rewrite_use_result =
      let apply_cont = AC.update_continuation_and_args apply_cont cont ~args in
      let apply_cont =
        match AC.trap_action apply_cont with
        | None -> apply_cont
        | Some (Push { exn_handler; } | Pop { exn_handler; _ }) ->
          if UE.mem_continuation uenv exn_handler then apply_cont
          else AC.clear_trap_action apply_cont
      in
      match rewrite with
      | None -> Apply_cont_rewrite.no_rewrite apply_cont
      | Some rewrite ->
        Apply_cont_rewrite.rewrite_use rewrite rewrite_id apply_cont
    in
    match rewrite_use_result with
    | Apply_cont apply_cont ->
      let expr, free_names = apply_cont_to_expr apply_cont in
      let uacc = UA.add_free_names uacc free_names in
      after_rebuild expr uacc
    | Expr build_expr ->
      let expr, free_names = build_expr ~apply_cont_to_expr in
      let uacc = UA.add_free_names uacc free_names in
      after_rebuild expr uacc
  in
  match UE.find_continuation uenv cont with
  | Linearly_used_and_inlinable { arity = _; params; handler;
      free_names_of_handler; } ->
    (* We must not fail to inline here, since we've already decided that the
       relevant [Let_cont] is no longer needed. *)
    inline_linearly_used_continuation uacc ~create_apply_cont ~params ~handler
      ~free_names_of_handler
  | Unreachable { arity = _; } ->
    (* We allow this transformation even if there is a trap action, on the
       basis that there wouldn't be any opportunity to collect any backtrace,
       even if the [Apply_cont] were compiled as "raise". *)
    after_rebuild (Expr.create_invalid ()) uacc
  | Other { arity = _; handler = _; } ->
    create_apply_cont ~apply_cont_to_expr:(fun apply_cont ->
      Expr.create_apply_cont apply_cont, Apply_cont.free_names apply_cont)

let simplify_apply_cont dacc apply_cont ~down_to_up =
  let min_name_mode = Name_mode.normal in
  match S.simplify_simples dacc (AC.args apply_cont) ~min_name_mode with
  | _, Bottom ->
    down_to_up dacc ~rebuild:Simplify_common.rebuild_invalid
  | _changed, Ok args_with_types ->
    let args, arg_types = List.split args_with_types in
    let dacc = DA.map_rec_uses dacc ~f:(
      Rec_uses.add_apply_cont_args
        (AC.continuation apply_cont)
        (List.map Simple.free_names args)
    ) in
    let use_kind : Continuation_use_kind.t =
      (* CR mshinwell: Is [Continuation.sort] reliable enough to detect
         the toplevel continuation?  Probably not -- we should store it in
         the environment. *)
      match Continuation.sort (AC.continuation apply_cont) with
      | Normal ->
        (* Until such time as we can manually add to the backtrace buffer,
           never substitute a "raise" for the body of an exception handler. *)
        if Option.is_none (Apply_cont.trap_action apply_cont) then Inlinable
        else Non_inlinable
      | Return | Toplevel_return | Exn -> Non_inlinable
      | Define_root_symbol ->
        assert (Option.is_none (Apply_cont.trap_action apply_cont));
        Inlinable
    in
    let dacc, rewrite_id =
      DA.record_continuation_use dacc (AC.continuation apply_cont)
        use_kind ~env_at_use:(DA.denv dacc) ~arg_types
    in
    down_to_up dacc ~rebuild:(rebuild_apply_cont apply_cont ~args ~rewrite_id)
