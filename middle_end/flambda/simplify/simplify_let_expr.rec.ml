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

let rebuild_let symbol_scoping_rule simplify_named_result
      ~lifted_constants_from_defining_expr ~at_unit_toplevel ~body uacc
      ~after_rebuild =
  (* At this point, the free names in [uacc] are the free names of [body],
     plus all used closure vars seen in the whole compilation unit. *)
  let no_constants_from_defining_expr =
    LCS.is_empty lifted_constants_from_defining_expr
  in
  (* The lifted constants present in [uacc] are the ones arising from
     the simplification of [body] which still have to be placed.  We
     augment these with any constants arising from the simplification of
     the defining expression.  Then we place (some of) them and/or return
     them in [uacc] for an outer [Let]-binding to deal with. *)
  let lifted_constants_from_body = UA.lifted_constants uacc in
  let no_constants_to_place =
    no_constants_from_defining_expr
      && LCS.is_empty lifted_constants_from_body
  in
  let bindings =
    Simplify_named_result.bindings_to_place_in_any_order simplify_named_result
  in
  (* Return as quickly as possible if there is nothing to do.  In this
     case, all constants get floated up to an outer binding. *)
  if no_constants_to_place || not at_unit_toplevel then
    let uacc =
      (* Avoid re-allocating [uacc] unless necessary. *)
      if no_constants_from_defining_expr then uacc
      else
        LCS.union_ordered ~innermost:lifted_constants_from_body
          ~outermost:lifted_constants_from_defining_expr
        |> UA.with_lifted_constants uacc
    in
    let body, uacc =
      EB.make_new_let_bindings uacc ~bindings_outermost_first:bindings ~body
    in
    after_rebuild body uacc
  else
    let scoping_rule =
      (* If this is a "normal" let rather than a "let symbol", then we
         use [Dominator] scoping for any symbol bindings we place, as the
         types of the symbols may have been used out of syntactic scope. *)
      Option.value ~default:Symbol_scoping_rule.Dominator symbol_scoping_rule
    in
    let critical_deps_of_bindings =
      ListLabels.fold_left bindings
        ~init:Name_occurrences.empty
        ~f:(fun critical_deps (bound, _) ->
          Name_occurrences.union (Bindable_let_bound.free_names bound)
            critical_deps)
    in
    let body, uacc =
      EB.place_lifted_constants uacc
        scoping_rule
        ~lifted_constants_from_defining_expr
        ~lifted_constants_from_body
        ~put_bindings_around_body:
          (fun uacc ~body ->
            EB.make_new_let_bindings uacc ~bindings_outermost_first:bindings
              ~body)
        ~body
        ~critical_deps_of_bindings
    in
    after_rebuild body uacc

let simplify_let dacc let_expr ~down_to_up =
  let module L = Flambda.Let in
  L.pattern_match let_expr ~f:(fun bindable_let_bound ~body ->
    let symbol_scoping_rule =
      Bindable_let_bound.let_symbol_scoping_rule bindable_let_bound
    in
    (* Remember then clear the lifted constants memory in [DA] so we can
       easily find out which constants are generated during simplification
       of the defining expression and the [body]. *)
    let dacc, prior_lifted_constants = DA.get_and_clear_lifted_constants dacc in
    (* Simplify the defining expression. *)
    let simplify_named_result =
      Simplify_named.simplify_named dacc bindable_let_bound
        (L.defining_expr let_expr)
    in
    let dacc = Simplify_named_result.dacc simplify_named_result in
    (* Accumulate uses about variables
       CR gbury/pchambart : in the case of an invalid, we currently over-approximate
       the uses. In case of an invalid, we might want to instead flush the uses
       of the current control flow branch (but this would require a more precise stack). *)
    (* We currently over-approximate the use of variables in symbols: both in the lifted
       constants, and in the bound constants, which we consider to be always used,
       leading to the free_names in their defining expressions to be considered as used
       unconditionally. *)
    let free_names_in_lifted_constants =
      Lifted_constant_state.fold (DA.get_lifted_constants dacc)
        ~init:[]
        ~f:(fun acc lifted_constant ->
          Lifted_constant.free_names_of_defining_exprs lifted_constant :: acc
        )
      |> Name_occurrences.union_list
    in
    let dacc = DA.map_rec_uses dacc ~f:(fun rec_uses ->
      let rec_uses = Rec_uses.add_used_in_current_handler
                       free_names_in_lifted_constants rec_uses
      in
      List.fold_left (fun acc (bindable_let_bound, simplified_named) ->
        match (simplified_named : Simplified_named.t) with
        | Invalid _ -> acc
        | Reachable { free_names; named; cost_metrics = _; } ->
          let can_be_removed =
            match named with
            | Simple _
            | Set_of_closures _ -> true
            | Prim (prim, _) ->
              Flambda_primitive.at_most_generative_effects prim
          in
          if not can_be_removed then
            Rec_uses.add_used_in_current_handler free_names acc
          else
            begin match (bindable_let_bound : Bindable_let_bound.t) with
            | Singleton v ->
              Rec_uses.add_binding (Var_in_binding_pos.var v) free_names acc
            | Set_of_closures { closure_vars; name_mode = _; } ->
              List.fold_left (fun acc v ->
                Rec_uses.add_binding (Var_in_binding_pos.var v) free_names acc
              ) acc closure_vars
            | Symbols _ ->
              Rec_uses.add_used_in_current_handler free_names acc
            end
      ) rec_uses (Simplify_named_result.bindings_to_place_in_any_order simplify_named_result)
    )
    in
    (* First remember any lifted constants that were generated during the
       simplification of the defining expression and sort them, since they
       may be mutually recursive.  Then add back in to [dacc]
       the [prior_lifted_constants] remembered above.  This results in the
       definitions and types for all these constants being available at a
       subsequent [Let_cont].  At such a point, [dacc] will be queried to
       retrieve all of the constants, which are then manually transferred
       into the computed [dacc] at the join point for subsequent
       simplification of the continuation handler(s).
       Note that no lifted constants are ever placed during the simplification
       of the defining expression.  (Not even in the case of a
       [Set_of_closures] binding, since "let symbol" is disallowed under a
       lambda.) *)
    let lifted_constants_from_defining_expr =
      Sort_lifted_constants.sort (DA.get_lifted_constants dacc)
    in
    let dacc = DA.add_lifted_constants dacc prior_lifted_constants in
    let at_unit_toplevel = DE.at_unit_toplevel (DA.denv dacc) in
    (* Simplify the body of the let-expression and make the new [Let] bindings
       around the simplified body.  [Simplify_named] will already have
       prepared [dacc] with the necessary bindings for the simplification of
       the body. *)
    Simplify_expr.simplify_expr dacc body
      ~down_to_up:(fun dacc ~rebuild:rebuild_body ->
        down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
          rebuild_body uacc ~after_rebuild:(fun body uacc ->
            rebuild_let symbol_scoping_rule simplify_named_result
              ~lifted_constants_from_defining_expr ~at_unit_toplevel ~body uacc
              ~after_rebuild))))
