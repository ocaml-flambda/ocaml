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

let debug = false

open! Simplify_import

let simplify_toplevel dacc expr ~return_continuation ~return_arity
      exn_continuation ~return_cont_scope ~exn_cont_scope =
  let toplevel_cont = Continuation.create ~name:"toplevel" () in
  let dacc = DA.map_rec_uses dacc ~f:(Rec_uses.init_toplevel toplevel_cont []) in
  if debug then (Format.eprintf "@.@.SIMP_TOPLEVEL:@\n%a@\n%a@\n@."
                   Continuation.print return_continuation
                   Rec_uses.print (DA.rec_uses dacc));
  let expr, uacc =
    Simplify_expr.simplify_expr dacc expr ~down_to_up:(fun dacc ~rebuild ->
      let dacc = DA.map_rec_uses dacc ~f:(Rec_uses.unstack_cont toplevel_cont) in
      let rec_uses = DA.rec_uses dacc in
      if debug then (Format.eprintf "@.@.REC_USES:@\n%a@.@." Rec_uses.print rec_uses);
      let used_continuation_params =
        Rec_uses.analyze rec_uses ~return_continuation
          ~exn_continuation:(Exn_continuation.exn_handler exn_continuation)
      in
      let uenv =
        UE.add_continuation UE.empty return_continuation
          return_cont_scope return_arity
      in
      let uenv =
        UE.add_exn_continuation uenv exn_continuation exn_cont_scope
      in
      let uacc = UA.create ~used_continuation_params uenv dacc in
      rebuild uacc ~after_rebuild:(fun expr uacc -> expr, uacc))
  in
  (* We don't check occurrences of variables or symbols here because the check
     required depends on whether we're dealing with a lambda or the whole
     compilation unit.  Instead these checks are in [Simplify] or
     [Simplify_set_of_closures]. *)
  Name_occurrences.fold_continuations_including_in_trap_actions
    (UA.name_occurrences uacc)
    ~init:()
    ~f:(fun () cont ->
      let exn_continuation =
        Exn_continuation.exn_handler exn_continuation
      in
      if (not (Continuation.equal cont return_continuation))
        && (not (Continuation.equal cont exn_continuation))
      then begin
        Misc.fatal_errorf "Continuation %a should not be free in \
            toplevel expression after simplification (return \
            continuation %a, exn continuation %a):@ %a"
          Continuation.print cont
          Expr.print expr
          Continuation.print return_continuation
          Continuation.print exn_continuation
      end);
  expr, uacc


