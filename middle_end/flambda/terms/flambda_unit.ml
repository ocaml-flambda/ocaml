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

open! Flambda.Import

type t = {
  return_continuation : Continuation.t;
  exn_continuation : Continuation.t;
  body : Flambda.Expr.t;
  module_symbol : Symbol.t;
}

let create ~return_continuation ~exn_continuation ~body ~module_symbol =
  { return_continuation;
    exn_continuation;
    body;
    module_symbol;
  }

let return_continuation t = t.return_continuation
let exn_continuation t = t.exn_continuation
let body t = t.body
let module_symbol t = t.module_symbol

let print ppf
      { return_continuation; exn_continuation; body; module_symbol;
      } =
  Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(module_symbol@ %a)@]@ \
        @[<hov 1>(return_continuation@ %a)@]@ \
        @[<hov 1>(exn_continuation@ %a)@]@ \
        @[<hov 1>%a@]\
      )@]"
    Symbol.print module_symbol
    Continuation.print return_continuation
    Continuation.print exn_continuation
    Flambda.Expr.print body

let invariant _t = ()

let used_closure_vars t =
  Name_occurrences.closure_vars (Flambda.Expr.free_names t.body)

(* Iter on all sets of closures of a given program. *)
(* CR mshinwell: These functions should be pushed directly into [Flambda] *)
module Iter = struct
  let rec expr f_c f_s e =
    match (Expr.descr e : Expr.descr) with
    | Let e' -> let_expr f_c f_s e'
    | Let_cont e' -> let_cont f_c f_s e'
    | Apply e' -> apply_expr f_c f_s e'
    | Apply_cont e' -> apply_cont f_c f_s e'
    | Switch e' -> switch f_c f_s e'
    | Invalid e' -> invalid f_c f_s e'

  and named let_expr (bindable_let_bound : Bindable_let_bound.t) f_c f_s n =
    match (n : Named.t) with
    | Simple _ | Prim _ -> ()
    | Set_of_closures s ->
        f_s ~closure_symbols:None s
    | Static_consts consts ->
      match bindable_let_bound with
      | Symbols { bound_symbols; _ } ->
        static_consts f_c f_s bound_symbols consts
      | Singleton _ | Set_of_closures _ ->
        Misc.fatal_errorf "[Static_const] can only be bound to [Symbols]:@ %a"
          Let.print let_expr

  and let_expr f_c f_s t =
    Let.pattern_match t ~f:(fun bindable_let_bound ~body ->
        let e = Let.defining_expr t in
        named t bindable_let_bound f_c f_s e;
        expr f_c f_s body
      )

  and let_cont f_c f_s = function
    | Let_cont.Non_recursive { handler; _ } ->
        Non_recursive_let_cont_handler.pattern_match handler ~f:(fun k ~body ->
            let h = Non_recursive_let_cont_handler.handler handler in
            let_cont_aux f_c f_s k h body
          )
    | Let_cont.Recursive handlers ->
        Recursive_let_cont_handlers.pattern_match handlers ~f:(fun ~body conts ->
            assert (not (Continuation_handlers.contains_exn_handler conts));
            let_cont_rec f_c f_s conts body
          )

  and let_cont_aux f_c f_s k h body =
    continuation_handler f_c f_s k h;
    expr f_c f_s body

  and let_cont_rec f_c f_s conts body =
    let map = Continuation_handlers.to_map conts in
    Continuation.Map.iter (continuation_handler f_c f_s) map;
    expr f_c f_s body

  and continuation_handler f_c f_s _ h =
    let h = Continuation_handler.params_and_handler h in
    Continuation_params_and_handler.pattern_match h ~f:(fun _ ~handler ->
        expr f_c f_s handler
      )

  (* Expression application, continuation application and Switches
     only use single expressions and continuations, so no sets_of_closures
     can syntatically appear inside. *)
  and apply_expr _ _ _ = ()

  and apply_cont _ _ _ = ()

  and switch _ _ _ = ()

  and invalid _ _ _ = ()

  and static_consts f_c f_s bound_symbols static_consts =
    Static_const.Group.match_against_bound_symbols static_consts bound_symbols
      ~init:()
      ~code:(fun () code_id (code : Code.t) ->
        f_c ~id:code_id code;
        match Code.params_and_body code with
        | Deleted -> ()
        | Present params_and_body ->
          Function_params_and_body.pattern_match params_and_body
            ~f:(fun ~return_continuation:_ _ _ ~body ~my_closure:_ ->
                expr f_c f_s body))
      ~set_of_closures:(fun () ~closure_symbols set_of_closures ->
        f_s ~closure_symbols:(Some closure_symbols) set_of_closures)
      ~block_like:(fun () _ _ -> ())
end

let ignore_code ~id:_ _ = ()
let ignore_set_of_closures ~closure_symbols:_ _ = ()

let iter ?(code = ignore_code) ?(set_of_closures = ignore_set_of_closures) t =
  Iter.expr code set_of_closures t.body
