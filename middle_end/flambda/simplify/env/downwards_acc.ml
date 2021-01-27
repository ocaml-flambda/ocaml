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

module CUE = Continuation_uses_env
module DE = Simplify_envs.Downwards_env
module LCS = Simplify_envs.Lifted_constant_state
module TE = Flambda_type.Typing_env

module Static_const = Flambda.Static_const

type aux = {
  continuation : Continuation.t;
  vars_used_in_expr : Name_occurrences.t;
  vars_as_k_arg : Name_occurrences.t Numbers.Int.Map.t Continuation.Map.t;
}

type var_uses = {
  vars_used_in_expr : Name_occurrences.t Continuation.Map.t;
  (* except for uses as continuation arguments *)
  vars_as_k_arg :
    Name_occurrences.t Numbers.Int.Map.t Continuation.Map.t Continuation.Map.t;
  k_arg_vars : Variable.t list Continuation.Map.t;
}

type t = {
  denv : DE.t;
  continuation_uses_env : CUE.t;
  shareable_constants : Symbol.t Static_const.Map.t;
  used_closure_vars : Name_occurrences.t;
  lifted_constants : LCS.t;

  var_uses : var_uses;
  var_stack : aux List.t;
}


let print_var_uses ppf { vars_used_in_expr; vars_as_k_arg; k_arg_vars } =
  Format.fprintf ppf "@[<hov 1>(\
                      @[<hov 1>(vars_used_in_expr %a)@]@ \
                      @[<hov 1>(vars_as_k_arg %a)@]@ \
                      @[<hov 1>(k_arg_vars %a)@]\
                      )@]"
    (Continuation.Map.print Name_occurrences.print) vars_used_in_expr
    (Continuation.Map.print (
       Continuation.Map.print (Numbers.Int.Map.print Name_occurrences.print)))
    vars_as_k_arg
    (Continuation.Map.print Variable.print_list) k_arg_vars

let print_aux ppf { continuation; vars_used_in_expr; vars_as_k_arg } =
  Format.fprintf ppf "@[<hov 1>(\
                      @[<hov 1>(continuation %a)@]@ \
                      @[<hov 1>(vars_used_in_expr %a)@]@ \
                      @[<hov 1>(vars_as_k_arg %a)@]\
                      )@]"
    Continuation.print continuation
    Name_occurrences.print vars_used_in_expr
    (Continuation.Map.print (Numbers.Int.Map.print Name_occurrences.print))
    vars_as_k_arg


let print_stack ppf stack =
  Format.fprintf ppf "(@[<v>%a@])"
    (Format.pp_print_list print_aux ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ "))
    stack


let print ppf
      { denv; continuation_uses_env; shareable_constants; used_closure_vars;
        lifted_constants; var_uses; var_stack } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(denv@ %a)@]@ \
      @[<hov 1>(continuation_uses_env@ %a)@]@ \
      @[<hov 1>(shareable_constants@ %a)@]@ \
      @[<hov 1>(used_closure_vars@ %a)@]@ \
      @[<hov 1>(lifted_constant_state@ %a)@]\
      @[<hov 1>(var_uses %a)@]@ \
      @[<hov 1>(var_stack (%a))@]\
      )@]"
    DE.print denv
    CUE.print continuation_uses_env
    (Static_const.Map.print Symbol.print) shareable_constants
    Name_occurrences.print used_closure_vars
    LCS.print lifted_constants
    print_var_uses var_uses
    print_stack var_stack

let empty_var_uses = {
  vars_used_in_expr = Continuation.Map.empty;
  vars_as_k_arg = Continuation.Map.empty;
  k_arg_vars = Continuation.Map.empty;
}

let create denv continuation_uses_env =
  { denv;
    continuation_uses_env;
    shareable_constants = Static_const.Map.empty;
    used_closure_vars = Name_occurrences.empty;
    lifted_constants = LCS.empty;
    var_uses = empty_var_uses;
    var_stack = [];
  }

let denv t = t.denv

let var_uses t = t.var_uses

let stack t = t.var_stack

let [@inline always] map_denv t ~f =
  { t with
    denv = f t.denv;
  }

let [@inline always] with_denv t denv =
  { t with
    denv;
  }

let with_continuation_uses_env t ~cont_uses_env =
  { t with
    continuation_uses_env = cont_uses_env;
  }

let record_continuation_use t cont use_kind ~env_at_use
      ~arg_types =
  let cont_uses_env, id =
    CUE.record_continuation_use t.continuation_uses_env cont use_kind
      ~env_at_use ~arg_types
  in
  with_continuation_uses_env t ~cont_uses_env, id

let delete_continuation_uses t cont =
  let cont_uses_env =
    CUE.delete_continuation_uses t.continuation_uses_env cont
  in
  with_continuation_uses_env t ~cont_uses_env

let compute_handler_env t ~env_at_fork_plus_params_and_consts
      ~consts_lifted_during_body cont ~params =
  CUE.compute_handler_env t.continuation_uses_env
    ~env_at_fork_plus_params_and_consts ~consts_lifted_during_body
    cont ~params

let num_continuation_uses t cont =
  CUE.num_continuation_uses t.continuation_uses_env cont

let continuation_uses_env t = t.continuation_uses_env

let code_age_relation t =
  TE.code_age_relation (DE.typing_env (denv t))

let with_code_age_relation t code_age_relation =
  let typing_env =
    TE.with_code_age_relation (DE.typing_env (denv t)) code_age_relation
  in
  with_denv t (DE.with_typing_env (denv t) typing_env)

let typing_env t = DE.typing_env (denv t)

let add_variable t var ty =
  with_denv t (DE.add_variable (denv t) var ty)

let extend_typing_environment t env_extension =
  with_denv t (DE.extend_typing_environment (denv t) env_extension)

let get_typing_env_no_more_than_one_use t k =
  CUE.get_typing_env_no_more_than_one_use t.continuation_uses_env k

let add_lifted_constant t const =
  { t with
    lifted_constants = LCS.add t.lifted_constants const;
  }

let add_lifted_constant_also_to_env t const =
  { t with
    lifted_constants = LCS.add t.lifted_constants const;
    denv = DE.add_lifted_constant t.denv const;
  }

let add_lifted_constants_from_list t consts =
  ListLabels.fold_left consts ~init:t ~f:add_lifted_constant

let add_lifted_constants t constants =
  { t with
    lifted_constants = LCS.union t.lifted_constants constants;
  }

let get_lifted_constants t = t.lifted_constants

let clear_lifted_constants t =
  { t with
    lifted_constants = LCS.empty;
  }

let no_lifted_constants t =
  LCS.is_empty t.lifted_constants

let get_and_clear_lifted_constants t =
  let constants = t.lifted_constants in
  let t = clear_lifted_constants t in
  t, constants

let set_lifted_constants t consts =
  { t with lifted_constants = consts; }

let find_shareable_constant t static_const =
  Static_const.Map.find_opt static_const t.shareable_constants

let consider_constant_for_sharing t symbol static_const =
  if not (Static_const.can_share static_const) then t
  else
    { t with
      shareable_constants =
        Static_const.Map.add static_const symbol t.shareable_constants;
    }

let with_shareable_constants t ~shareable_constants =
  { t with shareable_constants; }

let shareable_constants t = t.shareable_constants

let add_use_of_closure_var t closure_var =
  { t with
    used_closure_vars =
      Name_occurrences.add_closure_var t.used_closure_vars closure_var
        Name_mode.normal;
  }

let used_closure_vars t = t.used_closure_vars

let all_continuations_used t =
  CUE.all_continuations_used t.continuation_uses_env

let with_used_closure_vars t ~used_closure_vars =
  { t with used_closure_vars = used_closure_vars; }


let add_new_cont_for_used_vars t continuation =
  let elt = {
    continuation;
    vars_used_in_expr = Name_occurrences.empty;
    vars_as_k_arg = Continuation.Map.empty;
  } in
  { t with var_stack = elt :: t.var_stack; }

let add_var_used_in_expr t name_occurrences =
  match t.var_stack with
  | [] -> Misc.fatal_errorf "empty stack of variable uses in flambda2"
  | elt :: stack ->
    let elt' = {
      elt with vars_used_in_expr =
                 Name_occurrences.union elt.vars_used_in_expr name_occurrences;
    } in
    { t with var_stack = elt' :: stack; }

let end_cont_for_used_vars t cont arg_vars =
  match t.var_stack with
  | [] -> Misc.fatal_errorf "empty stack of variable uses in flambda2"
  | { continuation; vars_used_in_expr; vars_as_k_arg } :: stack ->
    assert (Continuation.equal cont continuation);
    let vars_used_in_expr =
      Continuation.Map.add
        continuation
        vars_used_in_expr
        t.var_uses.vars_used_in_expr
    in
    let vars_as_k_arg =
      Continuation.Map.add
        continuation
        vars_as_k_arg
        t.var_uses.vars_as_k_arg
    in
    let k_arg_vars =
      Continuation.Map.add
        continuation
        arg_vars
        t.var_uses.k_arg_vars
    in
    { t with
      var_stack = stack;
      var_uses = { vars_used_in_expr; vars_as_k_arg; k_arg_vars; };
    }

let find_nth_arg i map =
  match Numbers.Int.Map.find i map with
  | res -> res
  | exception Not_found -> Name_occurrences.empty

let add_vars_as_k_arg t cont names_occurrences =
  match t.var_stack with
  | [] -> Misc.fatal_errorf "empty stack of variable uses in flambda2"
  | elt :: stack ->
    let map =
      match Continuation.Map.find cont elt.vars_as_k_arg with
      | res -> res
      | exception Not_found -> Numbers.Int.Map.empty
    in
    let map, _ =
      List.fold_left (fun (map, i) name_occurrences ->
        let old = find_nth_arg i map in
        let map' = Numbers.Int.Map.add i (Name_occurrences.union old name_occurrences) map in
        (map', i + 1)
      ) (map, 0) names_occurrences
    in
    let vars_as_k_arg = Continuation.Map.add cont map elt.vars_as_k_arg in
    let elt' = { elt with vars_as_k_arg; } in
    { t with var_stack = elt' :: stack; }

