(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Chambart and Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2021--2021 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Typedefs *)
(* ******** *)

type elt = {
  continuation : Continuation.t;
  params : Variable.t list;
  used_in_handler : Name_occurrences.t;
  apply_cont_args :
    Name_occurrences.t Numbers.Int.Map.t Continuation.Map.t;
}

type t = {
  stack : elt list;
  map : elt Continuation.Map.t;
}

(* Print *)
(* ***** *)

let print_elt ppf { continuation; params; used_in_handler; apply_cont_args; } =
  Format.fprintf ppf "@[<hov 1>(\
                      @[<hov 1>(continuation %a)@]@ \
                      @(<hov 1>(params %a)@]@ \
                      @[<hov 1>(used_in_handler %a)@]@ \
                      @[<hov 1>(apply_cont_args %a)@]\
                      )@]"
    Continuation.print continuation
    Variable.print_list params
    Name_occurrences.print used_in_handler
    (Continuation.Map.print (Numbers.Int.Map.print Name_occurrences.print))
    apply_cont_args

let print_stack ppf stack =
  Format.fprintf ppf "@[<v 1>(%a)@]"
    (Format.pp_print_list print_elt ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ "))
    stack

let print_map ppf map =
  Continuation.Map.print print_elt ppf map

let print ppf { stack; map; } =
  Format.fprintf ppf "@[<hov 1>(\
                      @[<hov 1>(stack %a)@]@ \
                      @[<hov 1>(map %a)@]\
                      )@]"
    print_stack stack
    print_map map

(* Creation *)
(* ******** *)

let empty = {
  stack = [];
  map = Continuation.Map.empty;
}

(* Updates *)
(* ******* *)

let stack_cont continuation params t =
  let elt = {
    continuation; params;
    used_in_handler = Name_occurrences.empty;
    apply_cont_args = Continuation.Map.empty;
  }
  in
  { t with stack = elt :: t.stack; }

let unstack_cont cont t =
  match t.stack with
  | [] -> Misc.fatal_errorf "empty stack of variable uses in flambda2"
  | ({ continuation; _ } as elt) :: stack ->
    assert (Continuation.equal cont continuation);
    let map = Continuation.Map.add cont elt t.map in
    { stack; map; }

let update_top_of_stack ~t ~f =
  match t.stack with
  | [] -> Misc.fatal_errorf "empty stack of variable uses in flambda2"
  | elt :: stack -> { t with stack = f elt :: stack; }

let add_used_in_current_handler name_occurrences t =
  update_top_of_stack ~t ~f:(fun elt ->
    let used_in_handler =
      Name_occurrences.union elt.used_in_handler name_occurrences
    in
    { elt with used_in_handler; }
  )

let add_apply_cont_args cont arg_name_occurrences t =
  update_top_of_stack ~t ~f:(fun elt ->
    let apply_cont_args =
      Continuation.Map.update cont (fun map_opt ->
        let map = Option.value ~default:Numbers.Int.Map.empty map_opt in
        let map, _ = List.fold_left (fun (map, i) name_occurrences ->
          let map =
            Numbers.Int.Map.update i (fun old_opt ->
              let old = Option.value ~default:Name_occurrences.empty old_opt in
              Some (Name_occurrences.union old name_occurrences)
            ) map
          in
          map, i + 1
          ) (map, 0) arg_name_occurrences
        in
        Some map
      ) elt.apply_cont_args
    in
    { elt with apply_cont_args; }
  )

