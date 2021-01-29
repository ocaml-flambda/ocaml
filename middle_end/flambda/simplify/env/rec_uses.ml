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

(* Functional queues *)
(* ***************** *)

module Queue = struct

  type 'a t = {
    head : 'a list;
    rev_tail : 'a list;
  }

  let empty = {
    head = [];
    rev_tail = [];
  }

  let add x t =
    { t with rev_tail = x :: t.rev_tail; }

  let rec pop t =
    match t.head with
    | x :: head -> Some (x, { t with head; })
    | [] ->
      begin match t.rev_tail with
      | [] -> None
      | l -> pop { head = List.rev l; rev_tail = []; }
      end

end

(* Variable graph *)
(* ************** *)

module Var_graph = struct

  type t = Variable.Set.t Variable.Map.t

  let empty : t = Variable.Map.empty

  let add_edge ~src ~dst t =
    Variable.Map.update src (function
      | None -> Some (Variable.Set.singleton dst)
      | Some set -> Some (Variable.Set.add dst set)
    ) t

  let edges ~src t =
    match Variable.Map.find src t with
    | res -> res
    | exception Not_found -> Variable.Set.empty

  (* breadth-first reachability analysis
     CR gbury: would using sets (and thus union and diff) instead of a
               queue be better ? Enqueuing new nodes would be faster, but
               we'd lose the bread-first aspect, and revert to some kind of
               lexicographic order of traversal. *)
  let rec reachable t visited queue =
    match Queue.pop queue with
    | None -> visited
    | Some (v, queue) ->
      if Variable.Set.mem v visited then
        reachable t visited queue
      else begin
        let visited = Variable.Set.add v visited in
        let queue =
          Variable.Set.fold (fun dst q ->
            if Variable.Set.mem dst visited
            then queue
            else Queue.add dst q
          ) (edges t ~src:v) queue
        in
        reachable t visited queue
      end

end

(* Analysis *)
(* ******** *)

let used_variables map =
  (* Build the reversed graph of dependencies *)
  let graph =
    Continuation.Map.fold (fun _ { apply_cont_args; _ } graph ->
      Continuation.Map.fold (fun k args graph ->
        let params =
          match Continuation.Map.find k map with
          | params -> Array.of_list params
          | exception Not_found ->
            Misc.fatal_errorf "Continuation not found: %a@."
              Continuation.print k
        in
        Numbers.Int.Map.fold (fun i name_occurrence graph ->
          (* CR: is that the correct direction ? *)
          let dst = params.(i) in
          Name_occurrences.fold_variables name_occurrence ~init:graph
            ~f:(fun g src -> Var_graph.add_edge ~src ~dst g)
        ) args graph
      ) apply_cont_args graph
    ) map Var_graph.empty
  in
  let used =
    Continuation.Map.fold (fun _ { used_in_handler; _ } set ->
      Name_occurrences.fold_variables used_in_handler ~init:set
        ~f:(fun set var -> Variable.Set.add var set)
    ) map Variable.Set.empty
  in
  let transitively_used =
    Var_graph.reachable graph Variable.Set.empty
      (Variable.Set.fold Queue.add used Queue.empty)
  in
  transitively_used

let analyze { stack; map; } =
  assert (stack = []);
  let used = used_variables map in
  Format.eprintf "@.@\nUSED VARIABLES:@\n%a@\n@." Variable.Set.print used;
  ()



