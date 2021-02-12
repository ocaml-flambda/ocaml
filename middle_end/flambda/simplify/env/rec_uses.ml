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

[@@@ocaml.warning "+a-4-30-40-41-42"]

let debug = true

(* Typedefs *)
(* ******** *)

type elt = {
  continuation : Continuation.t;
  params : Variable.t list;
  used_in_handler : Name_occurrences.t;
  apply_result_conts : Continuation.Set.t;
  bindings : Name_occurrences.t Variable.Map.t;
  apply_cont_args :
    Name_occurrences.t Numbers.Int.Map.t Continuation.Map.t;
}

type t = {
  stack : elt list;
  map : elt Continuation.Map.t;
  extra : Continuation_extra_params_and_args.t Continuation.Map.t;
}

(* Print *)
(* ***** *)

let print_elt ppf
      { continuation; params; used_in_handler;
        apply_result_conts; bindings; apply_cont_args; } =
  Format.fprintf ppf "@[<hov 1>(\
                      @[<hov 1>(continuation %a)@]@ \
                      @[<hov 1>(params %a)@]@ \
                      @[<hov 1>(used_in_handler %a)@]@ \
                      @[<hov 1>(apply_result_conts %a)@]@ \
                      @[<hov 1>(bindings %a)@]@ \
                      @[<hov 1>(apply_cont_args %a)@]\
                      )@]"
    Continuation.print continuation
    Variable.print_list params
    Name_occurrences.print used_in_handler
    Continuation.Set.print apply_result_conts
    (Variable.Map.print Name_occurrences.print) bindings
    (Continuation.Map.print (Numbers.Int.Map.print Name_occurrences.print))
    apply_cont_args

let print_stack ppf stack =
  Format.fprintf ppf "@[<v 1>(%a)@]"
    (Format.pp_print_list print_elt ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ "))
    stack

let print_map ppf map =
  Continuation.Map.print print_elt ppf map

let print_extra ppf extra =
  Continuation.Map.print Continuation_extra_params_and_args.print ppf extra

let print ppf { stack; map; extra } =
  Format.fprintf ppf "@[<hov 1>(\
                      @[<hov 1>(stack %a)@]@ \
                      @[<hov 1>(map %a)@]@ \
                      @[<hov 1>(extra %a)@]\
                      )@]"
    print_stack stack
    print_map map
    print_extra extra

(* Creation *)
(* ******** *)

let empty = {
  stack = [];
  map = Continuation.Map.empty;
  extra = Continuation.Map.empty;
}

(* Updates *)
(* ******* *)

let add_extra_params_and_args cont extra t =
  let extra =
    Continuation.Map.update cont (function
      | Some _ ->
        Misc.fatal_errorf "Continuation extended a second time"
      | None -> Some extra
    ) t.extra
  in
  { t with extra; }

let stack_cont continuation params t =
  let elt = {
    continuation; params;
    bindings = Variable.Map.empty;
    used_in_handler = Name_occurrences.empty;
    apply_cont_args = Continuation.Map.empty;
    apply_result_conts = Continuation.Set.empty;
  }
  in
  { t with stack = elt :: t.stack; }

let init_toplevel continuation params _t =
  stack_cont continuation params empty

let unstack_cont cont t =
  match t.stack with
  | [] -> Misc.fatal_errorf "empty stack of variable uses in flambda2"
  | ({ continuation; _ } as elt) :: stack ->
    assert (Continuation.equal cont continuation);
    let map = Continuation.Map.add cont elt t.map in
    { t with stack; map; }

let update_top_of_stack ~t ~f =
  match t.stack with
  | [] -> Misc.fatal_errorf "empty stack of variable uses in flambda2"
  | elt :: stack -> { t with stack = f elt :: stack; }

let add_binding var name_occurrences t =
  update_top_of_stack ~t ~f:(fun elt ->
    let bindings =
      Variable.Map.update var (function
        | None -> Some name_occurrences
        | Some _ ->
          Misc.fatal_errorf "The same variable has been bound twice"
      ) elt.bindings
    in
    { elt with bindings; }
  )

let add_used_in_current_handler name_occurrences t =
  update_top_of_stack ~t ~f:(fun elt ->
    let used_in_handler =
      Name_occurrences.union elt.used_in_handler name_occurrences
    in
    { elt with used_in_handler; }
  )

let add_apply_result_cont k t =
  update_top_of_stack ~t ~f:(fun elt ->
    let apply_result_conts = Continuation.Set.add k elt.apply_result_conts in
    { elt with apply_result_conts; }
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

  let print ppf t =
    Variable.Map.print Variable.Set.print ppf t

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
            if Variable.Set.mem dst visited then q else Queue.add dst q
          ) (edges t ~src:v) queue
        in
        reachable t visited queue
      end

end

(* Analysis *)
(* ******** *)

let used_variables ~return_continuation ~exn_continuation map extra =
  (* Some auxiliary functions *)
  let add_used name_occurrences set =
    Name_occurrences.fold_variables name_occurrences ~init:set
      ~f:(fun set var ->
        let num = Name_occurrences.count_variable_normal_mode name_occurrences var in
        Format.eprintf "%a: %a@." Variable.print var Num_occurrences.print num;
        Variable.Set.add var set)
  in
  (* Build the reversed graph of dependencies *)
  let graph, used =
    Continuation.Map.fold (fun _ {
      apply_cont_args; apply_result_conts; used_in_handler; bindings;
      continuation = _; params = _;
    } (graph, used) ->
      (* Add the vars used in the handler *)
      let used = add_used used_in_handler used in
      (* Add the vars of continuation used as function call return as used *)
      let used =
        Continuation.Set.fold (fun k used ->
          match Continuation.Map.find k map with
          | elt ->
            List.fold_left (fun used v -> Variable.Set.add v used) used elt.params
          | exception Not_found ->
            if Continuation.equal return_continuation k ||
               Continuation.equal exn_continuation k
            then used
            else
              Misc.fatal_errorf "Continuation not found during rec analysis: %a@."
                Continuation.print k
        ) apply_result_conts used
      in
      (* Build the graph of dependencies between bindings *)
      let graph =
        Variable.Map.fold (fun src name_occurrences graph ->
          Name_occurrences.fold_variables name_occurrences ~init:graph
            ~f:(fun g dst -> Var_graph.add_edge ~src ~dst g)
        ) bindings graph
      in
      (* Build the graph of dependencies between continuation
         parameters and arguments. *)
      Continuation.Map.fold (fun k args (graph, used) ->
        if Continuation.equal return_continuation k ||
           Continuation.equal exn_continuation k then begin
          let used =
            Numbers.Int.Map.fold (fun _ name_occurrences used ->
              add_used name_occurrences used
            ) args used
          in
          graph, used
        end else begin
          let params =
            match Continuation.Map.find k map with
            | elt -> Array.of_list elt.params
            | exception Not_found ->
              Misc.fatal_errorf "Continuation not found during rec analysis: %a@."
                Continuation.print k
          in
          let graph =
            Numbers.Int.Map.fold (fun i name_occurrence graph ->
              (* Note on the direction of the edge:
                 We later do a reachability analysis to compute the
                 transitive cloture of the used variables.
                 Therefore an edge from src to dst means: if src is used, then
                 dst is also used.
                 Aplied here, this means : if the param of a continuation is used,
                 then any argument provided for that param is also used.
                 The other way wouldn't make much sense. *)
              let src = params.(i) in
              Name_occurrences.fold_variables name_occurrence ~init:graph
                ~f:(fun g dst -> Var_graph.add_edge ~src ~dst g)
            ) args graph
          in
          graph, used
        end
      ) apply_cont_args (graph, used)
    ) map (Var_graph.empty, Variable.Set.empty)
  in
  let graph =
    Continuation.Map.fold (
      fun _ (extra_params_and_args : Continuation_extra_params_and_args.t) graph ->
      Apply_cont_rewrite_id.Map.fold (fun _ extra_args graph ->
        List.fold_left2 (fun graph extra_param extra_arg ->
          let src = Kinded_parameter.var extra_param in
          match (extra_arg : Continuation_extra_params_and_args.Extra_arg.t) with
          | Already_in_scope simple ->
            Name_occurrences.fold_variables (Simple.free_names simple) ~init:graph
              ~f:(fun g dst -> Var_graph.add_edge ~src ~dst g)
          | New_let_binding (src', prim) ->
            Name_occurrences.fold_variables (Flambda_primitive.free_names prim)
              ~f:(fun g dst -> Var_graph.add_edge ~src:src' ~dst g)
              ~init:(Var_graph.add_edge ~src ~dst:src' graph)
        ) graph extra_params_and_args.extra_params extra_args
      ) extra_params_and_args.extra_args graph
    ) extra graph
  in
  if debug then (Format.eprintf "@.@\nGRAPH:@\n%a@\n@." Var_graph.print graph);
  if debug then (Format.eprintf "@.@\nUSED:@\n%a@\n@." Variable.Set.print used);
  let transitively_used =
    Var_graph.reachable graph Variable.Set.empty
      (Variable.Set.fold Queue.add used Queue.empty)
  in
  transitively_used

let analyze ~return_continuation ~exn_continuation { stack; map; extra; } =
  assert (stack = []);
  let used = used_variables ~return_continuation ~exn_continuation map extra in
  if debug then (Format.eprintf "@.@\nUSED VARIABLES:@\n%a@\n@." Variable.Set.print used);
  used



