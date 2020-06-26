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

module Binding = struct
  type t = {
    closure_id : Closure_id.t;
    func_decl : Function_declaration.t;
  }

  let of_pair (closure_id, func_decl) = { closure_id; func_decl }

  let to_pair { closure_id; func_decl } = closure_id, func_decl
end

type t = {
  funs : Function_declaration.t Closure_id.Map.t;
  as_list : Binding.t list
}

let invariant _env _t = ()

let empty =
  { funs = Closure_id.Map.empty;
    as_list = []
  }

let is_empty { funs; _ } =
  Closure_id.Map.is_empty funs

let create as_list =
  { funs = Closure_id.Map.of_list (List.map Binding.to_pair as_list);
    as_list
  }

let funs t = t.funs

let funs_in_order t = t.as_list

let find ({ funs; _ } : t) closure_id =
  Closure_id.Map.find closure_id funs

let print_with_cache ~cache ppf { as_list; _ } =
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Misc.print_assoc
      Closure_id.print (Function_declaration.print_with_cache ~cache))
    (List.map Binding.to_pair as_list)

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let free_names { funs; _ } =
  Closure_id.Map.fold
    (fun _closure_id (func_decl : Function_declaration.t) syms ->
      Name_occurrences.union syms (Function_declaration.free_names func_decl))
    funs
    (Name_occurrences.empty)

let apply_name_permutation ({ as_list; _ } as t) perm =
  let as_list' =
    Misc.Stdlib.List.map_sharing (fun (binding : Binding.t) ->
        let func_decl =
          Function_declaration.apply_name_permutation binding.func_decl perm
        in
        if func_decl == binding.func_decl then binding
        else { binding with Binding.func_decl })
      as_list
  in
  if as_list == as_list' then t
  else create as_list

let all_ids_for_export { funs; _ } =
  Closure_id.Map.fold
    (fun _closure_id (func_decl : Function_declaration.t) ids ->
      Ids_for_export.union ids
        (Function_declaration.all_ids_for_export func_decl))
    funs
    Ids_for_export.empty

let import import_map { as_list; _ } =
  let as_list = List.map (fun (binding : Binding.t) ->
    let func_decl = Function_declaration.import import_map binding.func_decl in
    { binding with func_decl }
  ) as_list
  in
  create as_list

let compare { funs = funs1; _ } { funs = funs2; _ } =
  Closure_id.Map.compare Function_declaration.compare funs1 funs2

let filter t ~f =
  let funs = Closure_id.Map.filter f t.funs in
  let as_list =
    List.filter (fun (binding : Binding.t) ->
      f binding.closure_id binding.func_decl) t.as_list
  in
  { funs; as_list; }
