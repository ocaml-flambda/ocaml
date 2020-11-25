(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Chambart and Guillaume Bury, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42-55"]

(* CR mshinwell: Disabling warning 55 is required to satisfy Closure, work
   out something better... *)

module Continuations = (Permutation.Make [@inlined hint]) (Continuation)
module Variables = (Permutation.Make [@inlined hint]) (Variable)
module Code_ids = (Permutation.Make [@inlined hint]) (Code_id)
module Symbols = (Permutation.Make [@inlined hint]) (Symbol)

type t = {
  continuations : Continuations.t;
  variables : Variables.t;
  code_ids : Code_ids.t;
  symbols : Symbols.t;
}

let empty =
  { continuations = Continuations.empty;
    variables = Variables.empty;
    code_ids = Code_ids.empty;
    symbols = Symbols.empty;
  }

let print ppf { continuations; variables; code_ids; symbols; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuations@ %a)@]@ \
      @[<hov 1>(variables@ %a)@])\
      @[<hov 1>(code_ids@ %a)@])\
      @[<hov 1>(symbols@ %a)@])\
      @]"
    Continuations.print continuations
    Variables.print variables
    Code_ids.print code_ids
    Symbols.print symbols

let is_empty { continuations; variables; code_ids; symbols; }  =
  Continuations.is_empty continuations
  && Variables.is_empty variables
  && Code_ids.is_empty code_ids
  && Symbols.is_empty symbols

let compose0
      ~second:
        { continuations = continuations2;
          variables = variables2;
          code_ids = code_ids2;
          symbols = symbols2;
        }
      ~first:
        { continuations = continuations1;
          variables = variables1;
          code_ids = code_ids1;
          symbols = symbols1;
        } =
  { continuations =
      Continuations.compose ~second:continuations2 ~first:continuations1;
    variables = Variables.compose ~second:variables2 ~first:variables1;
    code_ids = Code_ids.compose ~second:code_ids2 ~first:code_ids1;
    symbols = Symbols.compose ~second:symbols2 ~first:symbols1;
  }

let compose ~second ~first =
  if is_empty second then first
  else if is_empty first then second
  else compose0 ~second ~first


(* variables *)

let add_variable t var1 var2 =
  { t with
    variables = Variables.compose_one ~first:t.variables var1 var2;
  }

let add_fresh_variable t var1 ~guaranteed_fresh:var2 =
  { t with
    variables =
      Variables.compose_one_fresh t.variables var1 ~fresh:var2;
  }

let apply_variable t var =
  Variables.apply t.variables var

let apply_variable_set t vars =
  Variable.Set.fold (fun var result ->
      let var = apply_variable t var in
      Variable.Set.add var result)
    vars
    Variable.Set.empty


(* symbols *)

let add_symbol t symbol1 symbol2 =
  { t with
    symbols = Symbols.compose_one ~first:t.symbols symbol1 symbol2;
  }

let add_fresh_symbol t symbol1 ~guaranteed_fresh:symbol2 =
  { t with
    symbols = Symbols.compose_one_fresh t.symbols symbol1 ~fresh:symbol2;
  }

let apply_symbol t symbol =
  Symbols.apply t.symbols symbol

let apply_symbol_set t symbols =
  Symbol.Set.fold (fun symbol result ->
    let symbol = apply_symbol t symbol in
    Symbol.Set.add symbol result)
    symbols
    Symbol.Set.empty


(* application of permutations *)

let apply_name t name =
  Name.pattern_match name
    ~var:(fun var -> Name.var (apply_variable t var))
    ~symbol:(fun symbol -> Name.symbol (apply_symbol t symbol))


(* continuations *)

let add_continuation t k1 k2 =
  { t with
    continuations = Continuations.compose_one ~first:t.continuations k1 k2;
  }

let add_fresh_continuation t k1 ~guaranteed_fresh:k2 =
  { t with
    continuations =
      Continuations.compose_one_fresh t.continuations k1 ~fresh:k2;
  }

let apply_continuation t k =
  Continuations.apply t.continuations k


(* Code ids *)

let add_code_id t code_id1 code_id2 =
  { t with
    code_ids = Code_ids.compose_one ~first:t.code_ids code_id1 code_id2;
  }

let add_fresh_code_id t code_id1 ~guaranteed_fresh:code_id2 =
  { t with
    code_ids = Code_ids.compose_one_fresh t.code_ids code_id1 ~fresh:code_id2;
  }

let apply_code_id t code_id =
  Code_ids.apply t.code_ids code_id


