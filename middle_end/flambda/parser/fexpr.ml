[@@@ocaml.warning "-30"]

type location = Lambda.scoped_location
type 'a located = 'a Location.loc = {
  txt : 'a;
  loc : location;
}

type variable = string located
type continuation = string located
type code_id = string located
type closure_id = string located
type var_within_closure = string located

type symbol = string located

type immediate = string
type targetint = int64

type const =
  | Naked_immediate of immediate
  | Tagged_immediate of immediate
  | Naked_float of float
  | Naked_int32 of Int32.t
  | Naked_int64 of Int64.t
  | Naked_nativeint of targetint

type of_kind_value =
  | Symbol of symbol
  | Tagged_immediate of immediate
  | Dynamically_computed of variable

type is_recursive =
  | Nonrecursive
  | Recursive

type tag_scannable = int

type static_part =
  | Block of {
      tag : tag_scannable;
      mutability : Mutability.t;
      elements : of_kind_value list;
    }

module Naked_number_kind = struct
  type t =
    | Naked_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint
end

type kind =
  | Value
  | Naked_number of Naked_number_kind.t
  | Fabricated

type static_structure = {
  symbol : symbol;
  kind : kind option;
  defining_expr : static_part;
}

type invalid_term_semantics =
  | Treat_as_unreachable
  | Halt_and_catch_fire

type trap_action
type kinded_parameter = {
  param : variable;
  kind : kind option;
}

type kinded_var_within_closure = {
  var : var_within_closure;
  kind : kind option;
}

type name =
  | Var of variable
  | Symbol of symbol

type simple =
  | Var of variable
  | Symbol of symbol
  | Const of const

type unop =
  | Opaque_identity
  | Project_var of var_within_closure

type generic_array_specialisation =
  | No_specialisation
  | Full_of_naked_floats
  | Full_of_immediates
  | Full_of_arbitrary_values_but_not_floats

type block_access_kind =
  | Block of kind
  | Array of kind
  | Generic_array of generic_array_specialisation

type binop =
  | Block_load of block_access_kind * Mutability.t

type infix_binop =
  | Plus | Plusdot
  | Minus | Minusdot

type prim =
  | Unop of unop * simple
  | Infix_binop of infix_binop * simple * simple
  | Binop of binop * simple * simple
  | Block of tag_scannable * Mutability.t * simple list

type is_fabricated =
  | Value | Fabricated

type flambda_arity = kind list

type function_call =
  | Direct of {
      closure_id : closure_id;
      return_arity : flambda_arity;
    }
  | Indirect_unknown_arity
  | Indirect_known_arity of {
      param_arity : flambda_arity;
      return_arity : flambda_arity;
    }

type method_kind = Self | Public | Cached

type call_kind =
  | Function of function_call
  | Method of { kind : method_kind; obj : name; }
  | C_call of {
      alloc : bool;
      (* param_arity : flambda_arity; To recover from args *)
      return_arity : flambda_arity option;
    }

type apply = {
    func : name;
    continuation : continuation;
    exn_continuation : continuation;
    args : simple list;
    call_kind : call_kind;
    (* dbg : Debuginfo.t; *)
    (* inline : inline_attribute;
     * specialise : specialise_attribute; *)
  }

type size = int

type expr =
  | Let of let_
  | Let_cont of let_cont
  | Let_symbol of let_symbol
  | Apply of apply
  | Apply_cont of continuation * trap_action option * simple list
  | Switch of {
      scrutinee : simple;
      cases : (int * continuation) list;
    }
  | Invalid of invalid_term_semantics

and closure_elements = closure_element list

and closure_element = {
  var : var_within_closure;
  value : simple;
}

and let_ = {
  bindings : let_binding list;
  closure_elements : closure_elements option;
  body : expr;
}

and let_binding = {
    var : variable option;
    kind : kind option;
    defining_expr : named;
  }

and named =
  | Simple of simple
  | Prim of prim
  | Closure of { code_id : code_id }

and let_cont = {
  recursive : is_recursive;
  body : expr;
  handlers : let_cont_handlers;
}

and let_cont_handlers = continuation_handler list

and continuation_handler = {
  name : continuation;
  params : kinded_parameter list;
  stub : bool;
  is_exn_handler : bool;
  handler : expr;
}

and let_symbol = {
  bindings : symbol_bindings;
  body : expr;
}

and symbol_bindings =
  | Simple of static_structure
  | Segments of segment list

and segment = {
  code_bindings : code_binding list;
  closure_bindings : static_closure_binding list;
  closure_elements : closure_elements option;
}

and code_binding = {
  (* TODO: Allow deleted? *)
  id : code_id;
  closure_id : closure_id option;
  newer_version_of : code_id option;
  params : kinded_parameter list;
  closure_var : variable;
  vars_within_closures : kinded_var_within_closure list;
  ret_cont : continuation;
  exn_cont : continuation option;
  ret_arity : flambda_arity option;
  recursive : is_recursive;
  expr : expr;
}

and static_closure_binding = {
  symbol : symbol;
  code_id : code_id;
}

type flambda_unit = {
  return_cont : continuation;
  exception_cont : continuation option;
  body : expr;
}
