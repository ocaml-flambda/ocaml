%{
open Fexpr

let make_loc (startpos, endpos) = Lambda.of_raw_location {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

let make_located txt (startpos, endpos) =
  let loc = make_loc (startpos, endpos) in
  { txt; loc }

let make_tag ~loc:_ = function
  | s, None -> int_of_string s
  | _, Some _ ->
    failwith "No modifier allowed for tags"

let make_tagged_immediate ~loc:_ = function
  | s, Some 't' -> s
  | _, _ ->
    failwith "Tagged immediates must have modifier 't'"

let make_const_int (i, m) : Fexpr.const =
  match m with
  | None -> Naked_nativeint (Int64.of_string i)
  | Some 'u' -> Naked_immediate i
  | Some 't' -> Tagged_immediate i
  | Some 'l' -> Naked_int32 (Int32.of_string i)
  | Some 'L' -> Naked_int64 (Int64.of_string i)
  | Some c -> failwith (Printf.sprintf "Unknown int modifier %c" c)

let make_const_float (i, m) =
  match m with
  | None -> Naked_float (float_of_string i)
  | Some c -> failwith (Printf.sprintf "Unknown float modifier %c" c)

let partition_map (f : 'a -> [ `Left of 'b | `Right of 'c ]) (l : 'a list)
    : 'b list * 'c list =
  let next (acc_l, acc_r) x =
    match f x with
    | `Left b -> b :: acc_l, acc_r
    | `Right c -> acc_l, c :: acc_r
  in
  let rev_l, rev_r = List.fold_left next ([], []) l in
  List.rev rev_l, List.rev rev_r

let make_segment
      (bindings : [ `Code of code_binding
                  | `Closure of static_closure_binding ] list)
      (closure_elements : closure_element list option) =
  let code_bindings, closure_bindings =
    partition_map (function
    | `Code code_binding -> `Left code_binding
    | `Closure closure_binding -> `Right closure_binding
    ) bindings
  in
  { code_bindings; closure_bindings; closure_elements }
%}

/* Tokens */

%token AND   [@symbol "and"]
%token ANDWHERE [@symbol "andwhere"]
%token AT    [@symbol "@"]
%token APPLY [@symbol "apply"]
%token BLOCK [@symbol "Block"]
%token CCALL  [@symbol "ccall"]
%token CLOSURE  [@symbol "closure"]
%token CODE  [@symbol "code"]
%token COMMA [@symbol "comma"]
%token COLON  [@symbol ":"]
%token CONT  [@symbol "cont"]
%token DELETED [@symbol "deleted"]
%token DIRECT [@symbol "direct"]
%token DONE  [@symbol "done"]
%token DOT   [@symbol "."]
%token END   [@symbol "end"]
%token EQUAL [@symbol "="]
%token ERROR [@symbol "error"]
%token EXN   [@symbol "exn"]
%token FABRICATED [@symbol "fabricated"]
%token <string * char option> FLOAT
%token FLOAT_KIND [@symbol "float"]
%token HCF   [@symbol "HCF"]
%token IMM   [@symbol "imm" ]
%token IN    [@symbol "in"]
%token INT32 [@symbol "int32"]
%token INT64 [@symbol "int64"]
%token <string * char option> INT
%token LANGLE [@symbol "<"]
%token LBRACE [@symbol "{"]
%token LET    [@symbol "let"]
%token <string> LIDENT
%token LPAREN [@symbol "("]
%token MINUS    [@symbol "-"]
%token MINUSDOT [@symbol "-."]
%token MINUSGREATER [@symbol "->"]
%token NATIVEINT [@symbol "nativeint"]
%token NEWER_VERSION_OF [@symbol "newer_version_of"]
%token NOALLOC [@symbol "noalloc"]
%token OPAQUE [@symbol "opaque_identity"]
%token PHYS_EQ [@symbol "phys_eq"]
%token PHYS_NE [@symbol "phys_ne"]
%token PIPE [@symbol "|"]
%token PLUS     [@symbol "+"]
%token PLUSDOT  [@symbol "+."]
%token PROJECT_VAR [@symbol "project_var"]
%token RANGLE [@symbol ">"]
%token RBRACE [@symbol "}"]
%token REC    [@symbol "rec"]
%token RPAREN [@symbol ")"]
%token SELECT_CLOSURE [@symbol "select_closure"]
%token SEMICOLON [@symbol ";"]
%token SEGMENT [@symbol "segment"]
%token STUB   [@symbol "stub"]
%token STAR   [@symbol "*"]
%token SWITCH [@symbol "switch"]
%token SYMBOL [@symbol "symbol"]
%token <string> UIDENT
%token UNDERSCORE [@symbol "_"]
%token UNIT   [@symbol "unit"]
%token UNREACHABLE [@symbol "Unreachable"]
%token UNTAG_IMM [@symbol "untag_imm"]
%token VAL    [@symbol "val"]
%token WHERE  [@symbol "where"]
%token WITH   [@symbol "with"]
%token EOF

%start flambda_unit
%type <Fexpr.flambda_unit> flambda_unit
%type <Fexpr.static_structure> static_structure
%type <Fexpr.expr> expr
(* %type <Fexpr.name> name *)
%type <Fexpr.kind> kind
%type <Fexpr.named> named
%type <Fexpr.of_kind_value> of_kind_value
%%

(* XCR lwhite: Probably easier to just use some default names for these
   continuations
   
   lmaurer: Makes sense. I went with "done" and "error" for the names. *)
flambda_unit:
  | body = expr
    EOF
    { { body } }
;

exn_continuation:
  | STAR cont = continuation { cont }

exn_continuation_id:
  | STAR cont = continuation_id { cont }
;

let_symbol(body):
  | LET; bindings = symbol_bindings; IN; body = body; { { bindings; body } }
;

symbol_bindings:
  | SYMBOL s = static_structure { Simple s }
  | segs = segments { Segments segs }
;

segments:
  | seg = segment_body { [ seg ] }
  | segs = separated_nonempty_list(AND, segment) { segs }
;

segment:
  | SEGMENT; seg = segment_body; END { seg }
;

segment_body:
  | bindings = separated_nonempty_list(AND, code_or_closure_binding);
    closure_elements = with_closure_elements_opt;
    { make_segment bindings closure_elements }
;

code_or_closure_binding:
  | code_binding = code_binding { `Code code_binding }
  | closure_binding = static_closure_binding { `Closure closure_binding }
;

code_binding:
  | CODE;
    recursive = recursive;
    id = code_id;
    closure_id = option(AT; cid = closure_id { cid });
    newer_version_of = option(NEWER_VERSION_OF; id = code_id { id });
    code_or_deleted = code_or_deleted;
    { { id; closure_id; newer_version_of; code = code_or_deleted recursive } }
;

code_or_deleted:
  | DELETED { fun _ -> Deleted }
  | params = kinded_args;
    closure_var = variable_opt;
    vars_within_closures = kinded_vars_within_closures;
    MINUSGREATER; ret_cont = continuation_id;
    exn_cont = option(exn_continuation_id);
    ret_arity = return_arity;
    EQUAL; expr = expr;
    { fun recursive : code or_deleted ->
        Present { params; closure_var; vars_within_closures; ret_cont;
                ret_arity; exn_cont; recursive; expr; } }
;

static_closure_binding:
  | SYMBOL; symbol = symbol; EQUAL; code_id = closure; { { symbol; code_id } }
;

recursive:
  | { Nonrecursive }
  | REC { Recursive }
;

unop:
  | OPAQUE { Opaque_identity }
  | UNTAG_IMM { Untag_imm }
  | PROJECT_VAR; var = var_within_closure { Project_var var }
  | SELECT_CLOSURE; LPAREN; move_from = closure_id; MINUSGREATER;
      move_to = closure_id; RPAREN
    { Select_closure { move_from; move_to } }

infix_binop:
  | PLUS { Plus }
  | PLUSDOT { Plusdot }
  | MINUS { Minus }
  | MINUSDOT { Minusdot }
;

prefix_binop:
  | PHYS_EQ; k = kind_arg_opt { Phys_equal(k, Eq) }
  | PHYS_NE; k = kind_arg_opt { Phys_equal(k, Neq) }

binop:
  | a = simple DOT LPAREN f = simple RPAREN
    { Binop (Block_load (Block Value, Immutable), a, f) }
  | op = prefix_binop; LPAREN; arg1 = simple; COMMA; arg2 = simple; RPAREN
    { Binop (op, arg1, arg2) }
;

block:
  | BLOCK; t = tag; LPAREN;
    elts = separated_list(COMMA, simple);
    RPAREN
    { Block (t, Immutable, elts) }
;

named:
  | s = simple { Simple s }
  | u = unop a = simple { Prim (Unop (u, a)) }
  | a1 = simple b = infix_binop a2 = simple { Prim (Infix_binop (b, a1, a2)) }
  | b = binop { Prim b }
  | b = block { Prim b }
  | c = closure { Closure { code_id = c } }
;

switch_case:
  | i = tag MINUSGREATER c = continuation { i,c }
;

switch:
  | option(PIPE); cs = separated_list(PIPE, switch_case) { cs }
;
kind:
  | VAL { Value }
  | IMM { Naked_number Naked_immediate }
  | FLOAT_KIND { Naked_number Naked_float }
  | INT32 { Naked_number Naked_int32 }
  | INT64 { Naked_number Naked_int64 }
  | NATIVEINT { Naked_number Naked_nativeint }
  | FABRICATED { Fabricated }
;
kinds:
  | UNIT { [] }
  | ks = separated_nonempty_list(STAR, kind) { ks }
;
return_arity:
  | { None }
  | COLON k = kinds { Some k }
;

kind_arg_opt:
  | { None }
  | LBRACE; k = kind; RBRACE { Some k }
;

/* expr is staged so that let and where play nicely together. In particular, in
   let ... in ... where, we want the where to be on the inside so that the
   continuation can refer to the let-bound variables (and the defining
   expressions can't refer to continuations anyway); and in where ... where, we
   want both wheres to be at the same level (as it's easier to use parens to
   force them to nest than it is force them to un-nest). The straightforward way
   to achieve this is to have an expression be a let expression or an inner
   expression, and to have where be an inner expression. Then we say that the
   body of a continuation can have let but can't have where (without
   parentheses, that is). Unfortunately, it's hard to say "a let whose body is
   not a where" in a grammar, but we can get close by parameterizing let_expr by
   what nonterminal its body should be. */

expr:
  | l = let_expr(expr) { l }
  | i = inner_expr { i }
;

let_expr(body):
  | LET l = let_(body) { Let l }
  | ls = let_symbol(body) { Let_symbol ls }
;

inner_expr:
  | w = where_expr { w }
  | a = atomic_expr { a }
;

where_expr:
  | body = inner_expr; WHERE; recursive = recursive;
    handlers = separated_list(ANDWHERE, continuation_handler)
     { Let_cont { recursive; body; handlers } }
;

continuation_body:
  | l = let_expr(continuation_body) { l }
  | a = atomic_expr { a }
;

atomic_expr:
  | HCF { Invalid Halt_and_catch_fire }
  | UNREACHABLE { Invalid Treat_as_unreachable }
  | CONT c = continuation s = simple_args { Apply_cont (c, None, s) }
  | SWITCH; scrutinee = simple; cases = switch { Switch {scrutinee; cases} }
  | APPLY e = apply_expr { Apply e }
  | LPAREN; e = expr; RPAREN { e }
;

let_(body):
  | bindings = separated_nonempty_list(AND, let_binding);
(*  CR lwhite: I think this closure elements stuff is a bit of a hangover from
    when closures definitions contained the code as well. I imagine the closures
    used to look like:

    let f a b c =
      ...
    and g x y z =
      ...
    with { i = j; ... } in
    ...

    but now they should probably just look something like:

      let (f', g') = closure({f, g}, {i = j; ...}) in
      ...

    lmaurer: Let_symbol_expr.t still allows code and closure definitions to be
    mutually recursive, though, so we need some syntax that bundles them
    together. Also, several closures can share the same closure elements.
 *)
    closure_elements = with_closure_elements_opt;
    IN body = body;
    { { bindings; closure_elements; body } }
;

let_binding:
  | v = kinded_variable_opt EQUAL defining_expr = named
      { let (var, kind) = v in { var; kind; defining_expr } }
;

with_closure_elements_opt:
  | { None }
  | WITH LBRACE;
    elements = separated_list(SEMICOLON, closure_element);
    RBRACE;
    { Some elements }
;

closure_element:
  | var = var_within_closure; EQUAL; value = simple; { { var; value; } }
;

closure:
  | CLOSURE; code_id = code_id; { code_id }
;

apply_expr:
  | call_kind = call_kind func = func_name_with_optional_arities 
    args = simple_args MINUSGREATER
    r = continuation e = exn_continuation
     { let (func, arities) = func in {
          func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind;
          arities;
     } }
;

call_kind:
  | { Function Indirect }
  | DIRECT; LPAREN; AT; code_id = code_id; RPAREN
    { Function (Direct { code_id }) }
  | CCALL; noalloc = boption(NOALLOC)
    { C_call { alloc = not noalloc } }
;

exn_and_stub:
  | { false, false }
  | STUB { false, true }
  | EXN { true, false }
  | STUB EXN { true, true }
  | EXN STUB { true, true }
;

continuation_handler:
  | exn_and_stub = exn_and_stub; name = continuation_id;
    params = kinded_args; EQUAL; handler = continuation_body
    { let is_exn_handler, stub = exn_and_stub in
      { name; params; stub; is_exn_handler; handler } }
;

kinded_args:
  | LPAREN v = separated_nonempty_list(COMMA, kinded_variable) RPAREN { v }
  | { [] }

kinded_vars_within_closures:
    | LANGLE; v = separated_nonempty_list(COMMA, kinded_var_within_closure);
      RANGLE { v }
  | { [] }

static_structure:
  | s = symbol EQUAL sp = static_part
    { { symbol = s; kind = None; defining_expr = sp } }
;

static_part:
  | BLOCK; tag = tag; LPAREN;
    elements = separated_nonempty_list(COMMA, of_kind_value); RPAREN
    { (Block { tag; mutability = Immutable; elements } : static_part) }
;

tag:
  tag = INT { make_tag ~loc:(make_loc ($startpos, $endpos)) tag }
;

of_kind_value:
  | s = symbol { Symbol s }
  | v = variable { Dynamically_computed v }
  | i = INT { Tagged_immediate ( make_tagged_immediate ~loc:($startpos, $endpos) i ) }
;

kinded_variable:
  | param = variable { { param; kind = None } }
  | param = variable; COLON; kind = kind { { param; kind = Some kind } }
;

kinded_variable_opt:
  | v = variable_opt { v, None }
  | v = variable_opt; COLON; kind = kind; { v, Some kind }
;

kinded_var_within_closure:
  | var = var_within_closure { { var; kind = None } }
  | LPAREN; var = var_within_closure; COLON; kind = kind; RPAREN
    { { var; kind = Some kind } }
;

simple_args:
  | { [] }
  | LPAREN s = separated_nonempty_list(COMMA, simple) RPAREN { s }
;

const:
  | c = INT { make_const_int c }
  | c = FLOAT { make_const_float c }
;

name:
  | s = symbol { (Symbol s:name) }
  | v = variable { (Var v:name) }
;

func_name_with_optional_arities:
  | n = name { n, None }
  | LPAREN; n = name; COLON; params_arity = kinds; MINUSGREATER;
    ret_arity = kinds; RPAREN
    { n, Some ({ params_arity; ret_arity } : function_arities) }

simple:
  | s = symbol { Symbol s }
  | v = variable { Var v }
  | c = const { Const c }
;

code_id:
  | v = variable { v }
;
 
closure_id:
  | v = variable { v }
;

symbol:
  | e = UIDENT { make_located e ($startpos, $endpos) }
;

variable:
  | e = LIDENT { make_located e ($startpos, $endpos) }
;

variable_opt:
  | UNDERSCORE { None }
  | e = LIDENT { Some (make_located e ($startpos, $endpos)) }
;

continuation_id :
  | e = LIDENT { make_located e ($startpos, $endpos) }
;

continuation:
  | e = continuation_id { Named e }
  | s = special_continuation { Special s }
;

special_continuation:
  | DONE { Done : special_continuation }
  | ERROR { Error : special_continuation }
;

var_within_closure:
  | e = LIDENT { make_located e ($startpos, $endpos) }
;
%%
