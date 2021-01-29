%{
open Fexpr

let make_loc (startpos, endpos) = Debuginfo.Scoped_location.of_location ~scopes:[] {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

let make_located txt (startpos, endpos) =
  let loc = make_loc (startpos, endpos) in
  { txt; loc }

let make_plain_int = function
  | s, None -> Int64.of_string s |> Int64.to_int
  | _, Some _ ->
    failwith "No modifier expected here"

let make_targetint = function
  | s, None -> Int64.of_string s
  | _, Some _ ->
    failwith "No modifier expected here"

let make_tag ~loc:_ = function
  | s, None -> int_of_string s
  | _, Some _ ->
    failwith "No modifier allowed for tags"

let make_tagged_immediate ~loc:_ = function
  | s, None -> s
  | _, _ ->
    failwith "Must be a tagged immediate"

let make_const_int (i, m) : const =
  match m with
  | None -> Tagged_immediate i
  | Some 'u' -> Naked_immediate i
  | Some 'n' -> Naked_nativeint (Int64.of_string i)
  | Some 'l' -> Naked_int32 (Int32.of_string i)
  | Some 'L' -> Naked_int64 (Int64.of_string i)
  | Some c -> failwith (Printf.sprintf "Unknown int modifier %c" c)

%}

/* Tokens */

%token AT    [@symbol "@"]
%token BIGARROW [@symbol "===>"]
%token COLON  [@symbol ":"]
%token COMMA [@symbol ","]
%token DOT   [@symbol "."]
%token EQUAL [@symbol "="]
%token EQUALDOT [@symbol "=."]
%token <float> FLOAT
%token GREATER [@symbol ">"]
%token GREATEREQUAL [@symbol ">="]
%token GREATERDOT [@symbol ">."]
%token GREATEREQUALDOT [@symbol ">"]
%token <string> IDENT
%token <string * char option> INT
%token LBRACE [@symbol "{"]
%token LESS   [@symbol "<"]
%token LESSDOT [@symbol "<."]
%token LESSEQUAL [@symbol "<="]
%token LESSEQUALDOT [@symbol "<=."]
%token LESSMINUS [@symbol "<-"]
%token LPAREN [@symbol "("]
%token MINUS    [@symbol "-"]
%token MINUSDOT [@symbol "-."]
%token MINUSGREATER [@symbol "->"]
%token NOTEQUALDOT [@symbol "!=."]
%token PIPE [@symbol "|"]
%token PLUS     [@symbol "+"]
%token PLUSDOT  [@symbol "+."]
%token RBRACE [@symbol "}"]
%token RPAREN [@symbol ")"]
%token SEMICOLON [@symbol ";"]
%token STAR   [@symbol "*"]
%token<Fexpr.compilation_unit option * string> SYMBOL
%token EOF

%token KWD_ALWAYS [@symbol "always"]
%token KWD_AND   [@symbol "and"]
%token KWD_ANDWHERE [@symbol "andwhere"]
%token KWD_APPLY [@symbol "apply"]
%token KWD_BLOCK [@symbol "Block"]
%token KWD_CCALL  [@symbol "ccall"]
%token KWD_CLOSURE  [@symbol "closure"]
%token KWD_CODE  [@symbol "code"]
%token KWD_CONT  [@symbol "cont"]
%token KWD_DEFAULT [@symbol "default"]
%token KWD_DELETED [@symbol "deleted"]
%token KWD_DEPTH [@symbol "depth"]
%token KWD_DIRECT [@symbol "direct"]
%token KWD_DONE  [@symbol "done"]
%token KWD_DYNAMIC [@symbol "dynamic"]
%token KWD_END   [@symbol "end"]
%token KWD_ERROR [@symbol "error"]
%token KWD_EXN   [@symbol "exn"]
%token KWD_FABRICATED [@symbol "fabricated"]
%token KWD_FLOAT [@symbol "float"]
%token KWD_HCF   [@symbol "halt_and_catch_fire"]
%token KWD_HINT  [@symbol "hint"]
%token KWD_IMM   [@symbol "imm" ]
%token KWD_IMMUTABLE_UNIQUE [@symbol "immutable_unique"]
%token KWD_IN    [@symbol "in"]
%token KWD_INLINE [@symbol "inline"]
%token KWD_INLINING_STATE [@symbol "inlining_state"]
%token KWD_INT32 [@symbol "int32"]
%token KWD_INT64 [@symbol "int64"]
%token KWD_LET    [@symbol "let"]
%token KWD_MUTABLE [@symbol "mutable"]
%token KWD_NATIVEINT [@symbol "nativeint"]
%token KWD_NEVER  [@symbol "never"]
%token KWD_NEWER_VERSION_OF [@symbol "newer_version_of"]
%token KWD_NOALLOC [@symbol "noalloc"]
%token KWD_REC    [@symbol "rec"]
%token KWD_SET_OF_CLOSURES [@symbol "set_of_closures"]
%token KWD_SIZE   [@symbol "size"]
%token KWD_STUB   [@symbol "stub"]
%token KWD_SWITCH [@symbol "switch"]
%token KWD_TUPLED [@symbol "tupled"]
%token KWD_UNIT   [@symbol "unit"]
%token KWD_UNREACHABLE [@symbol "unreachable"]
%token KWD_UNROLL [@symbol "unroll"]
%token KWD_UNSIGNED [@symbol "unsigned"]
%token KWD_VAL    [@symbol "val"]
%token KWD_WHERE  [@symbol "where"]
%token KWD_WITH   [@symbol "with"]

%token PRIM_ARRAY_LENGTH [@symbol "%array_length"]
%token PRIM_ARRAY_LOAD [@symbol "%array_load"]
%token PRIM_ARRAY_SET [@symbol "%array_set"]
%token PRIM_BLOCK [@symbol "%Block"]
%token PRIM_BLOCK_LOAD [@symbol "%block_load"]
%token PRIM_GET_TAG [@symbol "%get_tag"]
%token PRIM_INT_COMP [@symbol "%int_comp"]
%token PRIM_IS_INT [@symbol "%is_int"]
%token PRIM_OPAQUE [@symbol "%Opaque"]
%token PRIM_PHYS_EQ [@symbol "%phys_eq"]
%token PRIM_PHYS_NE [@symbol "%phys_ne"]
%token PRIM_PROJECT_VAR [@symbol "%project_var"]
%token PRIM_SELECT_CLOSURE [@symbol "%select_closure"]
%token PRIM_TAG_IMM [@symbol "%Tag_imm"]
%token PRIM_UNTAG_IMM [@symbol "%untag_imm"]

%start flambda_unit expect_test_spec
%type <Fexpr.array_kind> array_kind
%type <Fexpr.block_access_field_kind> block_access_field_kind
%type <Fexpr.const> const
%type <Fexpr.expect_test_spec> expect_test_spec
%type <Fexpr.flambda_unit> flambda_unit
%type <Fexpr.kind> kind
%type <Fexpr.mutability> mutability
%type <Fexpr.name> name
%type <Fexpr.named> named
%type <Fexpr.of_kind_value> of_kind_value
%type <Fexpr.ordered_comparison> int_comp
%type <Fexpr.special_continuation> special_continuation
%type <Fexpr.standard_int> standard_int
%type <Fexpr.static_structure> static_structure
%type <Fexpr.symbol_binding> symbol_binding
%%

(* CR-someday lmaurer: Modularize and generally clean up *)

flambda_unit:
  | body = module_
    EOF
    { body }
;

expect_test_spec:
  | before = module_; BIGARROW; after = module_; EOF
    { { before; after } }
;

(* XCR lwhite: Probably easier to just use some default names for these
   continuations
   
   lmaurer: Makes sense. I went with "done" and "error" for the names. *)
module_:
  | body = expr
    { { body } }
;

exn_continuation:
  | STAR cont = continuation { cont }

exn_continuation_id:
  | STAR cont = continuation_id { cont }
;

let_symbol(body):
  | KWD_LET; bindings = separated_nonempty_list(KWD_AND, symbol_binding);
    closure_elements = with_closure_elements_opt;
    KWD_IN; body = body; { { bindings; closure_elements; body } }
;

symbol_binding:
  | s = static_structure { Block_like s }
  | code = code { Code code }
  | s = static_closure_binding { Closure s }
  | s = static_set_of_closures { Set_of_closures s }
;

code:
  | header = code_header;
    params = kinded_args; 
    closure_var = variable;
    MINUSGREATER; ret_cont = continuation_id;
    exn_cont = exn_continuation_id;
    ret_arity = return_arity;
    EQUAL; body = expr;
    { let recursive, inline, id, newer_version_of = header in
      { id; newer_version_of; param_arity = None; ret_arity; recursive; inline;
        params_and_body = Present { params; closure_var; ret_cont; exn_cont;
                                    body } } }
  | header = code_header;
    KWD_DELETED;
    COLON;
    param_arity = kinds;
    MINUSGREATER;
    ret_arity = kinds;
    { let recursive, inline, id, newer_version_of = header in
      { id; newer_version_of; param_arity = Some param_arity;
        ret_arity = Some ret_arity; recursive; inline;
        params_and_body = Deleted } }
;

code_header:
  | KWD_CODE;
    recursive = recursive;
    inline = option(inline);
    id = code_id;
    newer_version_of = option(newer_version_of);
    { recursive, inline, id, newer_version_of }
;

newer_version_of:
  | KWD_NEWER_VERSION_OF LPAREN; id = code_id; RPAREN { id };

static_closure_binding:
  | symbol = symbol; EQUAL; fun_decl = fun_decl;
    { { symbol; fun_decl } }
;

static_set_of_closures:
  | KWD_SET_OF_CLOSURES;
    bindings = separated_nonempty_list(KWD_AND, static_closure_binding);
    elements = with_closure_elements_opt;
    KWD_END
    { { bindings; elements } }

recursive:
  | { Nonrecursive }
  | KWD_REC { Recursive }
;

unop:
  | PRIM_ARRAY_LENGTH; ak = array_kind { Array_length ak }
  | PRIM_GET_TAG { Get_tag }
  | PRIM_IS_INT { Is_int }
  | PRIM_OPAQUE { Opaque_identity }
  | PRIM_TAG_IMM { Tag_imm }
  | PRIM_UNTAG_IMM { Untag_imm }
  | PRIM_PROJECT_VAR; project_from = closure_id; DOT; var = var_within_closure
    { Project_var { project_from; var } }
  | PRIM_SELECT_CLOSURE; LPAREN; move_from = closure_id; MINUSGREATER;
      move_to = closure_id; RPAREN
    { Select_closure { move_from; move_to } }

infix_binop:
  | PLUS { Plus }
  | MINUS { Minus }
  | LESS { Lt }
  | LESSEQUAL { Le }
  | GREATER { Gt }
  | GREATEREQUAL { Ge }
  | PLUSDOT { Plusdot }
  | MINUSDOT { Minusdot }
  | EQUALDOT { Eqdot }
  | NOTEQUALDOT { Neqdot }
  | LESSDOT { Lt }
  | LESSEQUALDOT { Le }
  | GREATERDOT { Gt }
  | GREATEREQUALDOT { Ge }
;

prefix_binop:
  | PRIM_BLOCK_LOAD;
    field_kind = block_access_field_kind;
    mutability = mutability;
    tag = tag;
    size = option(KWD_SIZE; size = targetint { size })
    { Block_load (Values { tag; size; field_kind }, mutability) }
  | PRIM_PHYS_EQ; k = kind_arg_opt { Phys_equal(k, Eq) }
  | PRIM_PHYS_NE; k = kind_arg_opt { Phys_equal(k, Neq) }

mutability:
  | KWD_MUTABLE { Mutable }
  | KWD_IMMUTABLE_UNIQUE { Immutable_unique }
  | { Immutable }

array_kind:
  | { Values }
  | KWD_IMM { Immediates }
  | KWD_FLOAT { Naked_floats }
  | KWD_DYNAMIC { Float_array_opt_dynamic }

block_access_field_kind:
  | { Any_value }
  | KWD_IMM { Immediate }

standard_int:
  | { Tagged_immediate }
  | KWD_IMM { Naked_immediate }
  | KWD_INT32 { Naked_int32 }
  | KWD_INT64 { Naked_int64 }
  | KWD_NATIVEINT { Naked_nativeint }

init_or_assign:
  | EQUAL { Initialization }
  | LESSMINUS { Assignment }

signed_or_unsigned:
  | { Signed }
  | KWD_UNSIGNED { Unsigned }

int_comp:
  | LESS { Lt }
  | GREATER { Gt }
  | LESSEQUAL { Le }
  | GREATEREQUAL { Ge }

binop_app:
  | op = prefix_binop; LPAREN; arg1 = simple; COMMA; arg2 = simple; RPAREN
    { Binary (op, arg1, arg2) }
  | arg1 = simple; op = infix_binop; arg2 = simple
    { Binary (Infix op, arg1, arg2) }
  | PRIM_ARRAY_LOAD; ak = array_kind; mut = mutability;
    arg1 = simple; DOT; LPAREN; arg2 = simple; RPAREN
    { Binary (Array_load (ak, mut), arg1, arg2) }
  | PRIM_INT_COMP;
    i = standard_int; s = signed_or_unsigned;
    arg1 = simple; c = int_comp; arg2 = simple
    { Binary (Int_comp (i, s, c), arg1, arg2) }
;

ternop_app:
  | PRIM_ARRAY_SET; ak = array_kind;
    arr = simple; DOT LPAREN; ix = simple; RPAREN; ia = init_or_assign;
    v = simple
    { Ternary (Array_set (ak, ia), arr, ix, v) }
;

block:
  | PRIM_BLOCK; m = mutability; t = tag; LPAREN;
    elts = separated_list(COMMA, simple);
    RPAREN
    { Variadic (Make_block (t, m), elts) }
;

named:
  | s = simple { Simple s }
  | u = unop a = simple { Prim (Unary (u, a)) }
  | b = binop_app { Prim b }
  | t = ternop_app { Prim t }
  | b = block { Prim b }
  | c = fun_decl { Closure c }
;

switch_case:
  | i = tag; MINUSGREATER; ac = apply_cont_expr { i,ac }
;

switch:
  | option(PIPE); cs = separated_list(PIPE, switch_case) { cs }
;
kind:
  | KWD_VAL { Value }
  | KWD_IMM { Naked_number Naked_immediate }
  | KWD_FLOAT { Naked_number Naked_float }
  | KWD_INT32 { Naked_number Naked_int32 }
  | KWD_INT64 { Naked_number Naked_int64 }
  | KWD_NATIVEINT { Naked_number Naked_nativeint }
  | KWD_FABRICATED { Fabricated }
;
kinds:
  | KWD_UNIT { [] }
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
  | KWD_LET l = let_(body) { Let l }
  | ls = let_symbol(body) { Let_symbol ls }
;

inner_expr:
  | w = where_expr { w }
  | a = atomic_expr { a }
;

where_expr:
  | body = inner_expr; KWD_WHERE; recursive = recursive;
    handlers = separated_list(KWD_ANDWHERE, continuation_handler)
     { Let_cont { recursive; body; handlers } }
;

continuation_body:
  | l = let_expr(continuation_body) { l }
  | a = atomic_expr { a }
;

atomic_expr:
  | KWD_HCF { Invalid Halt_and_catch_fire }
  | KWD_UNREACHABLE { Invalid Treat_as_unreachable }
  | KWD_CONT; ac = apply_cont_expr { Apply_cont ac }
  | KWD_SWITCH; scrutinee = simple; cases = switch { Switch {scrutinee; cases} }
  | KWD_APPLY e = apply_expr { Apply e }
  | LPAREN; e = expr; RPAREN { e }
;

let_(body):
  | bindings = separated_nonempty_list(KWD_AND, let_binding);
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
    KWD_IN body = body;
    { ({ bindings; closure_elements; body } : let_) }
;

let_binding:
  | v = kinded_variable EQUAL defining_expr = named
      { let { param = var; kind } = v in { var; kind; defining_expr } }
;

with_closure_elements_opt:
  | { None }
  | KWD_WITH LBRACE;
    elements = separated_list(SEMICOLON, closure_element);
    RBRACE;
    { Some elements }
;

closure_element:
  | var = var_within_closure; EQUAL; value = simple; { { var; value; } }
;

fun_decl:
  | KWD_CLOSURE; is_tupled = boption(KWD_TUPLED); code_id = code_id;
    closure_id = closure_id_opt;
    { { code_id; closure_id; is_tupled } }
;

apply_expr:
  | call_kind = call_kind;
    inline = option(inline);
    inlining_state = option(inlining_state);
    func = func_name_with_optional_arities 
    args = simple_args MINUSGREATER
    r = result_continuation e = exn_continuation
     { let (func, arities) = func in {
          func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind;
          inline;
          inlining_state;
          arities;
     } }
;

call_kind:
  | { Function Indirect }
  | KWD_DIRECT; LPAREN; code_id = code_id; closure_id = closure_id_opt; RPAREN
    { Function (Direct { code_id; closure_id }) }
  | KWD_CCALL; noalloc = boption(KWD_NOALLOC)
    { C_call { alloc = not noalloc } }
;

inline:
  | KWD_INLINE LPAREN KWD_ALWAYS RPAREN { Always_inline }
  | KWD_INLINE LPAREN KWD_HINT RPAREN { Hint_inline }
  | KWD_INLINE LPAREN KWD_NEVER RPAREN { Never_inline }
  | KWD_UNROLL LPAREN; i = plain_int; RPAREN { Unroll i }
  | KWD_INLINE LPAREN KWD_DEFAULT RPAREN { Default_inline }

inlining_state:
  | KWD_INLINING_STATE LPAREN; i = inlining_state_depth; RPAREN { Inlining_state.create ~depth:i }

inlining_state_depth:
  | KWD_DEPTH; i = plain_int; { i }

result_continuation:
  | c = continuation { Return c }
  | KWD_NEVER { Never_returns }
;

apply_cont_expr:
  | cont = continuation; args = simple_args
    { { cont; args; trap_action = None } }
;

exn_and_stub:
  | { false, false }
  | KWD_STUB { false, true }
  | KWD_EXN { true, false }
  | KWD_STUB KWD_EXN { true, true }
  | KWD_EXN KWD_STUB { true, true }
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

static_structure:
  | s = symbol EQUAL sp = static_part
    { { symbol = s; kind = None; defining_expr = sp } }
;

static_part:
  | KWD_BLOCK; m = mutability; tag = tag; LPAREN;
    elements = separated_list(COMMA, of_kind_value); RPAREN
    { (Block { tag; mutability = m; elements } : static_part) }
;

targetint:
  i = INT { make_targetint i }

tag:
  tag = INT { make_tag ~loc:(make_loc ($startpos, $endpos)) tag }
;

plain_int:
  i = INT { make_plain_int i }
;

of_kind_value:
  | s = symbol { Symbol s }
  | v = variable { Dynamically_computed v }
  | i = INT { Tagged_immediate ( make_tagged_immediate ~loc:($startpos, $endpos) i ) }
;

kinded_variable:
  | param = variable; kind = kind_opt { { param; kind } }
;

kind_opt:
  | { None }
  | COLON; kind = kind { Some kind }
;

simple_args:
  | { [] }
  | LPAREN s = separated_nonempty_list(COMMA, simple) RPAREN { s }
;

const:
  | c = INT { make_const_int c }
  | c = FLOAT { Naked_float c }
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

closure_id_opt :
  | { None }
  | AT; cid = closure_id { Some cid }
;

symbol:
  | e = SYMBOL { make_located e ($startpos, $endpos) }
;

variable:
  | e = IDENT { make_located e ($startpos, $endpos) }
;

continuation_id :
  | e = IDENT { make_located e ($startpos, $endpos) }
;

continuation:
  | e = continuation_id { Named e }
  | s = special_continuation { Special s }
;

special_continuation:
  | KWD_DONE { Done }
  | KWD_ERROR { Error }
;

var_within_closure:
  | e = IDENT { make_located e ($startpos, $endpos) }
;
%%
