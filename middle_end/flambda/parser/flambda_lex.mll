
{
open Flambda_parser

type location = Lexing.position * Lexing.position

type error =
  | Illegal_character of char
  | Invalid_literal of string
;;

let pp_error ppf = function
  | Illegal_character c -> Format.fprintf ppf "Illegal character %c" c
  | Invalid_literal s -> Format.fprintf ppf "Invalid litral %s" s

exception Error of error * location;;

let current_location lexbuf =
  (Lexing.lexeme_start_p lexbuf,
   Lexing.lexeme_end_p lexbuf)

let create_hashtable init =
  let tbl = Hashtbl.create (List.length init) in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

let keyword_table =
  create_hashtable [
    "and", AND;
    "andwhere", ANDWHERE;
    "apply", APPLY;
    "block_load", BLOCK_LOAD;
    "ccall", CCALL;
    "closure", CLOSURE;
    "code", CODE;
    "cont", CONT;
    "deleted", DELETED;
    "direct", DIRECT;
    "done", DONE;
    "end", END;
    "error", ERROR;
    "exn", EXN;
    "fabricated", FABRICATED;
    "float", FLOAT_KIND;
    "get_tag", GET_TAG;
    "imm", IMM;
    "immutable_unique", IMMUTABLE_UNIQUE;
    "in", IN;
    "int32", INT32;
    "int64", INT64;
    "is_int", IS_INT;
    "let", LET;
    "mutable", MUTABLE;
    "nativeint", NATIVEINT;
    "newer_version_of", NEWER_VERSION_OF;
    "noalloc", NOALLOC;
    "phys_eq", PHYS_EQ;
    "phys_ne", PHYS_NE;
    "project_var", PROJECT_VAR;
    "rec", REC;
    "select_closure", SELECT_CLOSURE;
    "set_of_closures", SET_OF_CLOSURES;
    "size", SIZE;
    "stub", STUB;
    "switch", SWITCH;
    "symbol", SYMBOL;
    "tag_imm", TAG_IMM;
    "tupled", TUPLED;
    "unit", UNIT;
    "untag_imm", UNTAG_IMM;
    "val", VAL;
    "where", WHERE;
    "with", WITH;
]

let ukeyword_table =
  create_hashtable [
    "Opaque", OPAQUE;
    "Block", BLOCK;
    "HCF", HCF;
    "Unreachable", UNREACHABLE;
]

}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let dotsymbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '/' ':' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let int_modifier = ['G'-'Z' 'g'-'z']

rule token = parse
  | newline
      { Lexing.new_line lexbuf; token lexbuf }
  | blank +
      { token lexbuf }
  | "(*"
      { comment 1 lexbuf;
        token lexbuf }
  | "let"
      { LET }
  | ":"
      { COLON }
  | ","
      { COMMA }
  | "."
      { DOT }
  | ";"
      { SEMICOLON }
  | "="
      { EQUAL }
  | "{"
      { LBRACE }
  | "}"
      { RBRACE }
  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | "+"  { PLUS }
  | "+." { PLUSDOT }
  | "*"  { STAR }
  | "-"  { MINUS }
  | "-." { MINUSDOT }
  | "->" { MINUSGREATER }
  | "@" { AT }
  | "|"  { PIPE }
  | lowercase identchar *
      { let s = Lexing.lexeme lexbuf in
        try Hashtbl.find keyword_table s
        with Not_found -> LIDENT s }
  | uppercase identchar *
      { let s = Lexing.lexeme lexbuf in
        try Hashtbl.find ukeyword_table s
        with Not_found -> UIDENT s }
  | int_literal { INT (Lexing.lexeme lexbuf, None) }
  | (int_literal as lit) (int_modifier as modif)?
      { INT (lit, modif) }
  | float_literal | hex_float_literal
      { FLOAT (Lexing.lexeme lexbuf |> Float.of_string) }
  | (float_literal | hex_float_literal | int_literal) identchar+
      { raise (Error(Invalid_literal (Lexing.lexeme lexbuf),
                     current_location lexbuf)) }
  | eof { EOF }
  | _
      { raise (Error(Illegal_character (Lexing.lexeme_char lexbuf 0),
                     current_location lexbuf))
      }

and comment n = parse
  | newline
         { Lexing.new_line lexbuf; comment n lexbuf }
  | "*)"
         { if n = 1 then ()
           else comment (n-1) lexbuf }
  | "(*"
         { comment (n+1) lexbuf }
  | _
         { comment n lexbuf }
