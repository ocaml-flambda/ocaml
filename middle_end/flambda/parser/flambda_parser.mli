
(* The type of tokens. *)

type token = 
  | WITH
  | UNREACHABLE
  | UNDERSCORE
  | UIDENT of (string)
  | TAG
  | SYMBOL
  | SWITCH
  | STUB
  | STAR
  | SEMICOLON
  | SEGMENT
  | RPAREN
  | REC
  | RBRACKET
  | RBRACE
  | RANGLE
  | PROJECT_VAR
  | PLUSDOT
  | PLUS
  | OPAQUE
  | NEWER_VERSION_OF
  | MINUSGREATER
  | MINUSDOT
  | MINUS
  | LPAREN
  | LIDENT of (string)
  | LETK
  | LET
  | LBRACKET
  | LBRACE
  | LANGLE
  | IS_INT
  | INT of (string * char option)
  | IN
  | HCF
  | FLOAT of (string * char option)
  | EXN
  | EQUAL
  | EOF
  | DOT
  | CONT
  | COMMA
  | COLON
  | CODE
  | CLOSURE
  | CCALL
  | BLOCK
  | AT
  | APPLY
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val flambda_unit: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Fexpr.flambda_unit)
