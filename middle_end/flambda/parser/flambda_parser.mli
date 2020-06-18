
(* The type of tokens. *)

type token = 
  | WITH
  | WHERE
  | VAL
  | UNREACHABLE
  | UNIT
  | UNDERSCORE
  | UIDENT of (string)
  | SYMBOL
  | SWITCH
  | STUB
  | STAR
  | SEMICOLON
  | SEGMENT
  | RPAREN
  | REC
  | RBRACE
  | RANGLE
  | PROJECT_VAR
  | PLUSDOT
  | PLUS
  | PIPE
  | OPAQUE
  | NOALLOC
  | NEWER_VERSION_OF
  | NATIVEINT
  | MINUSGREATER
  | MINUSDOT
  | MINUS
  | LPAREN
  | LIDENT of (string)
  | LET
  | LBRACE
  | LANGLE
  | INT64
  | INT32
  | INT of (string * char option)
  | IN
  | IMM
  | HCF
  | FLOAT_KIND
  | FLOAT of (string * char option)
  | FABRICATED
  | EXN
  | ERROR
  | EQUAL
  | EOF
  | END
  | DOT
  | DONE
  | DIRECT
  | CONT
  | COMMA
  | COLON
  | CODE
  | CLOSURE
  | CCALL
  | BLOCK
  | AT
  | APPLY
  | ANDWHERE
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val flambda_unit: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Fexpr.flambda_unit)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include CamlinternalMenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val flambda_unit: Lexing.position -> (Fexpr.flambda_unit) MenhirInterpreter.checkpoint
  
end
