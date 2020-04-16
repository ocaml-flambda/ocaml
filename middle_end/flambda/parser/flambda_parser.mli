
(* The type of tokens. *)

type token = 
  | WITH
  | VAL
  | UNREACHABLE
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
  | RBRACKET
  | RBRACE
  | RANGLE
  | PROJECT_VAR
  | PLUSDOT
  | PLUS
  | OPAQUE
  | NEWER_VERSION_OF
  | NATIVEINT
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
  | EQUAL
  | EOF
  | END
  | DOT
  | CONT
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

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include CamlinternalMenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val flambda_unit: Lexing.position -> (Fexpr.flambda_unit) MenhirInterpreter.checkpoint
  
end
