
(* The type of tokens. *)

type token = 
  | WITH
  | WHERE
  | VAL
  | UNREACHABLE
  | UNIT
  | TUPLED
  | SYMBOL of (string)
  | SWITCH
  | STUB
  | STAR
  | SIZE
  | SET_OF_CLOSURES
  | SEMICOLON
  | RPAREN
  | REC
  | RBRACE
  | PRIM_UNTAG_IMM
  | PRIM_TAG_IMM
  | PRIM_SELECT_CLOSURE
  | PRIM_PROJECT_VAR
  | PRIM_PHYS_NE
  | PRIM_PHYS_EQ
  | PRIM_OPAQUE
  | PRIM_IS_INT
  | PRIM_GET_TAG
  | PRIM_BLOCK_LOAD
  | PRIM_BLOCK
  | PLUSDOT
  | PLUS
  | PIPE
  | NOALLOC
  | NEWER_VERSION_OF
  | NATIVEINT
  | MUTABLE
  | MINUSGREATER
  | MINUSDOT
  | MINUS
  | LPAREN
  | LET
  | LBRACE
  | INT64
  | INT32
  | INT of (string * char option)
  | IN
  | IMMUTABLE_UNIQUE
  | IMM
  | IDENT of (string)
  | HCF
  | FLOAT_KIND
  | FLOAT of (float)
  | FABRICATED
  | EXN
  | ERROR
  | EQUAL
  | EOF
  | END
  | DOT
  | DONE
  | DIRECT
  | DELETED
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
