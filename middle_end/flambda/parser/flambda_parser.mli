
(* The type of tokens. *)

type token = 
  | SYMBOL of (Fexpr.compilation_unit option * string)
  | STAR
  | SEMICOLON
  | RPAREN
  | RBRACE
  | PRIM_UNTAG_IMM
  | PRIM_TAG_IMM
  | PRIM_SELECT_CLOSURE
  | PRIM_PROJECT_VAR
  | PRIM_PHYS_NE
  | PRIM_PHYS_EQ
  | PRIM_OPAQUE
  | PRIM_IS_INT
  | PRIM_INT_COMP
  | PRIM_GET_TAG
  | PRIM_BLOCK_LOAD
  | PRIM_BLOCK
  | PRIM_ARRAY_SET
  | PRIM_ARRAY_LOAD
  | PRIM_ARRAY_LENGTH
  | PLUSDOT
  | PLUS
  | PIPE
  | NOTEQUALDOT
  | MINUSGREATER
  | MINUSDOT
  | MINUS
  | LPAREN
  | LESSMINUS
  | LESSEQUALDOT
  | LESSEQUAL
  | LESSDOT
  | LESS
  | LBRACE
  | KWD_WITH
  | KWD_WHERE
  | KWD_VAL
  | KWD_UNSIGNED
  | KWD_UNROLL
  | KWD_UNREACHABLE
  | KWD_UNIT
  | KWD_TUPLED
  | KWD_SWITCH
  | KWD_STUB
  | KWD_SIZE
  | KWD_SET_OF_CLOSURES
  | KWD_REC
  | KWD_NOALLOC
  | KWD_NEWER_VERSION_OF
  | KWD_NEVER
  | KWD_NATIVEINT
  | KWD_MUTABLE
  | KWD_LET
  | KWD_INT64
  | KWD_INT32
  | KWD_INLINING_STATE
  | KWD_INLINE
  | KWD_IN
  | KWD_IMMUTABLE_UNIQUE
  | KWD_IMM
  | KWD_HINT
  | KWD_HCF
  | KWD_FLOAT
  | KWD_FABRICATED
  | KWD_EXN
  | KWD_ERROR
  | KWD_END
  | KWD_DYNAMIC
  | KWD_DONE
  | KWD_DIRECT
  | KWD_DEPTH
  | KWD_DELETED
  | KWD_DEFAULT
  | KWD_CONT
  | KWD_CODE
  | KWD_CLOSURE
  | KWD_CCALL
  | KWD_BLOCK
  | KWD_APPLY
  | KWD_ANDWHERE
  | KWD_AND
  | KWD_ALWAYS
  | INT of (string * char option)
  | IDENT of (string)
  | GREATEREQUALDOT
  | GREATEREQUAL
  | GREATERDOT
  | GREATER
  | FLOAT of (float)
  | EQUALDOT
  | EQUAL
  | EOF
  | DOT
  | COMMA
  | COLON
  | BIGARROW
  | AT

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val flambda_unit: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Fexpr.flambda_unit)

val expect_test_spec: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Fexpr.expect_test_spec)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include CamlinternalMenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val flambda_unit: Lexing.position -> (Fexpr.flambda_unit) MenhirInterpreter.checkpoint
  
  val expect_test_spec: Lexing.position -> (Fexpr.expect_test_spec) MenhirInterpreter.checkpoint
  
end
