
(* This generated code requires the following version of CamlinternalMenhirLib: *)

let () =
  CamlinternalMenhirLib.StaticVersion.require_20200211

module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WITH
    | WHERE
    | VAL
    | UNTAG_IMM
    | UNREACHABLE
    | UNIT
    | UNDERSCORE
    | UIDENT of (
# 98 "flambda_parser.mly"
       (string)
# 23 "flambda_parser-in.ml"
  )
    | TUPLED
    | SYMBOL
    | SWITCH
    | STUB
    | STAR
    | SET_OF_CLOSURES
    | SEMICOLON
    | SELECT_CLOSURE
    | RPAREN
    | REC
    | RBRACE
    | PROJECT_VAR
    | PLUSDOT
    | PLUS
    | PIPE
    | PHYS_NE
    | PHYS_EQ
    | OPAQUE
    | NOALLOC
    | NEWER_VERSION_OF
    | NATIVEINT
    | MINUSGREATER
    | MINUSDOT
    | MINUS
    | LPAREN
    | LIDENT of (
# 72 "flambda_parser.mly"
       (string)
# 53 "flambda_parser-in.ml"
  )
    | LET
    | LBRACE
    | INT64
    | INT32
    | INT of (
# 69 "flambda_parser.mly"
       (string * char option)
# 62 "flambda_parser-in.ml"
  )
    | IN
    | IMM
    | HCF
    | FLOAT_KIND
    | FLOAT of (
# 62 "flambda_parser.mly"
       (string * char option)
# 71 "flambda_parser-in.ml"
  )
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
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

# 1 "flambda_parser.mly"
  
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


# 141 "flambda_parser-in.ml"

module Tables = struct
  
  include MenhirBasics
  
  let token2terminal : token -> int =
    fun _tok ->
      match _tok with
      | AND ->
          65
      | ANDWHERE ->
          64
      | APPLY ->
          63
      | AT ->
          62
      | BLOCK ->
          61
      | CCALL ->
          60
      | CLOSURE ->
          59
      | CODE ->
          58
      | COLON ->
          57
      | COMMA ->
          56
      | CONT ->
          55
      | DELETED ->
          54
      | DIRECT ->
          53
      | DONE ->
          52
      | DOT ->
          51
      | END ->
          50
      | EOF ->
          49
      | EQUAL ->
          48
      | ERROR ->
          47
      | EXN ->
          46
      | FABRICATED ->
          45
      | FLOAT _ ->
          44
      | FLOAT_KIND ->
          43
      | HCF ->
          42
      | IMM ->
          41
      | IN ->
          40
      | INT _ ->
          39
      | INT32 ->
          38
      | INT64 ->
          37
      | LBRACE ->
          36
      | LET ->
          35
      | LIDENT _ ->
          34
      | LPAREN ->
          33
      | MINUS ->
          32
      | MINUSDOT ->
          31
      | MINUSGREATER ->
          30
      | NATIVEINT ->
          29
      | NEWER_VERSION_OF ->
          28
      | NOALLOC ->
          27
      | OPAQUE ->
          26
      | PHYS_EQ ->
          25
      | PHYS_NE ->
          24
      | PIPE ->
          23
      | PLUS ->
          22
      | PLUSDOT ->
          21
      | PROJECT_VAR ->
          20
      | RBRACE ->
          19
      | REC ->
          18
      | RPAREN ->
          17
      | SELECT_CLOSURE ->
          16
      | SEMICOLON ->
          15
      | SET_OF_CLOSURES ->
          14
      | STAR ->
          13
      | STUB ->
          12
      | SWITCH ->
          11
      | SYMBOL ->
          10
      | TUPLED ->
          9
      | UIDENT _ ->
          8
      | UNDERSCORE ->
          7
      | UNIT ->
          6
      | UNREACHABLE ->
          5
      | UNTAG_IMM ->
          4
      | VAL ->
          3
      | WHERE ->
          2
      | WITH ->
          1
  
  and error_terminal =
    0
  
  and token2value : token -> Obj.t =
    fun _tok ->
      match _tok with
      | AND ->
          Obj.repr ()
      | ANDWHERE ->
          Obj.repr ()
      | APPLY ->
          Obj.repr ()
      | AT ->
          Obj.repr ()
      | BLOCK ->
          Obj.repr ()
      | CCALL ->
          Obj.repr ()
      | CLOSURE ->
          Obj.repr ()
      | CODE ->
          Obj.repr ()
      | COLON ->
          Obj.repr ()
      | COMMA ->
          Obj.repr ()
      | CONT ->
          Obj.repr ()
      | DELETED ->
          Obj.repr ()
      | DIRECT ->
          Obj.repr ()
      | DONE ->
          Obj.repr ()
      | DOT ->
          Obj.repr ()
      | END ->
          Obj.repr ()
      | EOF ->
          Obj.repr ()
      | EQUAL ->
          Obj.repr ()
      | ERROR ->
          Obj.repr ()
      | EXN ->
          Obj.repr ()
      | FABRICATED ->
          Obj.repr ()
      | FLOAT _v ->
          Obj.repr _v
      | FLOAT_KIND ->
          Obj.repr ()
      | HCF ->
          Obj.repr ()
      | IMM ->
          Obj.repr ()
      | IN ->
          Obj.repr ()
      | INT _v ->
          Obj.repr _v
      | INT32 ->
          Obj.repr ()
      | INT64 ->
          Obj.repr ()
      | LBRACE ->
          Obj.repr ()
      | LET ->
          Obj.repr ()
      | LIDENT _v ->
          Obj.repr _v
      | LPAREN ->
          Obj.repr ()
      | MINUS ->
          Obj.repr ()
      | MINUSDOT ->
          Obj.repr ()
      | MINUSGREATER ->
          Obj.repr ()
      | NATIVEINT ->
          Obj.repr ()
      | NEWER_VERSION_OF ->
          Obj.repr ()
      | NOALLOC ->
          Obj.repr ()
      | OPAQUE ->
          Obj.repr ()
      | PHYS_EQ ->
          Obj.repr ()
      | PHYS_NE ->
          Obj.repr ()
      | PIPE ->
          Obj.repr ()
      | PLUS ->
          Obj.repr ()
      | PLUSDOT ->
          Obj.repr ()
      | PROJECT_VAR ->
          Obj.repr ()
      | RBRACE ->
          Obj.repr ()
      | REC ->
          Obj.repr ()
      | RPAREN ->
          Obj.repr ()
      | SELECT_CLOSURE ->
          Obj.repr ()
      | SEMICOLON ->
          Obj.repr ()
      | SET_OF_CLOSURES ->
          Obj.repr ()
      | STAR ->
          Obj.repr ()
      | STUB ->
          Obj.repr ()
      | SWITCH ->
          Obj.repr ()
      | SYMBOL ->
          Obj.repr ()
      | TUPLED ->
          Obj.repr ()
      | UIDENT _v ->
          Obj.repr _v
      | UNDERSCORE ->
          Obj.repr ()
      | UNIT ->
          Obj.repr ()
      | UNREACHABLE ->
          Obj.repr ()
      | UNTAG_IMM ->
          Obj.repr ()
      | VAL ->
          Obj.repr ()
      | WHERE ->
          Obj.repr ()
      | WITH ->
          Obj.repr ()
  
  and default_reduction =
    (8, "\000\005\000\146\157\029\030\134\133\000f\007\000\151\000\000$\139\138 \031\000\000\000\000~\135\000\137\002\145\000\000\128W\144\000\000\158\000\000\000\000\017\000\028\000\000\022\024/\000\000\000dcb\000\141\000\000|\143\140\000\000\000\000\000\000t\000\000\000\156\000\000\021Y\000\162\000\000\130\000\142\159\000n\000\000\000h\027\000\0008=<;9:>F\000\000\147\150\149v\000\000\000\000D\000A\000\000z\000\000GH\000\000\026\000\000\132\000\000\000\000\000+\000\000p\000\000\004\000\006\000\000\000\000\000\019\000\015\020\000\000\\[\000\000\000\000\00010\000\000\000\000\000*\003\b6O,\000\000\000\000(\000)S\160\000\000\000\000\000\000\000\000M!P\"\000\000\000I\000\000r\000\000\153\000\000\000\000\000\155\000\000\000\154\000\000\000@l\000k\152\000\000\000U\000\r\000^\0003254\000\000\000\n\000\012\000\000\000\000\000\011Ka`_L\148#\000\000x\0257j\000\000\000Q\000\000\000JN\000\t\000.\001Z")
  
  and error =
    (66, "\004\016\000\000P \001\001\000\000\000\000\000\000\000\000\000\b\000\000\002\016\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\016@\000`\016\0000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000`\000@\000\001\128@\000\192\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\000\000\000\128\004 \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\016@\016 \016\0000\b\000\000\002\016\128\000\000\000\001\000\000\000\000\002\000\000\128\000\000!\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\128\001\004\000\002\001\000\003\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\016\000\000P \001\001\000H\128\000\b\000\000\b\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\128\000\000\000\000\000\000\000\000\005\000\004\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\b\002\000$\000\000\000\000\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000@\000\000\000\000 \000\000\b@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\002\000\000\128\000\000!\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\000\000 \000\000\000\000\000\000\000\000\000\000\000\000\b\000\000\000\000\000\000\000\000\000@@\000\000\000\000\000 \000@\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\128\000\000\000\000\000\b\000\000\000\000\000\004\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\128\000\000!\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000D\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\128\000\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\000\000\000\000\016\000\000\134\000\000 \000\000\000\000\000\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\004\000@\000\000\016\025P\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000@\000\000\000\000\128\000\000@\b\128\000\000\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\006\000\000 \000\000\000\000\000\128\000\000\000\000\000@\000\000\000\000\192\004\000\000\001\001\149\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000 \000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000@\004\128\000\001\001\149\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\004\128\000\001\001\149\000\000\000\000\000\000\000\000\000\000\001\000\017\000\b\002\002\000\001\016\000\000\004\006T\000\000\000\000\000\000\000\000\000\000\000\016\000\000\002\000\000\000\000\000\000\000\b\000\000\000\000\000\000\000\000 \000\000\000\000\001\000\000\000\000 \016\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\128@\004\128\000\001\001\149\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\004\016\000\000P \001\001\000\000\000\000\000\000\000\000\000\000\000\000\002\000\016\128\000\000\000\000\000\000\000\000\000\000\128\000\000`\000\004\b\000\000\000\000\016\000\000\000\000\000\000\000\002\000\000\000\000\000\001\000\000\000\000\000\b\000\000@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\000\001\006\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\024\000\000\000\000\b\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\001 \000\000@e@\000\000\000\000\000\b\000\000\000\000\018\000\000\004\006T\000\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002@\000\000\000\000\000\000\000\128\000\000\000\000\000\000\000\002\000\016\128\000\000\016\000\000\000\000\000\000\000\000\000\000 \001\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\128\001\000\000\002\001\000\001`\b`\000 \130@\000X\002\016\000\b \144\000\016\000\000\000\002\000 \000\000\000\000\000\000\000\000\000\000\000\b\000\000 \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\000\000\016\000 \000\000\000\000\000\000\000\b\000\000\016@\000\001@\128\004\004\001\"\000\000 \000\000 \016\000\000\000\000 \000\000\000\000\000\000\000\b\000\000\000\016@\000\001@\128\004\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000@\000\000\000\000\128\000\000\000\000\000\000\000 \000\000\000A\000\000\005\002\000\016\016\000\000\000\000\000\000\000\000@\000\000\000\000\128\000\000@@\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\b\128\136\224!\b\000\020\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\000\128\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\b\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000H\000\000\000\004\000\000\001\001\149\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\016\000\000\000\000\b\004\000\002\016\128\000\000\000\000\000\000\000\000\000\000\000\000@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\000\000\002\016\128\000\000\000\000\000\000\000\000\000\000@\000\006\001\128\128\016\000@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000\b\000\000\002\016\128\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\bB\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\128\000\000!\b\000\000\000\000\000\000\000\000\000 \000\b\000\000\002\016\128\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000`\000@\000\000\128@\000\192\002\000\000\b\000\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\b\000\000\000\000\000\000\000\002\000\000\000\004\016\000\000P \001\001\000\000\000\000\000\000\000\000\004\000\000\000\000\b\000\000\000\000\000\000\000\002\000\000\000\004\016\000\000P \001\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")
  
  and start =
    1
  
  and action =
    ((16, "\000\236\000\000\000\t\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\000\000\000\000z\000\000\000!\000;\000\000\000\000\000\000\000\000\000\000\0006\000\t\000\226\000\t\000\000\000\000\000\140\000\000\000\000\000\000\000\134\000b\000\000\000\000\000\000\000\236\001\012\000\000\000\t\000t\000u\000$\000\000\000\t\000\000\001\018\000\t\000\000\000\000\000\000\000b\000\178\001\b\000\000\000\000\000\000\001\n\000\000\001\022\001\b\000\000\000\000\000\000\001\028\000\t\000\218\000u\000\002\001\028\000\000\000\184\001\\\000\140\000\000\001H\000\t\000\000\000\000\001\132\000\000\001J\000\140\000\000\001J\000\000\000\000\000\007\000\000\000\t\001@\000\t\000\000\000\000\000J\000\152\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\n\000.\000\000\000\000\000\000\000\000\001\028\000\t\000T\000\152\000\000\001\144\000\000\001\"\000\t\000\000\001B\000\152\000\000\000\000\001~\000\152\000\000\000\214\000\152\000\000\0006\001\130\000;\000\140\000;\000\000\001*\000\152\000\000\001`\000\236\000\000\000;\000\000\0018\001\128\000\t\001:\001\162\000\000\000\018\000\000\000\000\001T\000\t\000\000\000\000\001T\000\152\001\140\000\152\001\168\000\000\000\000\0010\001\144\000;\001\180\000;\000\000\000\000\000\000\000\000\000\000\000\000\000J\000\014\000~\0010\000\000\001x\000\000\000\000\000\000\000;\001X\001p\001\000\001\012\001\156\001\130\001\000\000\000\000\000\000\000\000\000\001\156\001\132\001\000\000\000\000\012\0006\000\000\001v\000\t\000\000\001\150\000\t\001\158\000\t\001\186\000\000\000\t\001x\000\140\000\000\001^\000\152\001\186\000\000\000\000\001^\000\000\000\000\000b\001\160\000\t\000\000\001\194\000\000\000\t\000\000\000\228\000\000\000\000\000\000\000\000\001\164\000\t\001\198\000\000\000\t\000\000\001\168\000\t\001~\000\t\001\206\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\188\000~\000\000\000\000\000\000\000\000\001\156\001\162\000\236\000\000\001\156\001\164\000\236\000\000\000\000\001\212\000\000\001\150\000\000\000\000\000\000"), (16, "\0036\000\170\001\201\000\206\000\014\000B\001\209\001\193\001\177\001\177\001\145\001\145\003:\001M\000>\001b\003R\0005\000F\001\177\003b\003v\003~\000J\001\177\001b\001\145\000\174\002\029\002\029\000\018\001\177\000*\000\158\000\154\000\022\002^\001\002\000\177\002\194\000\026\001\177\0005\0005\002\029\001\209\001\193\001\177\001\145\001\145\002\029\001\201\000=\001\177\000\177\000\170\001\177\003\130\001\145\001\t\000Z\001Z\001U\001U\001E\001E\001\022\002\029\001\249\001\249\001\166\003&\001\177\001\145\001\145\002\202\002\029\000\177\001U\001\130\001E\001^\001\234\002\026\001\249\001\017\000\177\000r\0006\001]\000\130\002\029\002\029\001\"\001~\001\217\001\217\000\145\001\t\001\198\0006\001U\000\177\001E\001*\001\134\000\166\001\249\002\t\002\210\001U\001\217\001E\001\138\001\142\001q\001\249\001\146\001\161\001\150\002\002\001\154\000\214\000\006\002\t\001U\001U\001\161\001E\000\n\001\241\001\249\001\249\000\006\001\217\003\166\003\170\002\t\000Y\000\n\000\014\000\154\002\129\001\217\000\158\003\174\003\178\002\t\001\002\001\005\000\234\000\146\001\006\000\150\001q\002\t\001\233\001\014\004\006\001\217\0026\000\146\001\225\002\242\000E\003\182\000\018\001\153\001Z\000b\0026\000\218\002\t\002:\000Y\001\190\001\005\000Y\000\014\001q\001J\002B\002\029\002:\002\001\000Z\000\149\000Y\001n\000E\000E\002B\001^\001\153\001\153\000\242\001\226\001\185\002\206\000\190\002\214\001\218\000Y\002j\000\018\001\190\002&\001\"\000\249\002F\001&\003f\0012\001B\001\153\001V\002Z\001\210\001\230\000\190\001\005\001\246\000\153\002\018\0022\002J\002V\002z\002\130\002\138\002\154\002\162\002\238\002\254\003\026\0032\003>\003F\003N\003Z\003n\003\138\003\150\003\186\003\194\003\210\002\129\003\218\003\226\004\"\0042\004B\004K"))
  
  and lhs =
    (8, "\000SRQQQQQQPPPONNMMLLLKJIIHHGFEEDDCCBA@@@@@?>==<;::9999887777777665544332210/..--,+**))((''&%%$$$$$###\"\"!!  \031\031\030\030\029\029\028\028\027\027\026\026\025\025\024\024\023\023\022\022\021\021\020\020\019\019\018\018\018\017\017\016\016\015\014\r\012\011\n\t\b\b\b\b\007\006\006\006\006\005\004\003\003\002\001\001")
  
  and goto =
    ((16, "\000(\000\000\000.\000\000\000\000\000\000\000\000\000\000\000\000\001\134\000\000\000\000\000\206\000\000\000\000\000\132\000\000\000\000\000\000\000\000\000\000\000\027\000\"\000\000\000\180\000\000\000\000\000\000\000\000\000\000\000\000\000\000\0012\000\000\000\000\000\000\000\012\000\007\000\000\000\b\000\000\0004\000\127\000\000\000\168\000\000\0007\000h\000\000\000\000\000\000\000P\000\000\000\232\000\000\000\000\000\000\000\000\000\000\000\000\000\236\000\000\000\000\000\000\000\174\000n\000\000\0004\000\000\001p\000\000\000\212\000\000\000\028\000\000\000\000\001\002\000\000\000\000\000\000\000\000\000\000\000\007\000\000\000\000\000\000\000\000\000\176\000\000\001$\000\180\001\144\000\000\000\000\000\000\001T\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000t\000\000\000\000\000\000\000\000\001d\000\186\000\000\001f\000\000\000\000\000\000\000\000\001@\000\000\000\000\0010\000\000\000\000\000\000\0012\000\000\000\000\000\025\000\000\001\210\000\000\001X\001Z\001Z\000\000\001\166\001>\000\000\000\000\000D\000\000\000\180\000\000\001\026\000\000\001\156\001P\000\000\000\000\001J\000\000\000\000\001\006\001p\000\000\000\000\000\000\001F\000\000\001d\000\000\000\000\000\000\001\198\000\000\0014\001p\001\\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\180\000\011\000\000\000\000\000\000\000\000\000\000\000\000\001p\001\138\000\000\000W\000\006\001\244\000\000\000\196\000\000\000\000\000\000\000\000\001\248\000\000\000\204\000\000\000\000\001*\000\000\000\000\0000\000\000\000\000\001\166\000\000\001\176\000\000\000\000\001\184\000\000\001\242\000\000\001\146\001\146\000\000\000\000\000\000\001\150\000\000\000\000\001\246\000\000\000\174\000\000\000\000\000\000\001:\000\000\001\148\000\000\000\000\000\000\000\000\000\000\001<\000\000\000\000\001`\000\000\000\000\001b\000\000\001n\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001L\000\000\000\000\000\000\000\000\002\006\000\000\000n\000\000\002\b\000\000\000\152\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"), (16, "\000_\000\193\000L\000\194\000\030\000i\000_\000\130\000\173\000k\000l\000i\000m\000)\000-\000k\000l\000T\000m\000L\000\183\000\b\000\173\001\007\001\000\001\011\000\t\000\b\000\b\000\190\000\231\000\197\000\t\000\t\000O\000\024\000\173\000\184\000\196\000\028\000?\000\n\000\233\000\128\000\201\001\015\0003\0005\000\204\000\174\000\201\000\175\000\254\000P\000\204\000\244\0001\000\173\001\018\000\185\000\250\001\001\000\176\000\174\000C\000\175\000i\001\016\000o\000\255\000k\000l\000R\000m\000o\000\255\000\176\000\174\000\173\000\175\001\020\001\021\000\020\000\251\000n\000@\000\027\001\005\000.\000R\000\176\000\b\000\027\000\027\000\b\001\004\000\t\000q\000\174\000\t\000\175\001\005\000E\000\252\000\253\000\024\000\020\000U\000\024\000\228\000\015\000\176\000\026\000 \000H\001\005\001\n\000t\000Z\000\174\0008\000\175\0008\000^\000#\0009\0002\0009\000\229\000o\000\255\000\021\000\176\000\b\000\022\000\156\001\005\001\014\000\t\000:\000\157\000>\000\193\000$\000\194\000v\000\193\000N\000\194\000\031\000.\000<\000_\000<\000/\000\021\000\027\001\005\000\022\000\027\000\015\000\b\000\b\000 \000q\000\195\000\t\000\t\000\164\000\200\000\020\000|\000|\000\144\000\"\000\232\000\240\000\203\000|\000\196\000\b\000\b\000|\000\196\000x\000\t\000\t\000\b\000\156\000\165\000\020\001\003\000\t\000\157\000\243\000\246\000\201\000|\000\027\000E\000\204\000\248\000}\000\127\000.\000\137\000\012\000\128\000\128\000\139\000.\000G\000v\000\160\000\128\0001\000[\000\154\000\128\000\021\0001\000\158\000\168\000\172\0001\000h\000\027\000\027\000\162\000\r\000\185\000\131\001\001\000\128\000s\001\006\000\132\000\134\000\136\000\021\000\140\000\149\000\170\000\153\000\166\000\027\000\027\000\171\000\178\000\186\000\187\000\191\000\027\000\198\000\216\000\221\000\219\000\223\000\226\000\242\001\b\001\012\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000]\000\000\000\000\000\000\000\000\000\000\000\148\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\209\000\000\000\000\000\000\000\000\000\211\000\000\000\000\000\000\000\214"))
  
  and semantic_action =
    [|
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = args;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_args_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_args_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = cont;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_cont_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_cont_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let args : 'tv_simple_args = Obj.magic args in
        let cont : 'tv_continuation = Obj.magic cont in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_cont_ in
        let _endpos = _endpos_args_ in
        let _v : 'tv_apply_cont_expr = 
# 395 "flambda_parser.mly"
    ( { cont; args; trap_action = None } )
# 461 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = e;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_e_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_e_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = r;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_r_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_r_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _4;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = args;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_args_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_args_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _;
                  CamlinternalMenhirLib.EngineTypes.semv = func;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos_func_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos_func_;
                  CamlinternalMenhirLib.EngineTypes.next = {
                    CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                    CamlinternalMenhirLib.EngineTypes.semv = call_kind;
                    CamlinternalMenhirLib.EngineTypes.startp = _startpos_call_kind_;
                    CamlinternalMenhirLib.EngineTypes.endp = _endpos_call_kind_;
                    CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let e : 'tv_exn_continuation = Obj.magic e in
        let r : 'tv_continuation = Obj.magic r in
        let _4 : unit = Obj.magic _4 in
        let args : 'tv_simple_args = Obj.magic args in
        let func : 'tv_func_name_with_optional_arities = Obj.magic func in
        let call_kind : 'tv_call_kind = Obj.magic call_kind in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_call_kind_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_apply_expr = 
# 375 "flambda_parser.mly"
     ( let (func, arities) = func in {
          func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind;
          arities;
     } )
# 528 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_atomic_expr = 
# 313 "flambda_parser.mly"
        ( Invalid Halt_and_catch_fire )
# 553 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_atomic_expr = 
# 314 "flambda_parser.mly"
                ( Invalid Treat_as_unreachable )
# 578 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = ac;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_ac_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_ac_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let ac : 'tv_apply_cont_expr = Obj.magic ac in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_ac_ in
        let _v : 'tv_atomic_expr = 
# 315 "flambda_parser.mly"
                               ( Apply_cont ac )
# 610 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = cases;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_cases_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_cases_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = scrutinee;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_scrutinee_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_scrutinee_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = _1;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let cases : 'tv_switch = Obj.magic cases in
        let scrutinee : 'tv_simple = Obj.magic scrutinee in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_cases_ in
        let _v : 'tv_atomic_expr = 
# 316 "flambda_parser.mly"
                                               ( Switch {scrutinee; cases} )
# 649 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = e;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_e_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_e_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let e : 'tv_apply_expr = Obj.magic e in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_atomic_expr = 
# 317 "flambda_parser.mly"
                         ( Apply e )
# 681 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _3;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = e;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_e_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_e_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = _1;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let _3 : unit = Obj.magic _3 in
        let e : 'tv_expr = Obj.magic e in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_atomic_expr = 
# 318 "flambda_parser.mly"
                             ( e )
# 720 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _5;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__5_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__5_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = f;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_f_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_f_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _3;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = _2;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                  CamlinternalMenhirLib.EngineTypes.semv = a;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos_a_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos_a_;
                  CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                };
              };
            };
          };
        } = _menhir_stack in
        let _5 : unit = Obj.magic _5 in
        let f : 'tv_simple = Obj.magic f in
        let _3 : unit = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let a : 'tv_simple = Obj.magic a in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_a_ in
        let _endpos = _endpos__5_ in
        let _v : 'tv_binop_app = 
# 221 "flambda_parser.mly"
    ( Binary (Block_load (Block Value, Immutable), a, f) )
# 773 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _6;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__6_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__6_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = arg2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_arg2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_arg2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _4;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = arg1;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_arg1_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_arg1_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _;
                  CamlinternalMenhirLib.EngineTypes.semv = _2;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
                  CamlinternalMenhirLib.EngineTypes.next = {
                    CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                    CamlinternalMenhirLib.EngineTypes.semv = op;
                    CamlinternalMenhirLib.EngineTypes.startp = _startpos_op_;
                    CamlinternalMenhirLib.EngineTypes.endp = _endpos_op_;
                    CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let _6 : unit = Obj.magic _6 in
        let arg2 : 'tv_simple = Obj.magic arg2 in
        let _4 : unit = Obj.magic _4 in
        let arg1 : 'tv_simple = Obj.magic arg1 in
        let _2 : unit = Obj.magic _2 in
        let op : 'tv_prefix_binop = Obj.magic op in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_op_ in
        let _endpos = _endpos__6_ in
        let _v : 'tv_binop_app = 
# 223 "flambda_parser.mly"
    ( Binary (op, arg1, arg2) )
# 833 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = arg2;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_arg2_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_arg2_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = op;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_op_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_op_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = arg1;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_arg1_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_arg1_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let arg2 : 'tv_simple = Obj.magic arg2 in
        let op : 'tv_infix_binop = Obj.magic op in
        let arg1 : 'tv_simple = Obj.magic arg1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_arg1_ in
        let _endpos = _endpos_arg2_ in
        let _v : 'tv_binop_app = 
# 225 "flambda_parser.mly"
    ( Binary (Infix op, arg1, arg2) )
# 872 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _5;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__5_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__5_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = xs;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _3;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = t;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_t_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_t_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                  CamlinternalMenhirLib.EngineTypes.semv = _1;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                  CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                };
              };
            };
          };
        } = _menhir_stack in
        let _5 : unit = Obj.magic _5 in
        let xs : 'tv_loption_separated_nonempty_list_COMMA_simple__ = Obj.magic xs in
        let _3 : unit = Obj.magic _3 in
        let t : 'tv_tag = Obj.magic t in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : 'tv_block = let elts = 
# 232 "<standard.mly>"
    ( xs )
# 925 "flambda_parser-in.ml"
         in
        
# 232 "flambda_parser.mly"
    ( Variadic (Make_block (t, Immutable), elts) )
# 930 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_boption_NOALLOC_ = 
# 133 "<standard.mly>"
    ( false )
# 948 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_boption_NOALLOC_ = 
# 135 "<standard.mly>"
    ( true )
# 973 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_boption_TUPLED_ = 
# 133 "<standard.mly>"
    ( false )
# 991 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_boption_TUPLED_ = 
# 135 "<standard.mly>"
    ( true )
# 1016 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_call_kind = 
# 386 "flambda_parser.mly"
    ( Function Indirect )
# 1034 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _5;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__5_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__5_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = closure_id;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_closure_id_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_closure_id_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = code_id;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_code_id_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_code_id_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = _2;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                  CamlinternalMenhirLib.EngineTypes.semv = _1;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                  CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                };
              };
            };
          };
        } = _menhir_stack in
        let _5 : unit = Obj.magic _5 in
        let closure_id : 'tv_closure_id_opt = Obj.magic closure_id in
        let code_id : 'tv_code_id = Obj.magic code_id in
        let _2 : unit = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : 'tv_call_kind = 
# 388 "flambda_parser.mly"
    ( Function (Direct { code_id; closure_id }) )
# 1087 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = noalloc;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_noalloc_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_noalloc_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let noalloc : 'tv_boption_NOALLOC_ = Obj.magic noalloc in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_noalloc_ in
        let _v : 'tv_call_kind = 
# 390 "flambda_parser.mly"
    ( C_call { alloc = not noalloc } )
# 1119 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = value;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_value_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_value_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = var;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_var_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_var_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let value : 'tv_simple = Obj.magic value in
        let _2 : unit = Obj.magic _2 in
        let var : 'tv_var_within_closure = Obj.magic var in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_var_ in
        let _endpos = _endpos_value_ in
        let _v : 'tv_closure_element = 
# 362 "flambda_parser.mly"
                                                     ( { var; value; } )
# 1158 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_v_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_v_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let v : 'tv_variable = Obj.magic v in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_v_ in
        let _endpos = _endpos_v_ in
        let _v : 'tv_closure_id = 
# 480 "flambda_parser.mly"
                 ( v )
# 1183 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_closure_id_opt = 
# 484 "flambda_parser.mly"
    ( None )
# 1201 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = cid;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_cid_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_cid_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let cid : 'tv_closure_id = Obj.magic cid in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_cid_ in
        let _v : 'tv_closure_id_opt = 
# 485 "flambda_parser.mly"
                         ( Some cid )
# 1233 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = body;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_body_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_body_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _8;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__8_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__8_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = ret_arity;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_ret_arity_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_ret_arity_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = exn_cont;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_exn_cont_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_exn_cont_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _;
                  CamlinternalMenhirLib.EngineTypes.semv = ret_cont;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos_ret_cont_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos_ret_cont_;
                  CamlinternalMenhirLib.EngineTypes.next = {
                    CamlinternalMenhirLib.EngineTypes.state = _;
                    CamlinternalMenhirLib.EngineTypes.semv = _4;
                    CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
                    CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
                    CamlinternalMenhirLib.EngineTypes.next = {
                      CamlinternalMenhirLib.EngineTypes.state = _;
                      CamlinternalMenhirLib.EngineTypes.semv = closure_var;
                      CamlinternalMenhirLib.EngineTypes.startp = _startpos_closure_var_;
                      CamlinternalMenhirLib.EngineTypes.endp = _endpos_closure_var_;
                      CamlinternalMenhirLib.EngineTypes.next = {
                        CamlinternalMenhirLib.EngineTypes.state = _;
                        CamlinternalMenhirLib.EngineTypes.semv = params;
                        CamlinternalMenhirLib.EngineTypes.startp = _startpos_params_;
                        CamlinternalMenhirLib.EngineTypes.endp = _endpos_params_;
                        CamlinternalMenhirLib.EngineTypes.next = {
                          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                          CamlinternalMenhirLib.EngineTypes.semv = header;
                          CamlinternalMenhirLib.EngineTypes.startp = _startpos_header_;
                          CamlinternalMenhirLib.EngineTypes.endp = _endpos_header_;
                          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                        };
                      };
                    };
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let body : 'tv_expr = Obj.magic body in
        let _8 : unit = Obj.magic _8 in
        let ret_arity : 'tv_return_arity = Obj.magic ret_arity in
        let exn_cont : 'tv_option_exn_continuation_id_ = Obj.magic exn_cont in
        let ret_cont : 'tv_continuation_id = Obj.magic ret_cont in
        let _4 : unit = Obj.magic _4 in
        let closure_var : 'tv_variable_opt = Obj.magic closure_var in
        let params : 'tv_kinded_args = Obj.magic params in
        let header : 'tv_code_header = Obj.magic header in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_header_ in
        let _endpos = _endpos_body_ in
        let _v : 'tv_code = 
# 159 "flambda_parser.mly"
    ( let recursive, id, newer_version_of = header in
      { id; newer_version_of; param_arity = None; ret_arity; recursive;
        params_and_body = Present { params; closure_var; ret_cont; exn_cont;
                                    body } } )
# 1317 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = ret_arity;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_ret_arity_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_ret_arity_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _5;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__5_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__5_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = param_arity;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_param_arity_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_param_arity_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = _3;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _;
                  CamlinternalMenhirLib.EngineTypes.semv = _2;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
                  CamlinternalMenhirLib.EngineTypes.next = {
                    CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                    CamlinternalMenhirLib.EngineTypes.semv = header;
                    CamlinternalMenhirLib.EngineTypes.startp = _startpos_header_;
                    CamlinternalMenhirLib.EngineTypes.endp = _endpos_header_;
                    CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let ret_arity : 'tv_kinds = Obj.magic ret_arity in
        let _5 : unit = Obj.magic _5 in
        let param_arity : 'tv_kinds = Obj.magic param_arity in
        let _3 : unit = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let header : 'tv_code_header = Obj.magic header in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_header_ in
        let _endpos = _endpos_ret_arity_ in
        let _v : 'tv_code = 
# 169 "flambda_parser.mly"
    ( let recursive, id, newer_version_of = header in
      { id; newer_version_of; param_arity = Some param_arity;
        ret_arity = Some ret_arity; recursive; params_and_body = Deleted } )
# 1379 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = newer_version_of;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_newer_version_of_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_newer_version_of_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = id;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_id_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_id_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = recursive;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_recursive_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_recursive_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = _1;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let newer_version_of : 'tv_option___anonymous_0_ = Obj.magic newer_version_of in
        let id : 'tv_code_id = Obj.magic id in
        let recursive : 'tv_recursive = Obj.magic recursive in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_newer_version_of_ in
        let _v : 'tv_code_header = 
# 179 "flambda_parser.mly"
    ( recursive, id, newer_version_of )
# 1425 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_v_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_v_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let v : 'tv_variable = Obj.magic v in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_v_ in
        let _endpos = _endpos_v_ in
        let _v : 'tv_code_id = 
# 476 "flambda_parser.mly"
                 ( v )
# 1450 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = c;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_c_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_c_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let c : (
# 69 "flambda_parser.mly"
       (string * char option)
# 1471 "flambda_parser-in.ml"
        ) = Obj.magic c in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_const = 
# 454 "flambda_parser.mly"
            ( make_const_int c )
# 1479 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = c;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_c_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_c_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let c : (
# 62 "flambda_parser.mly"
       (string * char option)
# 1500 "flambda_parser-in.ml"
        ) = Obj.magic c in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_const = 
# 455 "flambda_parser.mly"
              ( make_const_float c )
# 1508 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = e;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_e_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_e_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let e : 'tv_continuation_id = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_continuation = 
# 506 "flambda_parser.mly"
                        ( Named e )
# 1533 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = s;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let s : 'tv_special_continuation = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : 'tv_continuation = 
# 507 "flambda_parser.mly"
                             ( Special s )
# 1558 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = l;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_l_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_l_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let l : 'tv_let_expr_continuation_body_ = Obj.magic l in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_l_ in
        let _endpos = _endpos_l_ in
        let _v : 'tv_continuation_body = 
# 308 "flambda_parser.mly"
                                    ( l )
# 1583 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = a;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_a_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_a_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let a : 'tv_atomic_expr = Obj.magic a in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_a_ in
        let _endpos = _endpos_a_ in
        let _v : 'tv_continuation_body = 
# 309 "flambda_parser.mly"
                    ( a )
# 1608 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = handler;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_handler_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_handler_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _4;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = params;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_params_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_params_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = name;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_name_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_name_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                  CamlinternalMenhirLib.EngineTypes.semv = exn_and_stub;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos_exn_and_stub_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos_exn_and_stub_;
                  CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                };
              };
            };
          };
        } = _menhir_stack in
        let handler : 'tv_continuation_body = Obj.magic handler in
        let _4 : unit = Obj.magic _4 in
        let params : 'tv_kinded_args = Obj.magic params in
        let name : 'tv_continuation_id = Obj.magic name in
        let exn_and_stub : 'tv_exn_and_stub = Obj.magic exn_and_stub in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_exn_and_stub_ in
        let _endpos = _endpos_handler_ in
        let _v : 'tv_continuation_handler = 
# 409 "flambda_parser.mly"
    ( let is_exn_handler, stub = exn_and_stub in
      { name; params; stub; is_exn_handler; handler } )
# 1662 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = e;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_e_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_e_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let e : (
# 72 "flambda_parser.mly"
       (string)
# 1683 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_continuation_id = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 502 "flambda_parser.mly"
               ( make_located e (_startpos, _endpos) )
# 1693 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_exn_and_stub = 
# 399 "flambda_parser.mly"
    ( false, false )
# 1711 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_exn_and_stub = 
# 400 "flambda_parser.mly"
         ( false, true )
# 1736 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_exn_and_stub = 
# 401 "flambda_parser.mly"
        ( true, false )
# 1761 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _2;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let _2 : unit = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_exn_and_stub = 
# 402 "flambda_parser.mly"
             ( true, true )
# 1793 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _2;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let _2 : unit = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_exn_and_stub = 
# 403 "flambda_parser.mly"
             ( true, true )
# 1825 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = cont;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_cont_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_cont_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let cont : 'tv_continuation = Obj.magic cont in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_cont_ in
        let _v : 'tv_exn_continuation = 
# 132 "flambda_parser.mly"
                             ( cont )
# 1857 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = cont;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_cont_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_cont_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let cont : 'tv_continuation_id = Obj.magic cont in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_cont_ in
        let _v : 'tv_exn_continuation_id = 
# 135 "flambda_parser.mly"
                                ( cont )
# 1889 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = l;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_l_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_l_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let l : 'tv_let_expr_expr_ = Obj.magic l in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_l_ in
        let _endpos = _endpos_l_ in
        let _v : 'tv_expr = 
# 287 "flambda_parser.mly"
                       ( l )
# 1914 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = i;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_i_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_i_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let i : 'tv_inner_expr = Obj.magic i in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_i_ in
        let _endpos = _endpos_i_ in
        let _v : 'tv_expr = 
# 288 "flambda_parser.mly"
                   ( i )
# 1939 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _2;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = body;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_body_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_body_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let _2 : unit = Obj.magic _2 in
        let body : 'tv_module_ = Obj.magic body in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_body_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 109 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 1971 "flambda_parser-in.ml"
        ) = 
# 119 "flambda_parser.mly"
    ( body )
# 1975 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = closure_id;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_closure_id_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_closure_id_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = code_id;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_code_id_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_code_id_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = is_tupled;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_is_tupled_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_is_tupled_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = _1;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let closure_id : 'tv_closure_id_opt = Obj.magic closure_id in
        let code_id : 'tv_code_id = Obj.magic code_id in
        let is_tupled : 'tv_boption_TUPLED_ = Obj.magic is_tupled in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_closure_id_ in
        let _v : 'tv_fun_decl = 
# 368 "flambda_parser.mly"
    ( { code_id; closure_id; is_tupled } )
# 2021 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = n;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_n_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_n_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let n : 'tv_name = Obj.magic n in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos_n_ in
        let _v : 'tv_func_name_with_optional_arities = 
# 464 "flambda_parser.mly"
             ( n, None )
# 2046 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _7;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__7_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__7_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = ret_arity;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_ret_arity_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_ret_arity_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _5;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__5_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__5_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = params_arity;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_params_arity_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_params_arity_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _;
                  CamlinternalMenhirLib.EngineTypes.semv = _3;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
                  CamlinternalMenhirLib.EngineTypes.next = {
                    CamlinternalMenhirLib.EngineTypes.state = _;
                    CamlinternalMenhirLib.EngineTypes.semv = n;
                    CamlinternalMenhirLib.EngineTypes.startp = _startpos_n_;
                    CamlinternalMenhirLib.EngineTypes.endp = _endpos_n_;
                    CamlinternalMenhirLib.EngineTypes.next = {
                      CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                      CamlinternalMenhirLib.EngineTypes.semv = _1;
                      CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                      CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                      CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                    };
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let _7 : unit = Obj.magic _7 in
        let ret_arity : 'tv_kinds = Obj.magic ret_arity in
        let _5 : unit = Obj.magic _5 in
        let params_arity : 'tv_kinds = Obj.magic params_arity in
        let _3 : unit = Obj.magic _3 in
        let n : 'tv_name = Obj.magic n in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__7_ in
        let _v : 'tv_func_name_with_optional_arities = 
# 467 "flambda_parser.mly"
    ( n, Some ({ params_arity; ret_arity } : function_arities) )
# 2113 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_infix_binop = 
# 209 "flambda_parser.mly"
         ( Plus )
# 2138 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_infix_binop = 
# 210 "flambda_parser.mly"
            ( Plusdot )
# 2163 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_infix_binop = 
# 211 "flambda_parser.mly"
          ( Minus )
# 2188 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_infix_binop = 
# 212 "flambda_parser.mly"
             ( Minusdot )
# 2213 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = w;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_w_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_w_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let w : 'tv_where_expr = Obj.magic w in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_w_ in
        let _endpos = _endpos_w_ in
        let _v : 'tv_inner_expr = 
# 297 "flambda_parser.mly"
                   ( w )
# 2238 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = a;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_a_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_a_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let a : 'tv_atomic_expr = Obj.magic a in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_a_ in
        let _endpos = _endpos_a_ in
        let _v : 'tv_inner_expr = 
# 298 "flambda_parser.mly"
                    ( a )
# 2263 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 111 "flambda_parser.mly"
      (Fexpr.kind)
# 2288 "flambda_parser-in.ml"
        ) = 
# 251 "flambda_parser.mly"
        ( Value )
# 2292 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 111 "flambda_parser.mly"
      (Fexpr.kind)
# 2317 "flambda_parser-in.ml"
        ) = 
# 252 "flambda_parser.mly"
        ( Naked_number Naked_immediate )
# 2321 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 111 "flambda_parser.mly"
      (Fexpr.kind)
# 2346 "flambda_parser-in.ml"
        ) = 
# 253 "flambda_parser.mly"
               ( Naked_number Naked_float )
# 2350 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 111 "flambda_parser.mly"
      (Fexpr.kind)
# 2375 "flambda_parser-in.ml"
        ) = 
# 254 "flambda_parser.mly"
          ( Naked_number Naked_int32 )
# 2379 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 111 "flambda_parser.mly"
      (Fexpr.kind)
# 2404 "flambda_parser-in.ml"
        ) = 
# 255 "flambda_parser.mly"
          ( Naked_number Naked_int64 )
# 2408 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 111 "flambda_parser.mly"
      (Fexpr.kind)
# 2433 "flambda_parser-in.ml"
        ) = 
# 256 "flambda_parser.mly"
              ( Naked_number Naked_nativeint )
# 2437 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 111 "flambda_parser.mly"
      (Fexpr.kind)
# 2462 "flambda_parser-in.ml"
        ) = 
# 257 "flambda_parser.mly"
               ( Fabricated )
# 2466 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_kind_arg_opt = 
# 269 "flambda_parser.mly"
    ( None )
# 2484 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _3;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = k;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_k_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_k_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = _1;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let _3 : unit = Obj.magic _3 in
        let k : (
# 111 "flambda_parser.mly"
      (Fexpr.kind)
# 2518 "flambda_parser-in.ml"
        ) = Obj.magic k in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_kind_arg_opt = 
# 270 "flambda_parser.mly"
                             ( Some k )
# 2527 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _3;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = v;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_v_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_v_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = _1;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let _3 : unit = Obj.magic _3 in
        let v : 'tv_separated_nonempty_list_COMMA_kinded_variable_ = Obj.magic v in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_kinded_args = 
# 414 "flambda_parser.mly"
                                                                      ( v )
# 2566 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_kinded_args = 
# 415 "flambda_parser.mly"
    ( [] )
# 2584 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = param;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_param_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_param_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let param : 'tv_variable = Obj.magic param in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_param_ in
        let _endpos = _endpos_param_ in
        let _v : 'tv_kinded_variable = 
# 439 "flambda_parser.mly"
                     ( { param; kind = None } )
# 2609 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = kind;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_kind_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_kind_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = param;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_param_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_param_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let kind : (
# 111 "flambda_parser.mly"
      (Fexpr.kind)
# 2642 "flambda_parser-in.ml"
        ) = Obj.magic kind in
        let _2 : unit = Obj.magic _2 in
        let param : 'tv_variable = Obj.magic param in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_param_ in
        let _endpos = _endpos_kind_ in
        let _v : 'tv_kinded_variable = 
# 440 "flambda_parser.mly"
                                         ( { param; kind = Some kind } )
# 2652 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_v_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_v_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let v : 'tv_variable_opt = Obj.magic v in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_v_ in
        let _endpos = _endpos_v_ in
        let _v : 'tv_kinded_variable_opt = 
# 444 "flambda_parser.mly"
                     ( v, None )
# 2677 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = kind;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_kind_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_kind_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = v;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_v_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_v_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let kind : (
# 111 "flambda_parser.mly"
      (Fexpr.kind)
# 2710 "flambda_parser-in.ml"
        ) = Obj.magic kind in
        let _2 : unit = Obj.magic _2 in
        let v : 'tv_variable_opt = Obj.magic v in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_v_ in
        let _endpos = _endpos_kind_ in
        let _v : 'tv_kinded_variable_opt = 
# 445 "flambda_parser.mly"
                                          ( v, Some kind )
# 2720 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_kinds = 
# 260 "flambda_parser.mly"
         ( [] )
# 2745 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = ks;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_ks_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_ks_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let ks : 'tv_separated_nonempty_list_STAR_kind_ = Obj.magic ks in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_ks_ in
        let _endpos = _endpos_ks_ in
        let _v : 'tv_kinds = 
# 261 "flambda_parser.mly"
                                             ( ks )
# 2770 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = body;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_body_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_body_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _3;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = closure_elements;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_closure_elements_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_closure_elements_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = bindings;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_bindings_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_bindings_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let body : 'tv_continuation_body = Obj.magic body in
        let _3 : unit = Obj.magic _3 in
        let closure_elements : 'tv_with_closure_elements_opt = Obj.magic closure_elements in
        let bindings : 'tv_separated_nonempty_list_AND_let_binding_ = Obj.magic bindings in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_bindings_ in
        let _endpos = _endpos_body_ in
        let _v : 'tv_let__continuation_body_ = 
# 345 "flambda_parser.mly"
    ( ({ bindings; closure_elements; body } : let_) )
# 2816 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = body;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_body_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_body_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _3;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = closure_elements;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_closure_elements_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_closure_elements_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = bindings;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_bindings_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_bindings_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let body : 'tv_expr = Obj.magic body in
        let _3 : unit = Obj.magic _3 in
        let closure_elements : 'tv_with_closure_elements_opt = Obj.magic closure_elements in
        let bindings : 'tv_separated_nonempty_list_AND_let_binding_ = Obj.magic bindings in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_bindings_ in
        let _endpos = _endpos_body_ in
        let _v : 'tv_let__expr_ = 
# 345 "flambda_parser.mly"
    ( ({ bindings; closure_elements; body } : let_) )
# 2862 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = defining_expr;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_defining_expr_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_defining_expr_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = v;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_v_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_v_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let defining_expr : (
# 112 "flambda_parser.mly"
      (Fexpr.named)
# 2895 "flambda_parser-in.ml"
        ) = Obj.magic defining_expr in
        let _2 : unit = Obj.magic _2 in
        let v : 'tv_kinded_variable_opt = Obj.magic v in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_v_ in
        let _endpos = _endpos_defining_expr_ in
        let _v : 'tv_let_binding = 
# 350 "flambda_parser.mly"
      ( let (var, kind) = v in { var; kind; defining_expr } )
# 2905 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = l;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_l_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_l_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let l : 'tv_let__continuation_body_ = Obj.magic l in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_l_ in
        let _v : 'tv_let_expr_continuation_body_ = 
# 292 "flambda_parser.mly"
                       ( Let l )
# 2937 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = ls;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_ls_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_ls_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let ls : 'tv_let_symbol_continuation_body_ = Obj.magic ls in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_ls_ in
        let _endpos = _endpos_ls_ in
        let _v : 'tv_let_expr_continuation_body_ = 
# 293 "flambda_parser.mly"
                          ( Let_symbol ls )
# 2962 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = l;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_l_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_l_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let l : 'tv_let__expr_ = Obj.magic l in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_l_ in
        let _v : 'tv_let_expr_expr_ = 
# 292 "flambda_parser.mly"
                       ( Let l )
# 2994 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = ls;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_ls_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_ls_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let ls : 'tv_let_symbol_expr_ = Obj.magic ls in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_ls_ in
        let _endpos = _endpos_ls_ in
        let _v : 'tv_let_expr_expr_ = 
# 293 "flambda_parser.mly"
                          ( Let_symbol ls )
# 3019 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = body;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_body_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_body_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _4;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = closure_elements;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_closure_elements_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_closure_elements_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = bindings;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_bindings_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_bindings_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                  CamlinternalMenhirLib.EngineTypes.semv = _1;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                  CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                };
              };
            };
          };
        } = _menhir_stack in
        let body : 'tv_continuation_body = Obj.magic body in
        let _4 : unit = Obj.magic _4 in
        let closure_elements : 'tv_with_closure_elements_opt = Obj.magic closure_elements in
        let bindings : 'tv_separated_nonempty_list_AND_symbol_binding_ = Obj.magic bindings in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_body_ in
        let _v : 'tv_let_symbol_continuation_body_ = 
# 141 "flambda_parser.mly"
                     ( { bindings; closure_elements; body } )
# 3072 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = body;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_body_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_body_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _4;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = closure_elements;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_closure_elements_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_closure_elements_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = bindings;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_bindings_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_bindings_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                  CamlinternalMenhirLib.EngineTypes.semv = _1;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                  CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                };
              };
            };
          };
        } = _menhir_stack in
        let body : 'tv_expr = Obj.magic body in
        let _4 : unit = Obj.magic _4 in
        let closure_elements : 'tv_with_closure_elements_opt = Obj.magic closure_elements in
        let bindings : 'tv_separated_nonempty_list_AND_symbol_binding_ = Obj.magic bindings in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_body_ in
        let _v : 'tv_let_symbol_expr_ = 
# 141 "flambda_parser.mly"
                     ( { bindings; closure_elements; body } )
# 3125 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_loption_separated_nonempty_list_ANDWHERE_continuation_handler__ = 
# 142 "<standard.mly>"
    ( [] )
# 3143 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_separated_nonempty_list_ANDWHERE_continuation_handler_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_ANDWHERE_continuation_handler__ = 
# 144 "<standard.mly>"
    ( x )
# 3168 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_loption_separated_nonempty_list_COMMA_simple__ = 
# 142 "<standard.mly>"
    ( [] )
# 3186 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_separated_nonempty_list_COMMA_simple_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_COMMA_simple__ = 
# 144 "<standard.mly>"
    ( x )
# 3211 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_loption_separated_nonempty_list_PIPE_switch_case__ = 
# 142 "<standard.mly>"
    ( [] )
# 3229 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_separated_nonempty_list_PIPE_switch_case_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_PIPE_switch_case__ = 
# 144 "<standard.mly>"
    ( x )
# 3254 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__ = 
# 142 "<standard.mly>"
    ( [] )
# 3272 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_separated_nonempty_list_SEMICOLON_closure_element_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__ = 
# 144 "<standard.mly>"
    ( x )
# 3297 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = body;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_body_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_body_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let body : 'tv_expr = Obj.magic body in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_body_ in
        let _endpos = _endpos_body_ in
        let _v : 'tv_module_ = 
# 128 "flambda_parser.mly"
    ( { body } )
# 3322 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = s;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let s : 'tv_symbol = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : 'tv_name = 
# 459 "flambda_parser.mly"
               ( (Symbol s:name) )
# 3347 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_v_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_v_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let v : 'tv_variable = Obj.magic v in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_v_ in
        let _endpos = _endpos_v_ in
        let _v : 'tv_name = 
# 460 "flambda_parser.mly"
                 ( (Var v:name) )
# 3372 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = s;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let s : 'tv_simple = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : (
# 112 "flambda_parser.mly"
      (Fexpr.named)
# 3397 "flambda_parser-in.ml"
        ) = 
# 236 "flambda_parser.mly"
               ( Simple s )
# 3401 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = a;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_a_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_a_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = u;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_u_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_u_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let a : 'tv_simple = Obj.magic a in
        let u : 'tv_unop = Obj.magic u in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_u_ in
        let _endpos = _endpos_a_ in
        let _v : (
# 112 "flambda_parser.mly"
      (Fexpr.named)
# 3433 "flambda_parser-in.ml"
        ) = 
# 237 "flambda_parser.mly"
                        ( Prim (Unary (u, a)) )
# 3437 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = b;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_b_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_b_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let b : 'tv_binop_app = Obj.magic b in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_b_ in
        let _endpos = _endpos_b_ in
        let _v : (
# 112 "flambda_parser.mly"
      (Fexpr.named)
# 3462 "flambda_parser-in.ml"
        ) = 
# 238 "flambda_parser.mly"
                  ( Prim b )
# 3466 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = b;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_b_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_b_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let b : 'tv_block = Obj.magic b in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_b_ in
        let _endpos = _endpos_b_ in
        let _v : (
# 112 "flambda_parser.mly"
      (Fexpr.named)
# 3491 "flambda_parser-in.ml"
        ) = 
# 239 "flambda_parser.mly"
              ( Prim b )
# 3495 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = c;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_c_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_c_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let c : 'tv_fun_decl = Obj.magic c in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : (
# 112 "flambda_parser.mly"
      (Fexpr.named)
# 3520 "flambda_parser-in.ml"
        ) = 
# 240 "flambda_parser.mly"
                 ( Closure c )
# 3524 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = s;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let s : 'tv_symbol = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : (
# 113 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 3549 "flambda_parser-in.ml"
        ) = 
# 433 "flambda_parser.mly"
               ( Symbol s )
# 3553 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_v_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_v_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let v : 'tv_variable = Obj.magic v in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_v_ in
        let _endpos = _endpos_v_ in
        let _v : (
# 113 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 3578 "flambda_parser-in.ml"
        ) = 
# 434 "flambda_parser.mly"
                 ( Dynamically_computed v )
# 3582 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = i;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_i_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_i_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let i : (
# 69 "flambda_parser.mly"
       (string * char option)
# 3603 "flambda_parser-in.ml"
        ) = Obj.magic i in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_i_ in
        let _endpos = _endpos_i_ in
        let _v : (
# 113 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 3611 "flambda_parser-in.ml"
        ) = let _endpos = _endpos_i_ in
        let _startpos = _startpos_i_ in
        
# 435 "flambda_parser.mly"
            ( Tagged_immediate ( make_tagged_immediate ~loc:(_startpos, _endpos) i ) )
# 3617 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_option_PIPE_ = 
# 114 "<standard.mly>"
    ( None )
# 3635 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : unit = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_option_PIPE_ = 
# 116 "<standard.mly>"
    ( Some x )
# 3660 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_option___anonymous_0_ = 
# 114 "<standard.mly>"
    ( None )
# 3678 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = id;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_id_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_id_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let id : 'tv_code_id = Obj.magic id in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_id_ in
        let _v : 'tv_option___anonymous_0_ = let x = 
# 178 "flambda_parser.mly"
                                                             ( id )
# 3710 "flambda_parser-in.ml"
         in
        
# 116 "<standard.mly>"
    ( Some x )
# 3715 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_option_exn_continuation_id_ = 
# 114 "<standard.mly>"
    ( None )
# 3733 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_exn_continuation_id = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_option_exn_continuation_id_ = 
# 116 "<standard.mly>"
    ( Some x )
# 3758 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = k;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_k_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_k_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let k : 'tv_kind_arg_opt = Obj.magic k in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_k_ in
        let _v : 'tv_prefix_binop = 
# 216 "flambda_parser.mly"
                              ( Phys_equal(k, Eq) )
# 3790 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = k;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_k_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_k_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let k : 'tv_kind_arg_opt = Obj.magic k in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_k_ in
        let _v : 'tv_prefix_binop = 
# 217 "flambda_parser.mly"
                              ( Phys_equal(k, Neq) )
# 3822 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_recursive = 
# 195 "flambda_parser.mly"
    ( Nonrecursive )
# 3840 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_recursive = 
# 196 "flambda_parser.mly"
        ( Recursive )
# 3865 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_return_arity = 
# 264 "flambda_parser.mly"
    ( None )
# 3883 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = k;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_k_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_k_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let k : 'tv_kinds = Obj.magic k in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_k_ in
        let _v : 'tv_return_arity = 
# 265 "flambda_parser.mly"
                    ( Some k )
# 3915 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_let_binding = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_AND_let_binding_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3940 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_AND_let_binding_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_let_binding = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_AND_let_binding_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3979 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_static_closure_binding = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_AND_static_closure_binding_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4004 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_AND_static_closure_binding_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_static_closure_binding = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_AND_static_closure_binding_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4043 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_symbol_binding = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_AND_symbol_binding_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4068 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_AND_symbol_binding_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_symbol_binding = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_AND_symbol_binding_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4107 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_continuation_handler = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_ANDWHERE_continuation_handler_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4132 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_ANDWHERE_continuation_handler_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_continuation_handler = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_ANDWHERE_continuation_handler_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4171 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_kinded_variable = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_COMMA_kinded_variable_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4196 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_COMMA_kinded_variable_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_kinded_variable = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_COMMA_kinded_variable_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4235 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : (
# 113 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 4256 "flambda_parser-in.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_COMMA_of_kind_value_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4264 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_COMMA_of_kind_value_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (
# 113 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 4299 "flambda_parser-in.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_COMMA_of_kind_value_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4307 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_simple = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_COMMA_simple_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4332 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_COMMA_simple_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_simple = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_COMMA_simple_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4371 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_switch_case = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_PIPE_switch_case_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4396 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_PIPE_switch_case_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_switch_case = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_PIPE_switch_case_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4435 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_closure_element = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_closure_element_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4460 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_SEMICOLON_closure_element_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_closure_element = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_closure_element_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4499 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : (
# 111 "flambda_parser.mly"
      (Fexpr.kind)
# 4520 "flambda_parser-in.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_STAR_kind_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4528 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_STAR_kind_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (
# 111 "flambda_parser.mly"
      (Fexpr.kind)
# 4563 "flambda_parser-in.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_STAR_kind_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4571 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = s;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let s : 'tv_symbol = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : 'tv_simple = 
# 470 "flambda_parser.mly"
               ( Symbol s )
# 4596 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_v_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_v_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let v : 'tv_variable = Obj.magic v in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_v_ in
        let _endpos = _endpos_v_ in
        let _v : 'tv_simple = 
# 471 "flambda_parser.mly"
                 ( Var v )
# 4621 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = c;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_c_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_c_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let c : 'tv_const = Obj.magic c in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_simple = 
# 472 "flambda_parser.mly"
              ( Const c )
# 4646 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_simple_args = 
# 449 "flambda_parser.mly"
    ( [] )
# 4664 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _3;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = s;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = _1;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let _3 : unit = Obj.magic _3 in
        let s : 'tv_separated_nonempty_list_COMMA_simple_ = Obj.magic s in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_simple_args = 
# 450 "flambda_parser.mly"
                                                             ( s )
# 4703 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_special_continuation = 
# 511 "flambda_parser.mly"
         ( Done : special_continuation )
# 4728 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_special_continuation = 
# 512 "flambda_parser.mly"
          ( Error : special_continuation )
# 4753 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = fun_decl;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_fun_decl_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_fun_decl_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _3;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = symbol;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_symbol_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_symbol_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = _1;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let fun_decl : 'tv_fun_decl = Obj.magic fun_decl in
        let _3 : unit = Obj.magic _3 in
        let symbol : 'tv_symbol = Obj.magic symbol in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_fun_decl_ in
        let _v : 'tv_static_closure_binding = 
# 184 "flambda_parser.mly"
    ( { symbol; fun_decl } )
# 4799 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _5;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__5_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__5_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = elements;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_elements_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_elements_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _3;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = tag;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_tag_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_tag_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                  CamlinternalMenhirLib.EngineTypes.semv = _1;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                  CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                };
              };
            };
          };
        } = _menhir_stack in
        let _5 : unit = Obj.magic _5 in
        let elements : 'tv_separated_nonempty_list_COMMA_of_kind_value_ = Obj.magic elements in
        let _3 : unit = Obj.magic _3 in
        let tag : 'tv_tag = Obj.magic tag in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : 'tv_static_part = 
# 425 "flambda_parser.mly"
    ( (Block { tag; mutability = Immutable; elements } : static_part) )
# 4852 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _4;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = elements;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_elements_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_elements_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = bindings;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_bindings_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_bindings_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = _1;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let _4 : unit = Obj.magic _4 in
        let elements : 'tv_with_closure_elements_opt = Obj.magic elements in
        let bindings : 'tv_separated_nonempty_list_AND_static_closure_binding_ = Obj.magic bindings in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : 'tv_static_set_of_closures = 
# 192 "flambda_parser.mly"
    ( { bindings; elements } )
# 4898 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = sp;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_sp_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_sp_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _3;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = s;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = _1;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let sp : 'tv_static_part = Obj.magic sp in
        let _3 : unit = Obj.magic _3 in
        let s : 'tv_symbol = Obj.magic s in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_sp_ in
        let _v : (
# 110 "flambda_parser.mly"
      (Fexpr.static_structure)
# 4944 "flambda_parser-in.ml"
        ) = 
# 419 "flambda_parser.mly"
    ( { symbol = s; kind = None; defining_expr = sp } )
# 4948 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let xs : 'tv_loption_separated_nonempty_list_PIPE_switch_case__ = Obj.magic xs in
        let _1 : 'tv_option_PIPE_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_switch = let cs = 
# 232 "<standard.mly>"
    ( xs )
# 4980 "flambda_parser-in.ml"
         in
        
# 248 "flambda_parser.mly"
                                                         ( cs )
# 4985 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = ac;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_ac_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_ac_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = i;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_i_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_i_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let ac : 'tv_apply_cont_expr = Obj.magic ac in
        let _2 : unit = Obj.magic _2 in
        let i : 'tv_tag = Obj.magic i in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_i_ in
        let _endpos = _endpos_ac_ in
        let _v : 'tv_switch_case = 
# 244 "flambda_parser.mly"
                                                ( i,ac )
# 5024 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = e;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_e_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_e_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let e : (
# 98 "flambda_parser.mly"
       (string)
# 5045 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_symbol = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 489 "flambda_parser.mly"
               ( make_located e (_startpos, _endpos) )
# 5055 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = s;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let s : (
# 110 "flambda_parser.mly"
      (Fexpr.static_structure)
# 5076 "flambda_parser-in.ml"
        ) = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : 'tv_symbol_binding = 
# 145 "flambda_parser.mly"
                         ( Block_like s )
# 5084 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = code;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_code_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_code_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let code : 'tv_code = Obj.magic code in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_code_ in
        let _endpos = _endpos_code_ in
        let _v : 'tv_symbol_binding = 
# 146 "flambda_parser.mly"
                ( Code code )
# 5109 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = s;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let s : 'tv_static_closure_binding = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : 'tv_symbol_binding = 
# 147 "flambda_parser.mly"
                               ( Closure s )
# 5134 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = s;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let s : 'tv_static_set_of_closures = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : 'tv_symbol_binding = 
# 148 "flambda_parser.mly"
                               ( Set_of_closures s )
# 5159 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = tag;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_tag_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_tag_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let tag : (
# 69 "flambda_parser.mly"
       (string * char option)
# 5180 "flambda_parser-in.ml"
        ) = Obj.magic tag in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_tag_ in
        let _endpos = _endpos_tag_ in
        let _v : 'tv_tag = let _endpos = _endpos_tag_ in
        let _startpos = _startpos_tag_ in
        
# 429 "flambda_parser.mly"
            ( make_tag ~loc:(make_loc (_startpos, _endpos)) tag )
# 5190 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_unop = 
# 200 "flambda_parser.mly"
           ( Opaque_identity )
# 5215 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_unop = 
# 201 "flambda_parser.mly"
              ( Untag_imm )
# 5240 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = var;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_var_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_var_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _3;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = project_from;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_project_from_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_project_from_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = _1;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let var : 'tv_var_within_closure = Obj.magic var in
        let _3 : unit = Obj.magic _3 in
        let project_from : 'tv_closure_id = Obj.magic project_from in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_var_ in
        let _v : 'tv_unop = 
# 203 "flambda_parser.mly"
    ( Project_var { project_from; var } )
# 5286 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _6;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__6_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__6_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = move_to;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_move_to_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_move_to_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _4;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = move_from;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_move_from_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_move_from_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _;
                  CamlinternalMenhirLib.EngineTypes.semv = _2;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
                  CamlinternalMenhirLib.EngineTypes.next = {
                    CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                    CamlinternalMenhirLib.EngineTypes.semv = _1;
                    CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                    CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                    CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let _6 : unit = Obj.magic _6 in
        let move_to : 'tv_closure_id = Obj.magic move_to in
        let _4 : unit = Obj.magic _4 in
        let move_from : 'tv_closure_id = Obj.magic move_from in
        let _2 : unit = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__6_ in
        let _v : 'tv_unop = 
# 206 "flambda_parser.mly"
    ( Select_closure { move_from; move_to } )
# 5346 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = e;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_e_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_e_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let e : (
# 72 "flambda_parser.mly"
       (string)
# 5367 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_var_within_closure = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 516 "flambda_parser.mly"
               ( make_located e (_startpos, _endpos) )
# 5377 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = e;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_e_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_e_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let e : (
# 72 "flambda_parser.mly"
       (string)
# 5398 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_variable = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 493 "flambda_parser.mly"
               ( make_located e (_startpos, _endpos) )
# 5408 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_variable_opt = 
# 497 "flambda_parser.mly"
               ( None )
# 5433 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = e;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_e_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_e_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let e : (
# 72 "flambda_parser.mly"
       (string)
# 5454 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_variable_opt = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 498 "flambda_parser.mly"
               ( Some (make_located e (_startpos, _endpos)) )
# 5464 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = recursive;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_recursive_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_recursive_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _2;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = body;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_body_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_body_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let xs : 'tv_loption_separated_nonempty_list_ANDWHERE_continuation_handler__ = Obj.magic xs in
        let recursive : 'tv_recursive = Obj.magic recursive in
        let _2 : unit = Obj.magic _2 in
        let body : 'tv_inner_expr = Obj.magic body in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_body_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_where_expr = let handlers = 
# 232 "<standard.mly>"
    ( xs )
# 5510 "flambda_parser-in.ml"
         in
        
# 304 "flambda_parser.mly"
     ( Let_cont { recursive; body; handlers } )
# 5515 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_with_closure_elements_opt = 
# 354 "flambda_parser.mly"
    ( None )
# 5533 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _4;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = xs;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _2;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = _1;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let _4 : unit = Obj.magic _4 in
        let xs : 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : 'tv_with_closure_elements_opt = let elements = 
# 232 "<standard.mly>"
    ( xs )
# 5579 "flambda_parser-in.ml"
         in
        
# 358 "flambda_parser.mly"
    ( Some elements )
# 5584 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
    |]
  
  and trace =
    None
  
end

module MenhirInterpreter = struct
  
  module ET = CamlinternalMenhirLib.TableInterpreter.MakeEngineTable (Tables)
  
  module TI = CamlinternalMenhirLib.Engine.Make (ET)
  
  include TI
  
end

let flambda_unit =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 0 lexer lexbuf) : (
# 109 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 5615 "flambda_parser-in.ml"
    ))

module Incremental = struct
  
  let flambda_unit =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 109 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 5625 "flambda_parser-in.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 518 "flambda_parser.mly"
  

# 5633 "flambda_parser-in.ml"

# 269 "<standard.mly>"
  

# 5638 "flambda_parser-in.ml"
