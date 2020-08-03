
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
# 99 "flambda_parser.mly"
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
# 73 "flambda_parser.mly"
       (string)
# 53 "flambda_parser-in.ml"
  )
    | LET
    | LBRACE
    | INT64
    | INT32
    | INT of (
# 70 "flambda_parser.mly"
       (string * char option)
# 62 "flambda_parser-in.ml"
  )
    | IN
    | IMM
    | HCF
    | FLOAT_KIND
    | FLOAT of (
# 63 "flambda_parser.mly"
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
    | BIGARROW
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


# 142 "flambda_parser-in.ml"

module Tables = struct
  
  include MenhirBasics
  
  let token2terminal : token -> int =
    fun _tok ->
      match _tok with
      | AND ->
          66
      | ANDWHERE ->
          65
      | APPLY ->
          64
      | AT ->
          63
      | BIGARROW ->
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
      | BIGARROW ->
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
    (8, "\000\006\000\148\159\030\031\136\135\000h\b\000\153\000\000%\141\140! \000\000\000\000\128\137\000\139\003\147\000\000\130Y\146\000\000\160\000\000\000\000\018\000\029\000\000\023\0251\000\000\000fed\000\143\000\000~\145\142\000\000\000\000\000\000v\000\000\000\158\000\000\022[\000\164\000\000\132\000\144\161\000p\000\000\000j\028\000\000:?>=;<@H\000\000\149\152\151x\000\000\000\000F\000C\000\000|\000\000IJ\000\000\027\000\000\134\000\000\000\000\000,\000\000r\000\000\005\000\007\000\000\000\000\000\020\000\016\021\000\000^]\000\000\000\000\00032\000\000\000\000\000+\004\t8Q.\000\000\000\000)\000*U\162\000\000\000\000\000\000\000\000O\"R#\000\000\000K\000\000t\000\000\155\000\000\000\000\000\157\000\000\000\156\000\000\000Bn\000m\154\000\000\000W\000\014\000`\0005476\000\000\000\011\000\r\000\000\000\000\000\012McbaN\150$\000\000z\0269l\000\000\000S\000\000\000LP\000\n\000\000\000-\\\001\000\0000\002")
  
  and error =
    (67, "\004\016\000\000P \001\000\128\000\000\000\000\000\000\000\000\002\000\000\000\132 \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\b \0000\b\000L\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\000\004\000\000\024\004\000&\000\000\000\000\000\000\000\000\000\000\000\000\128\000\000\000\000\000\000\000\001\000\b@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\192\000\130\000\129\000\128\004\192 \000\000\bB\000\000\000\000\002\000\000\000\000\004\000\000\128\000\000!\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\002\b\000\004\002\000\019\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000A\000\000\005\002\000\016\b\002D\000\000@\000\000@\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\128\000\000\000\000\000\000\000\000\002\128\001\000\000\000\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000 \b\000H\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\128\000\000\000\000\000\000\004\000\000\000\000\001\000\000\000B\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000@\000\b\000\000\002\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\b\004\000\000\000\000\000\002\000\002\000@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\001\000\000\000\000\000\000\b\000\000\000\000\000\002\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000@\000\000\b\000\000\002\016\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\"\000\000\000\000\000\000\000\000\000\000\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\004\000\000!\128\000\b\000\000\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \016\000\128\000\000 2\160\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000@\000\000\000\000\128\000\000 \004@\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000@\000\000\024\000\000\128\000\000\000\000\001\000\000\000\000\000\000@\000\000\000\000\192\002\000\000\000\128\202\128\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\128\000\000\000\001\000\000\000\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000@\002@\000\000\128\202\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000$\000\000\b\012\168\000\000\000\000\000\000\000\000\000\000\002\000\"\000\016\004\004\000\001\016\000\000\004\006T\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\128\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\002\000\000\000\000\000\b\000\000\000\001\000\128\000\000\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\128@\002@\000\000\128\202\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000@\000\000A\000\000\005\002\000\016\b\000\000\000\000\000\000\000\000\000\000\000\000\b\000B\000\000\000\000\000\000\000\000\000\000\000\128\000\000`\000\004\b\000\000\000\000\b\000\000\000\000\000\000\000\000\128\000\000\000\000\000 \000\000\000\000\000\128\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\004\024\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\012\000\000\000\000\002\000\000\000\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\128\004\128\000\001\001\149\000\000\000\000\000\000\016\000\000\000\000\018\000\000\004\006T\000\000\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000$\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\000\b\000B\000\000\000 \000\000\000\000\000\000\000\000\000\000 \001\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\002\000\000\004\002\000\017`\b`\000 \130@\002,\001\b\000\004\016H\000D\000\000\000\000\128\b\000\000\000\000\000\000\000\000\000\000\000\000\128\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\000\000\b\000\016\000\000\000\000\000\000\000\002\000\000\002\b\000\000(\016\000\128@\018 \000\002\000\000\002\000\128\000\000\000\001\000\000\000\000\000\000\000\000 \000\000\000 \128\000\002\129\000\b\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\b\000\000\000\000\000\000\000\001\000\000\000\001\004\000\000\020\b\000@ \000\000\000\000\000\000\000\000@\000\000\000\000\128\000\000  \000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000@\000\000\136\b\142\002\016\128\001@\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\004\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000H\000\000\000\002\000\000\000\128\202\128\000\000\000\000@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\144\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\b\000\000\000\000\002\001\000\000\132 \000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\bB\000\000\000\000\000\000\000\000\000\000\000@\000\006\001\128\128\016\000 \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\128\000\000\000\000 \000\000\bB\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\004!\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\b\000\000\002\016\128\000\000\000\000\000\000\000\000\001\000\000 \000\000\bB\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000`\000@\000\000\128@\002`\001\000\000\004\000@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000 \000\000\000\000\000\000\000\004\000\000\000\004\016\000\000P \001\000\128\000\000\000\000\000\000\000\001\000\000\000\000\002\000\000\000\000\000\000\000\000@\000\000\000A\000\000\005\002\000\016\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000@\016@\000\001@\128\004\002\000\000\000\000\000\000 \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \128\000\002\129\000\b\004\000\000\000\000\000\000@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")
  
  and start =
    2
  
  and action =
    ((16, "\000\158\000\000\000\t\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\000\000\000\000|\000\000\000;\000;\000\000\000\000\000\000\000\000\000\000\0006\000\t\000\020\000\t\000\000\000\000\000\007\000\000\000\000\000\000\000\160\0001\000\000\000\000\000\000\000\158\000\146\000\000\000\t\000\021\000-\001N\000\000\000\t\000\000\001*\000\t\000\000\000\000\000\000\0001\000t\001\000\000\000\000\000\000\000\000\246\000\000\000\028\001\000\000\000\000\000\000\000\001,\000\t\000\244\000-\000\004\001,\000\000\000\002\001N\001<\000\000\001F\000\t\000\000\000\000\001\132\000\000\001H\001<\000\000\001L\000\000\000\000\000\014\000\000\000\t\0010\000\t\000\000\000\000\000f\000\220\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\012\000\146\000\000\000\000\000\000\000\000\000\000\000\t\000\023\000\220\000\000\001\148\000\000\000T\000\t\000\000\001F\000\220\000\000\000\000\001\130\000\220\000\000\000\248\000\220\000\000\000\142\001\132\000;\000\150\000;\000\000\000|\000\220\000\000\001b\000\158\000\000\000;\000\000\001B\001\130\000\t\001$\001\164\000\000\000\018\000\000\000\000\001\000\000\t\000\000\000\000\001V\000\220\001\142\000\220\001\170\000\000\000\000\0014\001\146\000;\001\182\000;\000\000\000\000\000\000\000\000\000\000\000\000\0018\000\014\000x\001H\000\000\001\018\000\000\000\000\000\000\000;\001@\001r\001\n\000\146\001\146\001\132\001\n\000\000\000\000\000\000\000\000\001\146\001\134\001\n\000\000\001.\000\142\000\000\001x\000\t\000\000\001\152\000\t\001\160\000\t\001\188\000\000\000\t\001z\001<\000\000\001V\000\220\001\190\000\000\000\000\001V\000\000\000\000\0001\001\164\000\t\000\000\001\198\000\000\000\t\000\000\000\128\000\000\000\000\000\000\000\000\001\168\000\t\001\202\000\000\000\t\000\000\001\172\000\t\001\128\000\t\001\208\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\232\000x\000\000\000\000\000\000\000\000\001\146\001\164\000\158\000\000\001\146\001\166\000\158\000\000\000\000\001\214\000\000\001~\000\158\001\154\000\000\000\000\000\000\000\158\001\156\000\000\000\000"), (16, "\0036\000>\001\"\001\209\000\014\000B\001\017\001\r\001\185\001\185\001\153\001\153\003:\001U\000r\0006\003R\0009\000F\001\185\003b\003v\003~\000J\001\185\001b\001\153\001\249\002%\002%\000\018\001\241\000*\001\190\001\r\000\022\002^\000\170\000\166\000\206\000\026\001\185\0009\0009\002%\001\017\001\198\001\185\001\153\001\153\002%\002\137\001\209\001\185\001\226\000\170\001\185\003\130\001\153\001\233\000Z\001M\001M\001]\001]\001y\000b\002%\001\022\001\185\000\242\001\153\002\202\001\185\001\153\001\153\002%\001M\000\154\001]\000\154\002\001\002\001\000\158\000\006\003\166\003\170\001\002\002\026\002%\000\n\000\214\002%\002%\000\149\003\174\003\178\002\001\001\218\001\025\001M\0006\001]\000\130\001y\001Z\002\210\001Z\001~\001M\001\193\001]\000\146\001\130\000\150\003\182\001\234\001\225\001\225\002&\002\001\0026\001M\001\169\001]\002\017\001M\001]\001]\002\001\001y\001^\001\169\001\225\002:\001\217\000\014\002\002\000\006\001\134\000\234\002\017\002\001\002B\000\n\002\001\002\001\001\138\001\142\002\214\000]\001\146\001\201\001\150\002\017\001\154\001\225\000\185\002\194\001\161\001\006\002j\000\018\000]\002\017\001\225\000\146\000\218\002\242\000I\001\014\000\157\002\017\000\185\001\217\0026\000\174\001e\001\225\001J\001n\004\006\001\225\002\t\002%\001\161\001\161\000Z\002:\000]\002\017\001\201\001*\001\190\000I\000I\000\185\002B\000\153\000]\001\166\000A\001\"\001&\001\001\000\185\001\161\003f\001\r\000\190\002\206\0012\000\190\001B\002F\000]\001V\003&\000\185\001\210\001\230\002Z\000\185\001\246\002\018\0022\002J\002V\002z\002\130\002\138\002\154\002\162\002\238\002\254\003\026\0032\003>\003F\003N\003Z\002\137\003n\003\138\003\150\003\186\003\194\003\210\003\218\003\226\004\"\0042\004B\004J\004S\004g"))
  
  and lhs =
    (8, "\001\000UTSSSSSSRRRQPPOONNNMLKKJJIHGGFFEEDCBBBBBA@?>>=<;;::::9988888887766554433210//..-,++**))(('&&%%%%%$$$##\"\"!!  \031\031\030\030\029\029\028\028\027\027\026\026\025\025\024\024\023\023\022\022\021\021\020\020\019\019\019\018\018\017\017\016\015\014\r\012\011\n\t\t\t\t\b\007\007\007\007\006\005\004\004\003\002\002")
  
  and goto =
    ((16, "\0006\000\000\001\018\000\000\000\000\000\000\000\000\000\000\000\000\000\019\000\000\000\000\000\012\000\000\000\000\000\130\000\000\000\000\000\000\000\000\000\000\000!\000\206\000\000\001\020\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001@\000\000\000\000\000\000\000F\000\t\000\000\000\r\000\000\000\\\000Q\000\000\000`\000\000\000\025\000L\000\000\000\000\000\000\000\192\000\000\000@\000\000\000\000\000\000\000\000\000\000\000\000\000\134\000\000\000\000\000\000\000\b\000\242\000\000\000\\\000\000\000\182\000\000\0012\000\000\001\004\000\000\000\000\001\022\000\000\000\000\000\000\000\000\000\000\0008\000\000\000\000\000\000\000\000\001\n\000\000\001N\001\144\001\166\000\000\000\000\000\000\001h\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000f\000\000\000\000\000\000\000\000\001p\000<\000\000\001r\000\000\000\000\000\000\000\000\001d\000\000\000\000\001\n\000\000\000\000\000\000\001P\000\000\000\000\001\150\000\000\001\224\000\000\001d\000\138\001l\000\000\001\184\001p\000\000\000\000\000\154\000\000\000\184\000\000\000\155\000\000\001\184\001b\000\000\000\000\001\\\000\000\000\000\000\006\001\164\000\000\000\000\000\000\001t\000\000\001\148\000\000\000\000\000\000\001\218\000\000\001F\001~\001`\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\202\000\252\000\000\000\000\000\000\000\000\000\000\000\000\001\134\001\164\000\000\000\236\000\004\002\014\000\000\001\000\000\000\000\000\000\000\000\000\002\016\000\000\001\006\000\000\000\000\001,\000\000\000\000\000\028\000\000\000\000\001\190\000\000\001\192\000\000\000\000\001\198\000\000\002\n\000\000\001\170\001\170\000\000\000\000\000\000\001\174\000\000\000\000\002\014\000\000\000\"\000\000\000\000\000\000\001`\000\000\001\172\000\000\000\000\000\000\000\000\000\000\001h\000\000\000\000\001t\000\000\000\000\001\158\000\000\001\160\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\134\000\000\000\000\000\000\000\000\002\030\000\000\000\166\000\000\002 \000\000\000\168\000\000\000\000\000\000\000\000\000\000\000n\000\000\000\000\000\000\000\000\000l\000\000\000\000\000\000"), (16, "\000_\000\154\000\030\000\012\000)\000i\000_\000\172\000\156\000k\000l\000i\000m\000\157\000\015\000k\000l\000 \000m\000\b\000E\000\231\000\b\001\007\000\t\001\011\000\r\000\t\000#\000\190\000\173\000\197\000H\000\233\000L\000q\000\024\0008\000\173\000-\000\228\000\164\0009\0001\000\201\001\015\000\244\000$\000\204\000T\000\201\000\250\000\254\000.\000\204\000t\000:\000\173\000\173\000\229\000i\000?\000\165\0003\000k\000l\001\018\000m\000<\000o\000\255\000\174\0008\000\175\000\251\000o\000\255\0009\000n\000\174\000\173\000\175\000\020\000v\000\176\000\027\000\173\000\173\000\027\001\022\001\023\000>\000\176\001\025\001\020\000\252\000\253\001\016\000\174\000\174\000\175\000\175\000\137\000<\0005\000R\000@\000E\000\b\000\020\001\005\000\176\000\176\000\t\0002\001\027\001\022\001\022\001\005\000G\000/\000\174\000\024\000\175\000o\000\255\000\028\000\174\000\174\000\175\000\175\000C\000\021\001\006\000\176\000\022\000L\001\005\001\005\001\004\000\176\000\176\000\b\000\b\000\b\001\n\001\014\000\t\000\t\000\t\000\031\000O\000\183\000|\000_\000U\000\n\000\024\000N\000\021\001\005\000\026\000\022\000\193\000Z\000\194\001\005\001\005\000\015\000\184\000P\000 \000.\000\193\000\027\000\194\000\193\000\144\000\194\000\203\000\020\000\b\000\"\000q\000}\000\b\000\t\001\000\000|\000\128\000\t\000\b\000\185\000\020\001\001\000\232\000\t\000\195\000\201\000\240\000\200\000\196\000\204\000x\000|\000\243\000|\000R\000\027\000\027\000\027\000\196\000\b\000\b\000\196\000\156\000.\000\t\000\t\000\127\000\157\001\003\000|\000\130\000\128\000.\000\246\000\248\0001\0001\000\021\000v\0001\000\168\000^\000\139\000h\000\160\000\131\000[\000\128\000s\000\128\000\021\000\132\000\134\000\170\000\027\000\158\000\136\000\140\000\027\000\149\000\162\000\153\000\166\000\171\000\027\000\128\000\128\000\178\000\185\000\186\001\001\000\187\000\191\000\198\000\216\000\221\000\219\000\223\000\226\000\242\001\b\001\012\000\000\000\000\000\000\000\027\000\027\000\000\000\000\000\000\000]\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\148\000\000\000\000\000\000\000\000\000\000\000\000\000\209\000\211\000\000\000\000\000\214"))
  
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
# 402 "flambda_parser.mly"
    ( { cont; args; trap_action = None } )
# 466 "flambda_parser-in.ml"
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
# 382 "flambda_parser.mly"
     ( let (func, arities) = func in {
          func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind;
          arities;
     } )
# 533 "flambda_parser-in.ml"
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
# 320 "flambda_parser.mly"
        ( Invalid Halt_and_catch_fire )
# 558 "flambda_parser-in.ml"
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
# 321 "flambda_parser.mly"
                ( Invalid Treat_as_unreachable )
# 583 "flambda_parser-in.ml"
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
# 322 "flambda_parser.mly"
                               ( Apply_cont ac )
# 615 "flambda_parser-in.ml"
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
# 323 "flambda_parser.mly"
                                               ( Switch {scrutinee; cases} )
# 654 "flambda_parser-in.ml"
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
# 324 "flambda_parser.mly"
                         ( Apply e )
# 686 "flambda_parser-in.ml"
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
# 325 "flambda_parser.mly"
                             ( e )
# 725 "flambda_parser-in.ml"
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
# 228 "flambda_parser.mly"
    ( Binary (Block_load (Block Value, Immutable), a, f) )
# 778 "flambda_parser-in.ml"
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
# 230 "flambda_parser.mly"
    ( Binary (op, arg1, arg2) )
# 838 "flambda_parser-in.ml"
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
# 232 "flambda_parser.mly"
    ( Binary (Infix op, arg1, arg2) )
# 877 "flambda_parser-in.ml"
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
# 930 "flambda_parser-in.ml"
         in
        
# 239 "flambda_parser.mly"
    ( Variadic (Make_block (t, Immutable), elts) )
# 935 "flambda_parser-in.ml"
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
# 953 "flambda_parser-in.ml"
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
# 978 "flambda_parser-in.ml"
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
# 996 "flambda_parser-in.ml"
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
# 1021 "flambda_parser-in.ml"
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
# 393 "flambda_parser.mly"
    ( Function Indirect )
# 1039 "flambda_parser-in.ml"
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
# 395 "flambda_parser.mly"
    ( Function (Direct { code_id; closure_id }) )
# 1092 "flambda_parser-in.ml"
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
# 397 "flambda_parser.mly"
    ( C_call { alloc = not noalloc } )
# 1124 "flambda_parser-in.ml"
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
# 369 "flambda_parser.mly"
                                                     ( { var; value; } )
# 1163 "flambda_parser-in.ml"
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
# 487 "flambda_parser.mly"
                 ( v )
# 1188 "flambda_parser-in.ml"
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
# 491 "flambda_parser.mly"
    ( None )
# 1206 "flambda_parser-in.ml"
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
# 492 "flambda_parser.mly"
                         ( Some cid )
# 1238 "flambda_parser-in.ml"
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
# 166 "flambda_parser.mly"
    ( let recursive, id, newer_version_of = header in
      { id; newer_version_of; param_arity = None; ret_arity; recursive;
        params_and_body = Present { params; closure_var; ret_cont; exn_cont;
                                    body } } )
# 1322 "flambda_parser-in.ml"
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
# 176 "flambda_parser.mly"
    ( let recursive, id, newer_version_of = header in
      { id; newer_version_of; param_arity = Some param_arity;
        ret_arity = Some ret_arity; recursive; params_and_body = Deleted } )
# 1384 "flambda_parser-in.ml"
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
# 186 "flambda_parser.mly"
    ( recursive, id, newer_version_of )
# 1430 "flambda_parser-in.ml"
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
# 483 "flambda_parser.mly"
                 ( v )
# 1455 "flambda_parser-in.ml"
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
# 70 "flambda_parser.mly"
       (string * char option)
# 1476 "flambda_parser-in.ml"
        ) = Obj.magic c in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_const = 
# 461 "flambda_parser.mly"
            ( make_const_int c )
# 1484 "flambda_parser-in.ml"
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
# 63 "flambda_parser.mly"
       (string * char option)
# 1505 "flambda_parser-in.ml"
        ) = Obj.magic c in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_const = 
# 462 "flambda_parser.mly"
              ( make_const_float c )
# 1513 "flambda_parser-in.ml"
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
# 513 "flambda_parser.mly"
                        ( Named e )
# 1538 "flambda_parser-in.ml"
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
# 514 "flambda_parser.mly"
                             ( Special s )
# 1563 "flambda_parser-in.ml"
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
# 315 "flambda_parser.mly"
                                    ( l )
# 1588 "flambda_parser-in.ml"
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
# 316 "flambda_parser.mly"
                    ( a )
# 1613 "flambda_parser-in.ml"
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
# 416 "flambda_parser.mly"
    ( let is_exn_handler, stub = exn_and_stub in
      { name; params; stub; is_exn_handler; handler } )
# 1667 "flambda_parser-in.ml"
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
# 73 "flambda_parser.mly"
       (string)
# 1688 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_continuation_id = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 509 "flambda_parser.mly"
               ( make_located e (_startpos, _endpos) )
# 1698 "flambda_parser-in.ml"
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
# 406 "flambda_parser.mly"
    ( false, false )
# 1716 "flambda_parser-in.ml"
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
# 407 "flambda_parser.mly"
         ( false, true )
# 1741 "flambda_parser-in.ml"
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
# 408 "flambda_parser.mly"
        ( true, false )
# 1766 "flambda_parser-in.ml"
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
# 409 "flambda_parser.mly"
             ( true, true )
# 1798 "flambda_parser-in.ml"
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
# 410 "flambda_parser.mly"
             ( true, true )
# 1830 "flambda_parser-in.ml"
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
# 139 "flambda_parser.mly"
                             ( cont )
# 1862 "flambda_parser-in.ml"
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
# 142 "flambda_parser.mly"
                                ( cont )
# 1894 "flambda_parser-in.ml"
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
            CamlinternalMenhirLib.EngineTypes.semv = after;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_after_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_after_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _2;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = before;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_before_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_before_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let _4 : unit = Obj.magic _4 in
        let after : 'tv_module_ = Obj.magic after in
        let _2 : unit = Obj.magic _2 in
        let before : 'tv_module_ = Obj.magic before in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_before_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 111 "flambda_parser.mly"
      (Fexpr.expect_test_spec)
# 1940 "flambda_parser-in.ml"
        ) = 
# 126 "flambda_parser.mly"
    ( { before; after } )
# 1944 "flambda_parser-in.ml"
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
# 294 "flambda_parser.mly"
                       ( l )
# 1969 "flambda_parser-in.ml"
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
# 295 "flambda_parser.mly"
                   ( i )
# 1994 "flambda_parser-in.ml"
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
# 110 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 2026 "flambda_parser-in.ml"
        ) = 
# 121 "flambda_parser.mly"
    ( body )
# 2030 "flambda_parser-in.ml"
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
# 375 "flambda_parser.mly"
    ( { code_id; closure_id; is_tupled } )
# 2076 "flambda_parser-in.ml"
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
# 471 "flambda_parser.mly"
             ( n, None )
# 2101 "flambda_parser-in.ml"
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
# 474 "flambda_parser.mly"
    ( n, Some ({ params_arity; ret_arity } : function_arities) )
# 2168 "flambda_parser-in.ml"
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
# 216 "flambda_parser.mly"
         ( Plus )
# 2193 "flambda_parser-in.ml"
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
# 217 "flambda_parser.mly"
            ( Plusdot )
# 2218 "flambda_parser-in.ml"
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
# 218 "flambda_parser.mly"
          ( Minus )
# 2243 "flambda_parser-in.ml"
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
# 219 "flambda_parser.mly"
             ( Minusdot )
# 2268 "flambda_parser-in.ml"
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
# 304 "flambda_parser.mly"
                   ( w )
# 2293 "flambda_parser-in.ml"
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
# 305 "flambda_parser.mly"
                    ( a )
# 2318 "flambda_parser-in.ml"
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
# 113 "flambda_parser.mly"
      (Fexpr.kind)
# 2343 "flambda_parser-in.ml"
        ) = 
# 258 "flambda_parser.mly"
        ( Value )
# 2347 "flambda_parser-in.ml"
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
# 113 "flambda_parser.mly"
      (Fexpr.kind)
# 2372 "flambda_parser-in.ml"
        ) = 
# 259 "flambda_parser.mly"
        ( Naked_number Naked_immediate )
# 2376 "flambda_parser-in.ml"
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
# 113 "flambda_parser.mly"
      (Fexpr.kind)
# 2401 "flambda_parser-in.ml"
        ) = 
# 260 "flambda_parser.mly"
               ( Naked_number Naked_float )
# 2405 "flambda_parser-in.ml"
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
# 113 "flambda_parser.mly"
      (Fexpr.kind)
# 2430 "flambda_parser-in.ml"
        ) = 
# 261 "flambda_parser.mly"
          ( Naked_number Naked_int32 )
# 2434 "flambda_parser-in.ml"
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
# 113 "flambda_parser.mly"
      (Fexpr.kind)
# 2459 "flambda_parser-in.ml"
        ) = 
# 262 "flambda_parser.mly"
          ( Naked_number Naked_int64 )
# 2463 "flambda_parser-in.ml"
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
# 113 "flambda_parser.mly"
      (Fexpr.kind)
# 2488 "flambda_parser-in.ml"
        ) = 
# 263 "flambda_parser.mly"
              ( Naked_number Naked_nativeint )
# 2492 "flambda_parser-in.ml"
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
# 113 "flambda_parser.mly"
      (Fexpr.kind)
# 2517 "flambda_parser-in.ml"
        ) = 
# 264 "flambda_parser.mly"
               ( Fabricated )
# 2521 "flambda_parser-in.ml"
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
# 276 "flambda_parser.mly"
    ( None )
# 2539 "flambda_parser-in.ml"
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
# 113 "flambda_parser.mly"
      (Fexpr.kind)
# 2573 "flambda_parser-in.ml"
        ) = Obj.magic k in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_kind_arg_opt = 
# 277 "flambda_parser.mly"
                             ( Some k )
# 2582 "flambda_parser-in.ml"
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
# 421 "flambda_parser.mly"
                                                                      ( v )
# 2621 "flambda_parser-in.ml"
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
# 422 "flambda_parser.mly"
    ( [] )
# 2639 "flambda_parser-in.ml"
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
# 446 "flambda_parser.mly"
                     ( { param; kind = None } )
# 2664 "flambda_parser-in.ml"
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
# 113 "flambda_parser.mly"
      (Fexpr.kind)
# 2697 "flambda_parser-in.ml"
        ) = Obj.magic kind in
        let _2 : unit = Obj.magic _2 in
        let param : 'tv_variable = Obj.magic param in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_param_ in
        let _endpos = _endpos_kind_ in
        let _v : 'tv_kinded_variable = 
# 447 "flambda_parser.mly"
                                         ( { param; kind = Some kind } )
# 2707 "flambda_parser-in.ml"
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
# 451 "flambda_parser.mly"
                     ( v, None )
# 2732 "flambda_parser-in.ml"
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
# 113 "flambda_parser.mly"
      (Fexpr.kind)
# 2765 "flambda_parser-in.ml"
        ) = Obj.magic kind in
        let _2 : unit = Obj.magic _2 in
        let v : 'tv_variable_opt = Obj.magic v in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_v_ in
        let _endpos = _endpos_kind_ in
        let _v : 'tv_kinded_variable_opt = 
# 452 "flambda_parser.mly"
                                          ( v, Some kind )
# 2775 "flambda_parser-in.ml"
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
# 267 "flambda_parser.mly"
         ( [] )
# 2800 "flambda_parser-in.ml"
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
# 268 "flambda_parser.mly"
                                             ( ks )
# 2825 "flambda_parser-in.ml"
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
# 352 "flambda_parser.mly"
    ( ({ bindings; closure_elements; body } : let_) )
# 2871 "flambda_parser-in.ml"
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
# 352 "flambda_parser.mly"
    ( ({ bindings; closure_elements; body } : let_) )
# 2917 "flambda_parser-in.ml"
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
# 114 "flambda_parser.mly"
      (Fexpr.named)
# 2950 "flambda_parser-in.ml"
        ) = Obj.magic defining_expr in
        let _2 : unit = Obj.magic _2 in
        let v : 'tv_kinded_variable_opt = Obj.magic v in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_v_ in
        let _endpos = _endpos_defining_expr_ in
        let _v : 'tv_let_binding = 
# 357 "flambda_parser.mly"
      ( let (var, kind) = v in { var; kind; defining_expr } )
# 2960 "flambda_parser-in.ml"
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
# 299 "flambda_parser.mly"
                       ( Let l )
# 2992 "flambda_parser-in.ml"
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
# 300 "flambda_parser.mly"
                          ( Let_symbol ls )
# 3017 "flambda_parser-in.ml"
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
# 299 "flambda_parser.mly"
                       ( Let l )
# 3049 "flambda_parser-in.ml"
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
# 300 "flambda_parser.mly"
                          ( Let_symbol ls )
# 3074 "flambda_parser-in.ml"
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
# 148 "flambda_parser.mly"
                     ( { bindings; closure_elements; body } )
# 3127 "flambda_parser-in.ml"
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
# 148 "flambda_parser.mly"
                     ( { bindings; closure_elements; body } )
# 3180 "flambda_parser-in.ml"
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
# 3198 "flambda_parser-in.ml"
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
# 3223 "flambda_parser-in.ml"
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
# 3241 "flambda_parser-in.ml"
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
# 3266 "flambda_parser-in.ml"
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
# 3284 "flambda_parser-in.ml"
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
# 3309 "flambda_parser-in.ml"
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
# 3327 "flambda_parser-in.ml"
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
# 3352 "flambda_parser-in.ml"
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
# 135 "flambda_parser.mly"
    ( { body } )
# 3377 "flambda_parser-in.ml"
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
# 466 "flambda_parser.mly"
               ( (Symbol s:name) )
# 3402 "flambda_parser-in.ml"
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
# 467 "flambda_parser.mly"
                 ( (Var v:name) )
# 3427 "flambda_parser-in.ml"
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
# 114 "flambda_parser.mly"
      (Fexpr.named)
# 3452 "flambda_parser-in.ml"
        ) = 
# 243 "flambda_parser.mly"
               ( Simple s )
# 3456 "flambda_parser-in.ml"
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
# 114 "flambda_parser.mly"
      (Fexpr.named)
# 3488 "flambda_parser-in.ml"
        ) = 
# 244 "flambda_parser.mly"
                        ( Prim (Unary (u, a)) )
# 3492 "flambda_parser-in.ml"
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
# 114 "flambda_parser.mly"
      (Fexpr.named)
# 3517 "flambda_parser-in.ml"
        ) = 
# 245 "flambda_parser.mly"
                  ( Prim b )
# 3521 "flambda_parser-in.ml"
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
# 114 "flambda_parser.mly"
      (Fexpr.named)
# 3546 "flambda_parser-in.ml"
        ) = 
# 246 "flambda_parser.mly"
              ( Prim b )
# 3550 "flambda_parser-in.ml"
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
# 114 "flambda_parser.mly"
      (Fexpr.named)
# 3575 "flambda_parser-in.ml"
        ) = 
# 247 "flambda_parser.mly"
                 ( Closure c )
# 3579 "flambda_parser-in.ml"
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
# 115 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 3604 "flambda_parser-in.ml"
        ) = 
# 440 "flambda_parser.mly"
               ( Symbol s )
# 3608 "flambda_parser-in.ml"
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
# 115 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 3633 "flambda_parser-in.ml"
        ) = 
# 441 "flambda_parser.mly"
                 ( Dynamically_computed v )
# 3637 "flambda_parser-in.ml"
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
# 70 "flambda_parser.mly"
       (string * char option)
# 3658 "flambda_parser-in.ml"
        ) = Obj.magic i in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_i_ in
        let _endpos = _endpos_i_ in
        let _v : (
# 115 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 3666 "flambda_parser-in.ml"
        ) = let _endpos = _endpos_i_ in
        let _startpos = _startpos_i_ in
        
# 442 "flambda_parser.mly"
            ( Tagged_immediate ( make_tagged_immediate ~loc:(_startpos, _endpos) i ) )
# 3672 "flambda_parser-in.ml"
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
# 3690 "flambda_parser-in.ml"
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
        let _v : 'tv_option___anonymous_0_ = 
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
# 185 "flambda_parser.mly"
                                                             ( id )
# 3765 "flambda_parser-in.ml"
         in
        
# 116 "<standard.mly>"
    ( Some x )
# 3770 "flambda_parser-in.ml"
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
# 3788 "flambda_parser-in.ml"
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
# 3813 "flambda_parser-in.ml"
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
# 223 "flambda_parser.mly"
                              ( Phys_equal(k, Eq) )
# 3845 "flambda_parser-in.ml"
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
# 224 "flambda_parser.mly"
                              ( Phys_equal(k, Neq) )
# 3877 "flambda_parser-in.ml"
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
# 202 "flambda_parser.mly"
    ( Nonrecursive )
# 3895 "flambda_parser-in.ml"
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
# 203 "flambda_parser.mly"
        ( Recursive )
# 3920 "flambda_parser-in.ml"
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
# 271 "flambda_parser.mly"
    ( None )
# 3938 "flambda_parser-in.ml"
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
# 272 "flambda_parser.mly"
                    ( Some k )
# 3970 "flambda_parser-in.ml"
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
# 3995 "flambda_parser-in.ml"
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
# 4034 "flambda_parser-in.ml"
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
# 4059 "flambda_parser-in.ml"
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
# 4098 "flambda_parser-in.ml"
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
# 4123 "flambda_parser-in.ml"
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
# 4162 "flambda_parser-in.ml"
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
# 4187 "flambda_parser-in.ml"
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
# 4226 "flambda_parser-in.ml"
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
# 4251 "flambda_parser-in.ml"
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
# 4290 "flambda_parser-in.ml"
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
# 115 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 4311 "flambda_parser-in.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_COMMA_of_kind_value_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4319 "flambda_parser-in.ml"
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
# 115 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 4354 "flambda_parser-in.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_COMMA_of_kind_value_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4362 "flambda_parser-in.ml"
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
# 4387 "flambda_parser-in.ml"
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
# 4426 "flambda_parser-in.ml"
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
# 4451 "flambda_parser-in.ml"
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
# 4490 "flambda_parser-in.ml"
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
# 4515 "flambda_parser-in.ml"
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
# 4554 "flambda_parser-in.ml"
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
      (Fexpr.kind)
# 4575 "flambda_parser-in.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_STAR_kind_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4583 "flambda_parser-in.ml"
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
# 113 "flambda_parser.mly"
      (Fexpr.kind)
# 4618 "flambda_parser-in.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_STAR_kind_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4626 "flambda_parser-in.ml"
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
# 477 "flambda_parser.mly"
               ( Symbol s )
# 4651 "flambda_parser-in.ml"
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
# 478 "flambda_parser.mly"
                 ( Var v )
# 4676 "flambda_parser-in.ml"
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
# 479 "flambda_parser.mly"
              ( Const c )
# 4701 "flambda_parser-in.ml"
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
# 456 "flambda_parser.mly"
    ( [] )
# 4719 "flambda_parser-in.ml"
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
# 457 "flambda_parser.mly"
                                                             ( s )
# 4758 "flambda_parser-in.ml"
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
# 518 "flambda_parser.mly"
         ( Done : special_continuation )
# 4783 "flambda_parser-in.ml"
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
# 519 "flambda_parser.mly"
          ( Error : special_continuation )
# 4808 "flambda_parser-in.ml"
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
# 191 "flambda_parser.mly"
    ( { symbol; fun_decl } )
# 4854 "flambda_parser-in.ml"
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
# 432 "flambda_parser.mly"
    ( (Block { tag; mutability = Immutable; elements } : static_part) )
# 4907 "flambda_parser-in.ml"
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
# 199 "flambda_parser.mly"
    ( { bindings; elements } )
# 4953 "flambda_parser-in.ml"
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
# 112 "flambda_parser.mly"
      (Fexpr.static_structure)
# 4999 "flambda_parser-in.ml"
        ) = 
# 426 "flambda_parser.mly"
    ( { symbol = s; kind = None; defining_expr = sp } )
# 5003 "flambda_parser-in.ml"
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
# 5035 "flambda_parser-in.ml"
         in
        
# 255 "flambda_parser.mly"
                                                         ( cs )
# 5040 "flambda_parser-in.ml"
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
# 251 "flambda_parser.mly"
                                                ( i,ac )
# 5079 "flambda_parser-in.ml"
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
# 99 "flambda_parser.mly"
       (string)
# 5100 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_symbol = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 496 "flambda_parser.mly"
               ( make_located e (_startpos, _endpos) )
# 5110 "flambda_parser-in.ml"
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
# 112 "flambda_parser.mly"
      (Fexpr.static_structure)
# 5131 "flambda_parser-in.ml"
        ) = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : 'tv_symbol_binding = 
# 152 "flambda_parser.mly"
                         ( Block_like s )
# 5139 "flambda_parser-in.ml"
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
# 153 "flambda_parser.mly"
                ( Code code )
# 5164 "flambda_parser-in.ml"
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
# 154 "flambda_parser.mly"
                               ( Closure s )
# 5189 "flambda_parser-in.ml"
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
# 155 "flambda_parser.mly"
                               ( Set_of_closures s )
# 5214 "flambda_parser-in.ml"
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
# 70 "flambda_parser.mly"
       (string * char option)
# 5235 "flambda_parser-in.ml"
        ) = Obj.magic tag in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_tag_ in
        let _endpos = _endpos_tag_ in
        let _v : 'tv_tag = let _endpos = _endpos_tag_ in
        let _startpos = _startpos_tag_ in
        
# 436 "flambda_parser.mly"
            ( make_tag ~loc:(make_loc (_startpos, _endpos)) tag )
# 5245 "flambda_parser-in.ml"
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
# 207 "flambda_parser.mly"
           ( Opaque_identity )
# 5270 "flambda_parser-in.ml"
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
# 208 "flambda_parser.mly"
              ( Untag_imm )
# 5295 "flambda_parser-in.ml"
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
# 210 "flambda_parser.mly"
    ( Project_var { project_from; var } )
# 5341 "flambda_parser-in.ml"
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
# 213 "flambda_parser.mly"
    ( Select_closure { move_from; move_to } )
# 5401 "flambda_parser-in.ml"
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
# 73 "flambda_parser.mly"
       (string)
# 5422 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_var_within_closure = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 523 "flambda_parser.mly"
               ( make_located e (_startpos, _endpos) )
# 5432 "flambda_parser-in.ml"
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
# 73 "flambda_parser.mly"
       (string)
# 5453 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_variable = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 500 "flambda_parser.mly"
               ( make_located e (_startpos, _endpos) )
# 5463 "flambda_parser-in.ml"
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
# 504 "flambda_parser.mly"
               ( None )
# 5488 "flambda_parser-in.ml"
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
# 73 "flambda_parser.mly"
       (string)
# 5509 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_variable_opt = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 505 "flambda_parser.mly"
               ( Some (make_located e (_startpos, _endpos)) )
# 5519 "flambda_parser-in.ml"
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
# 5565 "flambda_parser-in.ml"
         in
        
# 311 "flambda_parser.mly"
     ( Let_cont { recursive; body; handlers } )
# 5570 "flambda_parser-in.ml"
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
# 361 "flambda_parser.mly"
    ( None )
# 5588 "flambda_parser-in.ml"
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
# 5634 "flambda_parser-in.ml"
         in
        
# 365 "flambda_parser.mly"
    ( Some elements )
# 5639 "flambda_parser-in.ml"
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
    (Obj.magic (MenhirInterpreter.entry 279 lexer lexbuf) : (
# 110 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 5670 "flambda_parser-in.ml"
    ))

and expect_test_spec =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 0 lexer lexbuf) : (
# 111 "flambda_parser.mly"
      (Fexpr.expect_test_spec)
# 5678 "flambda_parser-in.ml"
    ))

module Incremental = struct
  
  let flambda_unit =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 279 initial_position) : (
# 110 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 5688 "flambda_parser-in.ml"
      ) MenhirInterpreter.checkpoint)
  
  and expect_test_spec =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 111 "flambda_parser.mly"
      (Fexpr.expect_test_spec)
# 5696 "flambda_parser-in.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 525 "flambda_parser.mly"
  

# 5704 "flambda_parser-in.ml"

# 269 "<standard.mly>"
  

# 5709 "flambda_parser-in.ml"
