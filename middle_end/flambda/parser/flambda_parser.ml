
(* This generated code requires the following version of CamlinternalMenhirLib: *)

let () =
  CamlinternalMenhirLib.StaticVersion.require_20200211

module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WITH
    | VAL
    | UNREACHABLE
    | UNDERSCORE
    | UIDENT of (
# 108 "flambda_parser.mly"
       (string)
# 20 "flambda_parser-in.ml"
  )
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
    | LIDENT of (
# 86 "flambda_parser.mly"
       (string)
# 46 "flambda_parser-in.ml"
  )
    | LETK
    | LET
    | LBRACKET
    | LBRACE
    | LANGLE
    | INT64
    | INT32
    | INT of (
# 80 "flambda_parser.mly"
       (string * char option)
# 58 "flambda_parser-in.ml"
  )
    | IN
    | IMM
    | HCF
    | FLOAT_KIND
    | FLOAT of (
# 73 "flambda_parser.mly"
       (string * char option)
# 67 "flambda_parser-in.ml"
  )
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

# 148 "flambda_parser-in.ml"

module Tables = struct
  
  include MenhirBasics
  
  let token2terminal : token -> int =
    fun _tok ->
      match _tok with
      | AND ->
          55
      | APPLY ->
          54
      | AT ->
          53
      | BLOCK ->
          52
      | CCALL ->
          51
      | CLOSURE ->
          50
      | CODE ->
          49
      | COLON ->
          48
      | CONT ->
          47
      | DOT ->
          46
      | END ->
          45
      | EOF ->
          44
      | EQUAL ->
          43
      | EXN ->
          42
      | FABRICATED ->
          41
      | FLOAT _ ->
          40
      | FLOAT_KIND ->
          39
      | HCF ->
          38
      | IMM ->
          37
      | IN ->
          36
      | INT _ ->
          35
      | INT32 ->
          34
      | INT64 ->
          33
      | LANGLE ->
          32
      | LBRACE ->
          31
      | LBRACKET ->
          30
      | LET ->
          29
      | LETK ->
          28
      | LIDENT _ ->
          27
      | LPAREN ->
          26
      | MINUS ->
          25
      | MINUSDOT ->
          24
      | MINUSGREATER ->
          23
      | NATIVEINT ->
          22
      | NEWER_VERSION_OF ->
          21
      | OPAQUE ->
          20
      | PLUS ->
          19
      | PLUSDOT ->
          18
      | PROJECT_VAR ->
          17
      | RANGLE ->
          16
      | RBRACE ->
          15
      | RBRACKET ->
          14
      | REC ->
          13
      | RPAREN ->
          12
      | SEGMENT ->
          11
      | SEMICOLON ->
          10
      | STAR ->
          9
      | STUB ->
          8
      | SWITCH ->
          7
      | SYMBOL ->
          6
      | UIDENT _ ->
          5
      | UNDERSCORE ->
          4
      | UNREACHABLE ->
          3
      | VAL ->
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
      | CONT ->
          Obj.repr ()
      | DOT ->
          Obj.repr ()
      | END ->
          Obj.repr ()
      | EOF ->
          Obj.repr ()
      | EQUAL ->
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
      | LANGLE ->
          Obj.repr ()
      | LBRACE ->
          Obj.repr ()
      | LBRACKET ->
          Obj.repr ()
      | LET ->
          Obj.repr ()
      | LETK ->
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
      | OPAQUE ->
          Obj.repr ()
      | PLUS ->
          Obj.repr ()
      | PLUSDOT ->
          Obj.repr ()
      | PROJECT_VAR ->
          Obj.repr ()
      | RANGLE ->
          Obj.repr ()
      | RBRACE ->
          Obj.repr ()
      | RBRACKET ->
          Obj.repr ()
      | REC ->
          Obj.repr ()
      | RPAREN ->
          Obj.repr ()
      | SEGMENT ->
          Obj.repr ()
      | SEMICOLON ->
          Obj.repr ()
      | STAR ->
          Obj.repr ()
      | STUB ->
          Obj.repr ()
      | SWITCH ->
          Obj.repr ()
      | SYMBOL ->
          Obj.repr ()
      | UIDENT _v ->
          Obj.repr _v
      | UNDERSCORE ->
          Obj.repr ()
      | UNREACHABLE ->
          Obj.repr ()
      | VAL ->
          Obj.repr ()
      | WITH ->
          Obj.repr ()
  
  and default_reduction =
    (8, "\000\014\001\000\000\022\000\024v\000}\012\rlk\000\000Ry\000\000u\000\000tr\000\026m\000|{z\000Z\000\000\020\000\021\000\000\000\000\000\000&+*)'(,\00021\000-\000?\000\000\000~\000\000\000\000\t\005\000\000\000PON\000A\000qpw\000\000\000\000\000\000\000\000\007T\000\000V\000\000\000\000\000\000\000\0000/\0005\000=\000\000\000\000\000\00078\\\000\000j\000\000\023\000\000\000\000C\000o\025\000\000\016\000\000\000\000\000\000\031\000\000\000\000L\000GF\000\000\000\000 \000I\000#\"%$\000\000\000\004\000J\000\000\030\028MK\bX\011\000\000\000\000\000\006E\000\129\000\000h^\000]\000\000b\n\000\127\000\000\00043\000\000;`\000\000\0009x_\000\000f\000\000d\000\000:\027\000\015\000\000\000\002\000\000\029\000!")
  
  and error =
    (56, "\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\021@H\028\018\129:\000\000\000\016\000\000\000\000\000\000\000\000\000\000\021\000H\028\018\129:\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\016\016\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000!\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000!\000\000\000\000\000\000!\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\132\000\016\000 \000\000\000\000\000\000\000\000\000\128\000\016\000 \000\000\000\000\016\000 \000\000\000\000\000\000\000\000\000\128\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000\000\000!\000\000\000\000\b\0000\000\000\000\000\000\000\016\000\000\000\000\000\000\000\000\000\128 \000\002\000e@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\b\0000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\021\000H\028\018\129:\n\016\0000\000\000@\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000\000\000\000(\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000\000 \000\000\000\004\b\000\016\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\b\000\016\016\000\000\000\000\000\000\000\000\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000@\004\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000\000\000\000 \000\004\000\016\000\000\000\000\000\000\016\000\000\000\000\000\0040\000\000\004\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\0040\000\000\000\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\0000\000\000\000\000\000\000\016\000\000\000\000\000\001\000\128\000\000\000\000\1280\000\000\000\000\000\000\016\000\000\000\000\000\000\000\000\000\128 \000\002\000e@\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\128\000\000\000\000\000\000\000\000\000\000\000\000\000\1280\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\016\000\000\000\000@\000\000\000\016\128\000\000\000\000\000\016\128 \000\002 e@\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000@\001\000\000\016\000 \000\002\000e@\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\021\000H\028\018\129:\000\000\000\000\000\000\000\000\000\000\016\000\000\000@\001\000 \b\012\001\004\b\000\016\016\128\000\004\b\000\016\016\128\000\000\000\000\000\000\000\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\001 \000\000\128\000\000\001\000\000\000\128\000\000\001\000\000\000\000\000\000\000\016\000\000\000\000@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000\000 \000\000\000\004\b\000\016\016\128\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001 \000\000\000\000\000\001\000\000\000\000\000\000\000\016\000\000\000\000@\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\016\016\128\000\000\000\000\000\000\000\000@ 0\192\b\002\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\004\000\000\016\016\128\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\016\016\128\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\021\000H\028\018\129:\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000@\000\000\000\b\004\000\000\000\000\001\000\000\000\000\001\000\016\000\000\000\000\000\000\000\000\016\000\004\000\000\016\016\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000!\000\000\000\000\000\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\000@\000\000\000\b\004\001\002\000\000\000\000\000@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\128 \000\002\000e@\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\000\000\021\000H\028\018\129:\000\000\000\000\000\000\000\000\000\000\000\000\000\000@\000\000\000\b\000\000\000\000\000\000\b\000\000\021\000H\028\018\129:\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\000\001\000\016\000\000\000\000\000\000\000\000\000\000\000\000@\000\000\000\b\000\001\b\000\0000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\004\000H\016\016\128(\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\000\001\000\128\000\016\000 \000\000\000\000\000\b\000\001\000\000\000\000\000\000\000\000\000\000\000\b\000\000\021\000H\028\018\129:\000\000\000\000\000\000\000\000\000\000\000\000\b\000\000\000\000\000\000\000\000")
  
  and start =
    1
  
  and action =
    ((16, "\000\t\000\000\000\000\000\007\000\t\000\000\0004\000\000\000\000\000.\000\000\000\000\000\000\000\000\000\000\000\n\000\n\000\000\000\000\000D\000\t\000\000\000\006\000\n\000\000\000\000\000Z\000\000\000\000\000\148\000\000\000\000\000\000\000\006\000\000\000\134\000\156\000\000\000\176\000\000\000\t\000\184\000\011\000\011\000\186\000\130\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\004\000\000\000\000\001\006\000\000\000\011\000\000\000\228\0004\000\172\000\000\000\014\000\206\000\154\000\011\000\000\000\000\000\n\000\244\000\014\000\000\000\000\000\000\000\014\000\000\001\018\000\000\000\000\000\000\000\178\000\014\000\214\0004\000\006\000\011\000t\000\011\000\000\000\000\000%\000\011\000\000\000T\000\011\000-\000\196\000\148\000\208\000\130\001\026\000\000\000\000\001\020\000\000\000\196\000\000\001\b\000\t\000\t\000\176\000\130\001 \000\000\000\000\000\000\000\188\000\130\000\000\000\228\0004\000\000\000\t\0004\0004\0004\000\000\001$\000\000\000\000\001\004\001\012\000\000\001(\000B\000\170\001\024\000\t\000\t\000\000\000\n\001\020\0004\0012\000\000\000\014\000\000\000\000\000B\001\030\000\t\000\t\000\000\000.\000\000\000\128\000\000\000\000\000\000\000\000\001\026\000.\0018\000\000\000.\000\000\001>\0004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\170\001\022\000\214\001\000\000.\000\000\000\000\001:\000\000\001\002\000\148\000\000\000\000\001\000\000\000\000\142\000\178\000\000\000\000\000\172\000\000\000\252\000\130\001F\000\000\000\000\001\024\0004\000\000\000\000\000\170\001\026\0004\000\000\000\000\000\000\000\192\000\172\000\000\000\184\000\172\000\000\001\014\0004\000\000\000\000\001H\000\000\000\208\000\134\000\208\000\000\001 \0004\000\000\001\018\000\000"), (16, "\001Y\000\213\001Y\001v\001Y\000\018\000\018\000\245\001Q\001Q\001\138\001a\000\"\000^\001Y\000F\000\138\001Y\001\201\000\253\001A\000\174\000*\000\006\001Y\001Y\001Y\001\181\000\"\000\030\001a\000\"\001Y\000&\000*\001Y\000B\001Y\001\005\001Y\000J\001\181\001&\000v\001Y\001a\000\130\001Y\001Y\001Y\000*\001Y\001\250\000*\000\134\000\250\001\181\000R\000.\001\250\000n\000.\001\181\0002\001\238\001\029\0002\000\186\000\170\000\181\001\181\001\181\001\129\001\242\001\029\000\146\001\014\002\018\002:\001I\002N\001\181\002z\002~\001I\001I\002\202\000\190\002\130\002\134\000\254\001\202\001\002\001\137\000A\001N\000\158\001J\000\194\000\198\001\029\000z\000\202\001\222\000\206\000E\000\210\001\129\001i\000\154\002\138\001f\003\018\003\022\000\237\000I\001\129\001\161\000\170\001\029\000\150\001\253\001\r\000\181\001\142\000z\003\006\001\014\001\137\001\026\001\253\001i\001\145\001\198\000z\001Z\001\198\001\161\001Z\002\238\000\t\000\182\000\218\000\230\001\153\000\246\001\n\003f\001\"\001>\001V\003Z\001\150\001\158\001\170\001\186\001\206\001\234\002\n\003\138\002\022\002\026\002\"\002.\002B\002J\002b\002\142\002\150\002\166\002\206\002\214\002\230\002\254\003\030\003&\0032\003F\003r\003\130\003\154\003\167"))
  
  and lhs =
    (8, "\000??>=<;:9887765433333211111111110////.......--,,++**))(('&%$$##\"\"!!  \031\031\030\030\030\030\030\030\029\029\029\028\028\027\027\026\026\025\025\024\024\023\023\022\021\020\020\019\019\018\018\017\017\016\016\015\015\014\014\014\r\r\012\011\n\n\n\t\b\007\007\006\005\005\004\003\002\002\001\001")
  
  and goto =
    ((16, "\000S\000\000\000\000\000+\000\025\000\000\000\005\000\000\000\000\000&\000\000\000\000\000\000\000\000\000\000\000\000\000\176\000\000\000\000\000\000\000\020\000\000\000\000\000\216\000\000\000\000\000\000\000\000\000\000\000\134\000\000\000\000\000\000\000j\000\000\000U\000\000\000\000\000\000\000\000\000d\000\132\000,\000\242\000\000\000\170\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\134\000\000\000\000\000$\000\005\000\000\000.\000\000\000\154\000\n\000\000\000\000\001\020\000\000\0016\000\000\000\000\000\000\001B\000\000\000\000\000\000\000\000\000\000\001\004\001*\000\000\000\154\001B\000*\001d\001V\000\000\000\000\001p\000\146\000\000\001R\001\168\001^\000\236\001\172\000\000\001Z\000\000\000\000\000\000\000\000\000\000\001N\000\000\000\000\001L\000\168\001\140\001D\000\000\000\000\000\000\000\000\000\000\001L\000\000\000\000\000L\000\000\001P\001\164\000H\001\028\000\000\000\000\000\000\000\000\000\000\001X\000\000\000\000\001\168\001\150\000\000\001Z\001d\000\000\001\190\000\000\001<\000\000\000\000\001^\000\000\000\000\001\180\000\000\001d\001n\000\000\000\164\000\000\001v\000\000\000\000\000\000\000\000\000\000\001D\000\000\000\000\001p\000\000\000\000\000t\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\212\000\000\000\005\000\000\001\130\000\000\000\000\000\000\000\000\000\000\001d\000\000\000\000\000\000\000\000\000\000\0004\000\000\000\000\001\212\000\000\000\000\001~\000\000\000\000\000\000\000\000\000\156\000\000\000\000\001\222\000\000\000\196\000\000\000\000\000\000\000\000\001v\000\000\000\000\000\174\000\000\000\000\001\020\000\000\000\000\000\000\000\000\001d\000\005\001f\000\000\000\000\000\236\000\000\000\000\000\000"), (8, "\203\014\181\156\007\204\015\003E)\177\226\158\004\184\207\208\178\212\213\214\014\014\156E8\015\015\169\176\185B\158\016R\171\217\223\177\014\220\014\006\156\015\195\015\233\169)\128\228\158\029\193\171\1969\187\173\174\014F\156\022;\015\224\169\1308 \158\029\029\171E$Y\173\174\014\193\156\196\014\015\175\169\203\015\029\158\029\020\171\157\023\027\173\174\014<\156*\219\015\172\169q;=\158\029\020\171\026\023\025\173\174\014i\156-\217\015\206\169\220_6\158\029\176\171\026\029Q\173\174\014\177\156H\014\015\211\169\178\015\191\158\029j\171\128UK\173\174\014lLK\014\015\232\222L\015i\128\029[\129\165u\149\173\174y\181\150MX\193\014\196OM\146\015\029\189N\014\029\168\173\174\015vm\216]\151\183w\214`lw\029abn\029egpz~\132\136\138\139\141\142\144\\\152\154\155\167\190\199\201\187\029\209\230\229\000\000\000\000\000\029"))
  
  and semantic_action =
    [|
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = t;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_t_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_t_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = h;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_h_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_h_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = _1;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let t : 'tv_andk = Obj.magic t in
        let h : 'tv_continuation_handler = Obj.magic h in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_t_ in
        let _v : 'tv_andk = 
# 327 "flambda_parser.mly"
                                          ( h :: t )
# 435 "flambda_parser-in.ml"
         in
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
        let _v : 'tv_andk = 
# 328 "flambda_parser.mly"
    ( [] )
# 453 "flambda_parser-in.ml"
         in
        {
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
        let _v : 'tv_binop = 
# 206 "flambda_parser.mly"
    ( Binop (Block_load (Block Value, Immutable), a, f) )
# 506 "flambda_parser-in.ml"
         in
        {
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
          CamlinternalMenhirLib.EngineTypes.semv = code_id;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_code_id_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_code_id_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let code_id : 'tv_code_id = Obj.magic code_id in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_code_id_ in
        let _v : 'tv_closure = 
# 308 "flambda_parser.mly"
                                ( code_id )
# 538 "flambda_parser-in.ml"
         in
        {
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
# 304 "flambda_parser.mly"
                                                     ( { var; value; } )
# 577 "flambda_parser-in.ml"
         in
        {
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
# 397 "flambda_parser.mly"
                 ( v )
# 602 "flambda_parser-in.ml"
         in
        {
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
          CamlinternalMenhirLib.EngineTypes.semv = expr;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_expr_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_expr_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _13;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__13_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__13_;
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
                    CamlinternalMenhirLib.EngineTypes.semv = _9;
                    CamlinternalMenhirLib.EngineTypes.startp = _startpos__9_;
                    CamlinternalMenhirLib.EngineTypes.endp = _endpos__9_;
                    CamlinternalMenhirLib.EngineTypes.next = {
                      CamlinternalMenhirLib.EngineTypes.state = _;
                      CamlinternalMenhirLib.EngineTypes.semv = vars_within_closures;
                      CamlinternalMenhirLib.EngineTypes.startp = _startpos_vars_within_closures_;
                      CamlinternalMenhirLib.EngineTypes.endp = _endpos_vars_within_closures_;
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
                            CamlinternalMenhirLib.EngineTypes.state = _;
                            CamlinternalMenhirLib.EngineTypes.semv = newer_version_of;
                            CamlinternalMenhirLib.EngineTypes.startp = _startpos_newer_version_of_;
                            CamlinternalMenhirLib.EngineTypes.endp = _endpos_newer_version_of_;
                            CamlinternalMenhirLib.EngineTypes.next = {
                              CamlinternalMenhirLib.EngineTypes.state = _;
                              CamlinternalMenhirLib.EngineTypes.semv = closure_id;
                              CamlinternalMenhirLib.EngineTypes.startp = _startpos_closure_id_;
                              CamlinternalMenhirLib.EngineTypes.endp = _endpos_closure_id_;
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
                            };
                          };
                        };
                      };
                    };
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let expr : (
# 118 "flambda_parser.mly"
      (Fexpr.expr)
# 701 "flambda_parser-in.ml"
        ) = Obj.magic expr in
        let _13 : unit = Obj.magic _13 in
        let ret_arity : 'tv_return_arity = Obj.magic ret_arity in
        let exn_cont : 'tv_option_exn_continuation_ = Obj.magic exn_cont in
        let ret_cont : 'tv_continuation = Obj.magic ret_cont in
        let _9 : unit = Obj.magic _9 in
        let vars_within_closures : 'tv_kinded_vars_within_closures = Obj.magic vars_within_closures in
        let closure_var : 'tv_variable = Obj.magic closure_var in
        let params : 'tv_kinded_args = Obj.magic params in
        let newer_version_of : 'tv_option___anonymous_1_ = Obj.magic newer_version_of in
        let closure_id : 'tv_option___anonymous_0_ = Obj.magic closure_id in
        let id : 'tv_code_id = Obj.magic id in
        let recursive : 'tv_recursive = Obj.magic recursive in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_expr_ in
        let _v : 'tv_code_binding = 
# 179 "flambda_parser.mly"
    ( { id; closure_id; newer_version_of; params; closure_var;
        vars_within_closures; ret_cont; ret_arity; exn_cont; recursive; expr; }
       : code_binding )
# 724 "flambda_parser-in.ml"
         in
        {
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
# 393 "flambda_parser.mly"
                 ( v )
# 749 "flambda_parser-in.ml"
         in
        {
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
          CamlinternalMenhirLib.EngineTypes.semv = code_binding;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_code_binding_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_code_binding_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let code_binding : 'tv_code_binding = Obj.magic code_binding in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_code_binding_ in
        let _endpos = _endpos_code_binding_ in
        let _v : 'tv_code_or_closure_binding = 
# 162 "flambda_parser.mly"
                                ( `Code code_binding )
# 774 "flambda_parser-in.ml"
         in
        {
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
          CamlinternalMenhirLib.EngineTypes.semv = closure_binding;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_closure_binding_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_closure_binding_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let closure_binding : 'tv_static_closure_binding = Obj.magic closure_binding in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_closure_binding_ in
        let _endpos = _endpos_closure_binding_ in
        let _v : 'tv_code_or_closure_binding = 
# 163 "flambda_parser.mly"
                                             ( `Closure closure_binding )
# 799 "flambda_parser-in.ml"
         in
        {
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
# 80 "flambda_parser.mly"
       (string * char option)
# 820 "flambda_parser-in.ml"
        ) = Obj.magic c in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_const = 
# 377 "flambda_parser.mly"
            ( make_const_int c )
# 828 "flambda_parser-in.ml"
         in
        {
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
# 73 "flambda_parser.mly"
       (string * char option)
# 849 "flambda_parser-in.ml"
        ) = Obj.magic c in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_const = 
# 378 "flambda_parser.mly"
              ( make_const_float c )
# 857 "flambda_parser-in.ml"
         in
        {
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
# 86 "flambda_parser.mly"
       (string)
# 878 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_continuation = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 418 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 888 "flambda_parser-in.ml"
         in
        {
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
          };
        } = _menhir_stack in
        let _6 : unit = Obj.magic _6 in
        let handler : (
# 118 "flambda_parser.mly"
      (Fexpr.expr)
# 940 "flambda_parser-in.ml"
        ) = Obj.magic handler in
        let _4 : unit = Obj.magic _4 in
        let params : 'tv_kinded_args = Obj.magic params in
        let name : 'tv_continuation = Obj.magic name in
        let exn_and_stub : 'tv_exn_and_stub = Obj.magic exn_and_stub in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_exn_and_stub_ in
        let _endpos = _endpos__6_ in
        let _v : 'tv_continuation_handler = 
# 322 "flambda_parser.mly"
    ( let is_exn_handler, stub = exn_and_stub in
      { name; params; stub; is_exn_handler; handler } )
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
          CamlinternalMenhirLib.EngineTypes.semv = s;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let s : (
# 86 "flambda_parser.mly"
       (string)
# 974 "flambda_parser-in.ml"
        ) = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : 'tv_csymbol = let _endpos = _endpos_s_ in
        let _startpos = _startpos_s_ in
        
# 405 "flambda_parser.mly"
               ( s, make_loc (_startpos, _endpos) )
# 984 "flambda_parser-in.ml"
         in
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
# 312 "flambda_parser.mly"
    ( false, false )
# 1002 "flambda_parser-in.ml"
         in
        {
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
# 313 "flambda_parser.mly"
         ( false, true )
# 1027 "flambda_parser-in.ml"
         in
        {
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
# 314 "flambda_parser.mly"
        ( true, false )
# 1052 "flambda_parser-in.ml"
         in
        {
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
# 315 "flambda_parser.mly"
             ( true, true )
# 1084 "flambda_parser-in.ml"
         in
        {
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
# 316 "flambda_parser.mly"
             ( true, true )
# 1116 "flambda_parser-in.ml"
         in
        {
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
# 134 "flambda_parser.mly"
                             ( cont )
# 1148 "flambda_parser-in.ml"
         in
        {
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
# 118 "flambda_parser.mly"
      (Fexpr.expr)
# 1173 "flambda_parser-in.ml"
        ) = 
# 245 "flambda_parser.mly"
        ( Invalid Halt_and_catch_fire )
# 1177 "flambda_parser-in.ml"
         in
        {
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
# 118 "flambda_parser.mly"
      (Fexpr.expr)
# 1202 "flambda_parser-in.ml"
        ) = 
# 246 "flambda_parser.mly"
                ( Invalid Treat_as_unreachable )
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
          CamlinternalMenhirLib.EngineTypes.semv = s;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = c;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_c_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_c_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = _1;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let s : 'tv_simple_args = Obj.magic s in
        let c : 'tv_continuation = Obj.magic c in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_s_ in
        let _v : (
# 118 "flambda_parser.mly"
      (Fexpr.expr)
# 1245 "flambda_parser-in.ml"
        ) = 
# 247 "flambda_parser.mly"
                                          ( Apply_cont (c, None, s) )
# 1249 "flambda_parser-in.ml"
         in
        {
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
            CamlinternalMenhirLib.EngineTypes.semv = cases;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_cases_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_cases_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _3;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
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
            };
          };
        } = _menhir_stack in
        let _5 : unit = Obj.magic _5 in
        let cases : 'tv_switch = Obj.magic cases in
        let _3 : unit = Obj.magic _3 in
        let scrutinee : 'tv_simple = Obj.magic scrutinee in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : (
# 118 "flambda_parser.mly"
      (Fexpr.expr)
# 1302 "flambda_parser-in.ml"
        ) = 
# 249 "flambda_parser.mly"
    ( Switch {scrutinee; cases} )
# 1306 "flambda_parser-in.ml"
         in
        {
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
        let l : 'tv_let_ = Obj.magic l in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_l_ in
        let _v : (
# 118 "flambda_parser.mly"
      (Fexpr.expr)
# 1338 "flambda_parser-in.ml"
        ) = 
# 250 "flambda_parser.mly"
                 ( Let l )
# 1342 "flambda_parser-in.ml"
         in
        {
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
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = defining_expr;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_defining_expr_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_defining_expr_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let body : (
# 118 "flambda_parser.mly"
      (Fexpr.expr)
# 1375 "flambda_parser-in.ml"
        ) = Obj.magic body in
        let _2 : unit = Obj.magic _2 in
        let defining_expr : (
# 121 "flambda_parser.mly"
      (Fexpr.named)
# 1381 "flambda_parser-in.ml"
        ) = Obj.magic defining_expr in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_defining_expr_ in
        let _endpos = _endpos_body_ in
        let _v : (
# 118 "flambda_parser.mly"
      (Fexpr.expr)
# 1389 "flambda_parser-in.ml"
        ) = 
# 252 "flambda_parser.mly"
      ( Let { bindings = [ { var = None; kind = None; defining_expr; } ];
              closure_elements = None; 
              body } )
# 1395 "flambda_parser-in.ml"
         in
        {
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
            CamlinternalMenhirLib.EngineTypes.semv = _5;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__5_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__5_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = t;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_t_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_t_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = handler;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_handler_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_handler_;
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
            };
          };
        } = _menhir_stack in
        let body : (
# 118 "flambda_parser.mly"
      (Fexpr.expr)
# 1446 "flambda_parser-in.ml"
        ) = Obj.magic body in
        let _5 : unit = Obj.magic _5 in
        let t : 'tv_andk = Obj.magic t in
        let handler : 'tv_continuation_handler = Obj.magic handler in
        let recursive : 'tv_recursive = Obj.magic recursive in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_body_ in
        let _v : (
# 118 "flambda_parser.mly"
      (Fexpr.expr)
# 1459 "flambda_parser-in.ml"
        ) = 
# 256 "flambda_parser.mly"
     ( let handlers = handler :: t in
       Let_cont { recursive; body; handlers } )
# 1464 "flambda_parser-in.ml"
         in
        {
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
        let ls : 'tv_let_symbol = Obj.magic ls in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_ls_ in
        let _endpos = _endpos_ls_ in
        let _v : (
# 118 "flambda_parser.mly"
      (Fexpr.expr)
# 1489 "flambda_parser-in.ml"
        ) = 
# 258 "flambda_parser.mly"
                     ( Let_symbol ls )
# 1493 "flambda_parser-in.ml"
         in
        {
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
              CamlinternalMenhirLib.EngineTypes.semv = _7;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__7_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__7_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = ra;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_ra_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_ra_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _;
                  CamlinternalMenhirLib.EngineTypes.semv = args;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos_args_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos_args_;
                  CamlinternalMenhirLib.EngineTypes.next = {
                    CamlinternalMenhirLib.EngineTypes.state = _;
                    CamlinternalMenhirLib.EngineTypes.semv = _4;
                    CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
                    CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
                    CamlinternalMenhirLib.EngineTypes.next = {
                      CamlinternalMenhirLib.EngineTypes.state = _;
                      CamlinternalMenhirLib.EngineTypes.semv = func;
                      CamlinternalMenhirLib.EngineTypes.startp = _startpos_func_;
                      CamlinternalMenhirLib.EngineTypes.endp = _endpos_func_;
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
              };
            };
          };
        } = _menhir_stack in
        let e : 'tv_exn_continuation = Obj.magic e in
        let r : 'tv_continuation = Obj.magic r in
        let _7 : unit = Obj.magic _7 in
        let ra : 'tv_return_arity = Obj.magic ra in
        let args : 'tv_simple_args = Obj.magic args in
        let _4 : unit = Obj.magic _4 in
        let func : 'tv_csymbol = Obj.magic func in
        let _2 : unit = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_e_ in
        let _v : (
# 118 "flambda_parser.mly"
      (Fexpr.expr)
# 1574 "flambda_parser-in.ml"
        ) = 
# 261 "flambda_parser.mly"
     ( Apply {
          func = Symbol func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind = C_call {
              alloc = true; (* TODO noalloc *)
              (* param_arity =; *)
              return_arity = ra;
            };
       })
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
        let e : 'tv_exn_continuation = Obj.magic e in
        let r : 'tv_continuation = Obj.magic r in
        let _4 : unit = Obj.magic _4 in
        let args : 'tv_simple_args = Obj.magic args in
        let func : 'tv_name = Obj.magic func in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_e_ in
        let _v : (
# 118 "flambda_parser.mly"
      (Fexpr.expr)
# 1648 "flambda_parser-in.ml"
        ) = 
# 274 "flambda_parser.mly"
     ( Apply {
          func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind = Function Indirect_unknown_arity;
       })
# 1658 "flambda_parser-in.ml"
         in
        {
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
            CamlinternalMenhirLib.EngineTypes.semv = body;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_body_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_body_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = exception_cont;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_exception_cont_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_exception_cont_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = return_cont;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_return_cont_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_return_cont_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let _4 : unit = Obj.magic _4 in
        let body : (
# 118 "flambda_parser.mly"
      (Fexpr.expr)
# 1698 "flambda_parser-in.ml"
        ) = Obj.magic body in
        let exception_cont : 'tv_option_exn_continuation_ = Obj.magic exception_cont in
        let return_cont : 'tv_continuation = Obj.magic return_cont in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_return_cont_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 116 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 1708 "flambda_parser-in.ml"
        ) = 
# 130 "flambda_parser.mly"
    ( { return_cont; exception_cont; body } )
# 1712 "flambda_parser-in.ml"
         in
        {
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
# 198 "flambda_parser.mly"
         ( Plus )
# 1737 "flambda_parser-in.ml"
         in
        {
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
# 199 "flambda_parser.mly"
            ( Plusdot )
# 1762 "flambda_parser-in.ml"
         in
        {
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
# 200 "flambda_parser.mly"
          ( Minus )
# 1787 "flambda_parser-in.ml"
         in
        {
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
# 201 "flambda_parser.mly"
             ( Minusdot )
# 1812 "flambda_parser-in.ml"
         in
        {
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
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 1837 "flambda_parser-in.ml"
        ) = 
# 227 "flambda_parser.mly"
        ( Value )
# 1841 "flambda_parser-in.ml"
         in
        {
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
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 1866 "flambda_parser-in.ml"
        ) = 
# 228 "flambda_parser.mly"
        ( Naked_number Naked_immediate )
# 1870 "flambda_parser-in.ml"
         in
        {
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
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 1895 "flambda_parser-in.ml"
        ) = 
# 229 "flambda_parser.mly"
               ( Naked_number Naked_float )
# 1899 "flambda_parser-in.ml"
         in
        {
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
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 1924 "flambda_parser-in.ml"
        ) = 
# 230 "flambda_parser.mly"
          ( Naked_number Naked_int32 )
# 1928 "flambda_parser-in.ml"
         in
        {
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
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 1953 "flambda_parser-in.ml"
        ) = 
# 231 "flambda_parser.mly"
          ( Naked_number Naked_int64 )
# 1957 "flambda_parser-in.ml"
         in
        {
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
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 1982 "flambda_parser-in.ml"
        ) = 
# 232 "flambda_parser.mly"
              ( Naked_number Naked_nativeint )
# 1986 "flambda_parser-in.ml"
         in
        {
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
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 2011 "flambda_parser-in.ml"
        ) = 
# 233 "flambda_parser.mly"
               ( Fabricated )
# 2015 "flambda_parser-in.ml"
         in
        {
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
        let v : 'tv_list_kinded_variable_ = Obj.magic v in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_kinded_args = 
# 331 "flambda_parser.mly"
                                       ( v )
# 2054 "flambda_parser-in.ml"
         in
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
# 332 "flambda_parser.mly"
    ( [] )
# 2072 "flambda_parser-in.ml"
         in
        {
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
          CamlinternalMenhirLib.EngineTypes.semv = var;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_var_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_var_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let var : 'tv_var_within_closure = Obj.magic var in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_var_ in
        let _endpos = _endpos_var_ in
        let _v : 'tv_kinded_var_within_closure = 
# 366 "flambda_parser.mly"
                             ( { var; kind = None } )
# 2097 "flambda_parser-in.ml"
         in
        {
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
            CamlinternalMenhirLib.EngineTypes.semv = kind;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_kind_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_kind_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _3;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = var;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_var_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_var_;
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
        let kind : (
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 2143 "flambda_parser-in.ml"
        ) = Obj.magic kind in
        let _3 : unit = Obj.magic _3 in
        let var : 'tv_var_within_closure = Obj.magic var in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : 'tv_kinded_var_within_closure = 
# 368 "flambda_parser.mly"
    ( { var; kind = Some kind } )
# 2154 "flambda_parser-in.ml"
         in
        {
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
# 354 "flambda_parser.mly"
                     ( { param; kind = None } )
# 2179 "flambda_parser-in.ml"
         in
        {
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
            CamlinternalMenhirLib.EngineTypes.semv = kind;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_kind_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_kind_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _3;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = param;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_param_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_param_;
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
        let kind : (
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 2225 "flambda_parser-in.ml"
        ) = Obj.magic kind in
        let _3 : unit = Obj.magic _3 in
        let param : 'tv_variable = Obj.magic param in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : 'tv_kinded_variable = 
# 356 "flambda_parser.mly"
    ( { param; kind = Some kind } )
# 2236 "flambda_parser-in.ml"
         in
        {
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
# 360 "flambda_parser.mly"
                     ( v, None )
# 2261 "flambda_parser-in.ml"
         in
        {
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
            CamlinternalMenhirLib.EngineTypes.semv = kind;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_kind_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_kind_;
            CamlinternalMenhirLib.EngineTypes.next = {
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
            };
          };
        } = _menhir_stack in
        let _5 : unit = Obj.magic _5 in
        let kind : (
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 2307 "flambda_parser-in.ml"
        ) = Obj.magic kind in
        let _3 : unit = Obj.magic _3 in
        let v : 'tv_variable_opt = Obj.magic v in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : 'tv_kinded_variable_opt = 
# 362 "flambda_parser.mly"
    ( v, Some kind )
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
        let v : 'tv_list_kinded_var_within_closure_ = Obj.magic v in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_kinded_vars_within_closures = 
# 335 "flambda_parser.mly"
                                                 ( v )
# 2357 "flambda_parser-in.ml"
         in
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
        let _v : 'tv_kinded_vars_within_closures = 
# 336 "flambda_parser.mly"
    ( [] )
# 2375 "flambda_parser-in.ml"
         in
        {
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
        let _v : 'tv_kinds = 
# 236 "flambda_parser.mly"
                  ( [] )
# 2407 "flambda_parser-in.ml"
         in
        {
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
# 237 "flambda_parser.mly"
                                             ( ks )
# 2432 "flambda_parser-in.ml"
         in
        {
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
        let body : (
# 118 "flambda_parser.mly"
      (Fexpr.expr)
# 2471 "flambda_parser-in.ml"
        ) = Obj.magic body in
        let _3 : unit = Obj.magic _3 in
        let closure_elements : 'tv_with_closure_elements_opt = Obj.magic closure_elements in
        let bindings : 'tv_separated_nonempty_list_AND_let_binding_ = Obj.magic bindings in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_bindings_ in
        let _endpos = _endpos_body_ in
        let _v : 'tv_let_ = 
# 287 "flambda_parser.mly"
    ( { bindings; closure_elements; body } )
# 2482 "flambda_parser-in.ml"
         in
        {
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
# 121 "flambda_parser.mly"
      (Fexpr.named)
# 2515 "flambda_parser-in.ml"
        ) = Obj.magic defining_expr in
        let _2 : unit = Obj.magic _2 in
        let v : 'tv_kinded_variable_opt = Obj.magic v in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_v_ in
        let _endpos = _endpos_defining_expr_ in
        let _v : 'tv_let_binding = 
# 292 "flambda_parser.mly"
      ( let (var, kind) = v in { var; kind; defining_expr } )
# 2525 "flambda_parser-in.ml"
         in
        {
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
        let body : (
# 118 "flambda_parser.mly"
      (Fexpr.expr)
# 2564 "flambda_parser-in.ml"
        ) = Obj.magic body in
        let _3 : unit = Obj.magic _3 in
        let bindings : 'tv_symbol_bindings = Obj.magic bindings in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_body_ in
        let _v : 'tv_let_symbol = 
# 138 "flambda_parser.mly"
                                                      ( { bindings; body } )
# 2575 "flambda_parser-in.ml"
         in
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
        let _v : 'tv_list_kinded_var_within_closure_ = 
# 211 "<standard.mly>"
    ( [] )
# 2593 "flambda_parser-in.ml"
         in
        {
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
            CamlinternalMenhirLib.EngineTypes.semv = x;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let xs : 'tv_list_kinded_var_within_closure_ = Obj.magic xs in
        let x : 'tv_kinded_var_within_closure = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_kinded_var_within_closure_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2625 "flambda_parser-in.ml"
         in
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
        let _v : 'tv_list_kinded_variable_ = 
# 211 "<standard.mly>"
    ( [] )
# 2643 "flambda_parser-in.ml"
         in
        {
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
            CamlinternalMenhirLib.EngineTypes.semv = x;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let xs : 'tv_list_kinded_variable_ = Obj.magic xs in
        let x : 'tv_kinded_variable = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_kinded_variable_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2675 "flambda_parser-in.ml"
         in
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
        let _v : 'tv_list_of_kind_value_ = 
# 211 "<standard.mly>"
    ( [] )
# 2693 "flambda_parser-in.ml"
         in
        {
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
            CamlinternalMenhirLib.EngineTypes.semv = x;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let xs : 'tv_list_of_kind_value_ = Obj.magic xs in
        let x : (
# 122 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 2721 "flambda_parser-in.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_of_kind_value_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2729 "flambda_parser-in.ml"
         in
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
        let _v : 'tv_list_simple_ = 
# 211 "<standard.mly>"
    ( [] )
# 2747 "flambda_parser-in.ml"
         in
        {
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
            CamlinternalMenhirLib.EngineTypes.semv = x;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let xs : 'tv_list_simple_ = Obj.magic xs in
        let x : 'tv_simple = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_simple_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2779 "flambda_parser-in.ml"
         in
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
# 2797 "flambda_parser-in.ml"
         in
        {
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
# 2822 "flambda_parser-in.ml"
         in
        {
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
# 382 "flambda_parser.mly"
               ( (Symbol s:name) )
# 2847 "flambda_parser-in.ml"
         in
        {
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
# 383 "flambda_parser.mly"
                 ( (Var v:name) )
# 2872 "flambda_parser-in.ml"
         in
        {
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
# 121 "flambda_parser.mly"
      (Fexpr.named)
# 2897 "flambda_parser-in.ml"
        ) = 
# 209 "flambda_parser.mly"
               ( Simple s )
# 2901 "flambda_parser-in.ml"
         in
        {
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
# 121 "flambda_parser.mly"
      (Fexpr.named)
# 2933 "flambda_parser-in.ml"
        ) = 
# 210 "flambda_parser.mly"
                        ( Prim (Unop (u, a)) )
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
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = a2;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_a2_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_a2_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = b;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_b_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_b_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = a1;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_a1_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_a1_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let a2 : 'tv_simple = Obj.magic a2 in
        let b : 'tv_infix_binop = Obj.magic b in
        let a1 : 'tv_simple = Obj.magic a1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_a1_ in
        let _endpos = _endpos_a2_ in
        let _v : (
# 121 "flambda_parser.mly"
      (Fexpr.named)
# 2976 "flambda_parser-in.ml"
        ) = 
# 211 "flambda_parser.mly"
                                            ( Prim (Infix_binop (b, a1, a2)) )
# 2980 "flambda_parser-in.ml"
         in
        {
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
        let b : 'tv_binop = Obj.magic b in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_b_ in
        let _endpos = _endpos_b_ in
        let _v : (
# 121 "flambda_parser.mly"
      (Fexpr.named)
# 3005 "flambda_parser-in.ml"
        ) = 
# 212 "flambda_parser.mly"
              ( Prim b )
# 3009 "flambda_parser-in.ml"
         in
        {
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
            CamlinternalMenhirLib.EngineTypes.semv = elts;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_elts_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_elts_;
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
        let elts : 'tv_list_simple_ = Obj.magic elts in
        let _3 : unit = Obj.magic _3 in
        let t : 'tv_tag = Obj.magic t in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : (
# 121 "flambda_parser.mly"
      (Fexpr.named)
# 3062 "flambda_parser-in.ml"
        ) = 
# 213 "flambda_parser.mly"
                                               ( Prim (Block (t, Immutable, elts)) )
# 3066 "flambda_parser-in.ml"
         in
        {
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
        let c : 'tv_closure = Obj.magic c in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : (
# 121 "flambda_parser.mly"
      (Fexpr.named)
# 3091 "flambda_parser-in.ml"
        ) = 
# 214 "flambda_parser.mly"
                ( Closure { code_id = c } )
# 3095 "flambda_parser-in.ml"
         in
        {
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
# 122 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 3120 "flambda_parser-in.ml"
        ) = 
# 348 "flambda_parser.mly"
               ( Symbol s )
# 3124 "flambda_parser-in.ml"
         in
        {
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
# 122 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 3149 "flambda_parser-in.ml"
        ) = 
# 349 "flambda_parser.mly"
                 ( Dynamically_computed v )
# 3153 "flambda_parser-in.ml"
         in
        {
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
# 80 "flambda_parser.mly"
       (string * char option)
# 3174 "flambda_parser-in.ml"
        ) = Obj.magic i in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_i_ in
        let _endpos = _endpos_i_ in
        let _v : (
# 122 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 3182 "flambda_parser-in.ml"
        ) = let _endpos = _endpos_i_ in
        let _startpos = _startpos_i_ in
        
# 350 "flambda_parser.mly"
            ( Tagged_immediate ( make_tagged_immediate ~loc:(_startpos, _endpos) i ) )
# 3188 "flambda_parser-in.ml"
         in
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
        let _v : 'tv_option_SEMICOLON_ = 
# 114 "<standard.mly>"
    ( None )
# 3206 "flambda_parser-in.ml"
         in
        {
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
        let _v : 'tv_option_SEMICOLON_ = 
# 116 "<standard.mly>"
    ( Some x )
# 3231 "flambda_parser-in.ml"
         in
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
# 3249 "flambda_parser-in.ml"
         in
        {
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
        let _v : 'tv_option___anonymous_0_ = let x = 
# 170 "flambda_parser.mly"
                                             ( cid )
# 3281 "flambda_parser-in.ml"
         in
        
# 116 "<standard.mly>"
    ( Some x )
# 3286 "flambda_parser-in.ml"
         in
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
        let _v : 'tv_option___anonymous_1_ = 
# 114 "<standard.mly>"
    ( None )
# 3304 "flambda_parser-in.ml"
         in
        {
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
        let _v : 'tv_option___anonymous_1_ = let x = 
# 171 "flambda_parser.mly"
                                                             ( id )
# 3336 "flambda_parser-in.ml"
         in
        
# 116 "<standard.mly>"
    ( Some x )
# 3341 "flambda_parser-in.ml"
         in
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
        let _v : 'tv_option_exn_continuation_ = 
# 114 "<standard.mly>"
    ( None )
# 3359 "flambda_parser-in.ml"
         in
        {
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
        let x : 'tv_exn_continuation = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_option_exn_continuation_ = 
# 116 "<standard.mly>"
    ( Some x )
# 3384 "flambda_parser-in.ml"
         in
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
# 189 "flambda_parser.mly"
    ( Nonrecursive )
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
# 190 "flambda_parser.mly"
        ( Recursive )
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
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_return_arity = 
# 240 "flambda_parser.mly"
    ( None )
# 3445 "flambda_parser-in.ml"
         in
        {
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
# 241 "flambda_parser.mly"
                    ( Some k )
# 3477 "flambda_parser-in.ml"
         in
        {
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
            CamlinternalMenhirLib.EngineTypes.semv = seg;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_seg_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_seg_;
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
        let seg : 'tv_segment_body = Obj.magic seg in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_segment = 
# 152 "flambda_parser.mly"
                                     ( seg )
# 3516 "flambda_parser-in.ml"
         in
        {
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
        } = _menhir_stack in
        let closure_elements : 'tv_with_closure_elements_opt = Obj.magic closure_elements in
        let bindings : 'tv_separated_nonempty_list_AND_code_or_closure_binding_ = Obj.magic bindings in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_bindings_ in
        let _endpos = _endpos_closure_elements_ in
        let _v : 'tv_segment_body = 
# 158 "flambda_parser.mly"
    ( make_segment bindings closure_elements )
# 3548 "flambda_parser-in.ml"
         in
        {
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
          CamlinternalMenhirLib.EngineTypes.semv = seg;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_seg_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_seg_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let seg : 'tv_segment_body = Obj.magic seg in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_seg_ in
        let _endpos = _endpos_seg_ in
        let _v : 'tv_segments = 
# 147 "flambda_parser.mly"
                       ( [ seg ] )
# 3573 "flambda_parser-in.ml"
         in
        {
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
          CamlinternalMenhirLib.EngineTypes.semv = segs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_segs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_segs_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let segs : 'tv_separated_nonempty_list_AND_segment_ = Obj.magic segs in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_segs_ in
        let _endpos = _endpos_segs_ in
        let _v : 'tv_segments = 
# 148 "flambda_parser.mly"
                                                 ( segs )
# 3598 "flambda_parser-in.ml"
         in
        {
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
        let x : 'tv_code_or_closure_binding = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_AND_code_or_closure_binding_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3623 "flambda_parser-in.ml"
         in
        {
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
        let xs : 'tv_separated_nonempty_list_AND_code_or_closure_binding_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_code_or_closure_binding = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_AND_code_or_closure_binding_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3662 "flambda_parser-in.ml"
         in
        {
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
# 3687 "flambda_parser-in.ml"
         in
        {
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
# 3726 "flambda_parser-in.ml"
         in
        {
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
        let x : 'tv_segment = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_AND_segment_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3751 "flambda_parser-in.ml"
         in
        {
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
        let xs : 'tv_separated_nonempty_list_AND_segment_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_segment = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_AND_segment_ = 
# 243 "<standard.mly>"
    ( x :: xs )
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
# 3815 "flambda_parser-in.ml"
         in
        {
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
# 3854 "flambda_parser-in.ml"
         in
        {
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
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 3875 "flambda_parser-in.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_STAR_kind_ = 
# 241 "<standard.mly>"
    ( [ x ] )
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
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 3918 "flambda_parser-in.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_STAR_kind_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3926 "flambda_parser-in.ml"
         in
        {
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
# 387 "flambda_parser.mly"
               ( Symbol s )
# 3951 "flambda_parser-in.ml"
         in
        {
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
# 388 "flambda_parser.mly"
                 ( Var v )
# 3976 "flambda_parser-in.ml"
         in
        {
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
# 389 "flambda_parser.mly"
              ( Const c )
# 4001 "flambda_parser-in.ml"
         in
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
# 372 "flambda_parser.mly"
    ( [] )
# 4019 "flambda_parser-in.ml"
         in
        {
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
        let s : 'tv_list_simple_ = Obj.magic s in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_simple_args = 
# 373 "flambda_parser.mly"
                              ( s )
# 4058 "flambda_parser-in.ml"
         in
        {
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
          CamlinternalMenhirLib.EngineTypes.semv = code_id;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_code_id_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_code_id_;
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
        let code_id : 'tv_closure = Obj.magic code_id in
        let _3 : unit = Obj.magic _3 in
        let symbol : 'tv_symbol = Obj.magic symbol in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_code_id_ in
        let _v : 'tv_static_closure_binding = 
# 185 "flambda_parser.mly"
                                                       ( { symbol; code_id } )
# 4104 "flambda_parser-in.ml"
         in
        {
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
            CamlinternalMenhirLib.EngineTypes.semv = elts;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_elts_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_elts_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _5;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__5_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__5_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = t;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_t_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_t_;
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
                      CamlinternalMenhirLib.EngineTypes.semv = s;
                      CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
                      CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
                      CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                    };
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let _7 : unit = Obj.magic _7 in
        let elts : 'tv_list_of_kind_value_ = Obj.magic elts in
        let _5 : unit = Obj.magic _5 in
        let t : 'tv_tag = Obj.magic t in
        let _3 : unit = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let s : 'tv_symbol = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos__7_ in
        let _v : (
# 117 "flambda_parser.mly"
      (Fexpr.static_structure)
# 4171 "flambda_parser-in.ml"
        ) = 
# 340 "flambda_parser.mly"
    ( ( s, None, Block (t, Immutable, elts) ) )
# 4175 "flambda_parser-in.ml"
         in
        {
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
        let _1 : 'tv_option_SEMICOLON_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_switch = 
# 222 "flambda_parser.mly"
                      ( [] )
# 4200 "flambda_parser-in.ml"
         in
        {
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
        let c : 'tv_switch_case = Obj.magic c in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_switch = 
# 223 "flambda_parser.mly"
                    ( [c] )
# 4225 "flambda_parser-in.ml"
         in
        {
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
          CamlinternalMenhirLib.EngineTypes.semv = t;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_t_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_t_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = h;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_h_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_h_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let t : 'tv_switch = Obj.magic t in
        let _2 : unit = Obj.magic _2 in
        let h : 'tv_switch_case = Obj.magic h in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_h_ in
        let _endpos = _endpos_t_ in
        let _v : 'tv_switch = 
# 224 "flambda_parser.mly"
                                         ( h :: t )
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
          CamlinternalMenhirLib.EngineTypes.semv = c;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_c_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_c_;
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
        let c : 'tv_continuation = Obj.magic c in
        let _2 : unit = Obj.magic _2 in
        let i : 'tv_tag = Obj.magic i in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_i_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_switch_case = 
# 218 "flambda_parser.mly"
                                          ( i,c )
# 4303 "flambda_parser-in.ml"
         in
        {
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
# 108 "flambda_parser.mly"
       (string)
# 4324 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_symbol = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 401 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 4334 "flambda_parser-in.ml"
         in
        {
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
        } = _menhir_stack in
        let s : (
# 117 "flambda_parser.mly"
      (Fexpr.static_structure)
# 4361 "flambda_parser-in.ml"
        ) = Obj.magic s in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_s_ in
        let _v : 'tv_symbol_bindings = 
# 142 "flambda_parser.mly"
                                ( Simple s )
# 4370 "flambda_parser-in.ml"
         in
        {
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
          CamlinternalMenhirLib.EngineTypes.semv = segs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_segs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_segs_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let segs : 'tv_segments = Obj.magic segs in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_segs_ in
        let _endpos = _endpos_segs_ in
        let _v : 'tv_symbol_bindings = 
# 143 "flambda_parser.mly"
                    ( Segments segs )
# 4395 "flambda_parser-in.ml"
         in
        {
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
# 80 "flambda_parser.mly"
       (string * char option)
# 4416 "flambda_parser-in.ml"
        ) = Obj.magic tag in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_tag_ in
        let _endpos = _endpos_tag_ in
        let _v : 'tv_tag = let _endpos = _endpos_tag_ in
        let _startpos = _startpos_tag_ in
        
# 344 "flambda_parser.mly"
            ( make_tag ~loc:(make_loc (_startpos, _endpos)) tag )
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
# 194 "flambda_parser.mly"
           ( Opaque_identity )
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
          CamlinternalMenhirLib.EngineTypes.semv = var;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_var_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_var_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let var : 'tv_var_within_closure = Obj.magic var in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_var_ in
        let _v : 'tv_unop = 
# 195 "flambda_parser.mly"
                                           ( Project_var var )
# 4483 "flambda_parser-in.ml"
         in
        {
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
# 86 "flambda_parser.mly"
       (string)
# 4504 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_var_within_closure = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 422 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 4514 "flambda_parser-in.ml"
         in
        {
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
# 86 "flambda_parser.mly"
       (string)
# 4535 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_variable = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 409 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 4545 "flambda_parser-in.ml"
         in
        {
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
# 413 "flambda_parser.mly"
               ( None )
# 4570 "flambda_parser-in.ml"
         in
        {
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
# 86 "flambda_parser.mly"
       (string)
# 4591 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_variable_opt = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 414 "flambda_parser.mly"
               ( Some (e, make_loc (_startpos, _endpos)) )
# 4601 "flambda_parser-in.ml"
         in
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
# 296 "flambda_parser.mly"
    ( None )
# 4619 "flambda_parser-in.ml"
         in
        {
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
# 4665 "flambda_parser-in.ml"
         in
        
# 300 "flambda_parser.mly"
    ( Some elements )
# 4670 "flambda_parser-in.ml"
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
# 116 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 4701 "flambda_parser-in.ml"
    ))

module Incremental = struct
  
  let flambda_unit =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 116 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 4711 "flambda_parser-in.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 424 "flambda_parser.mly"
  

# 4719 "flambda_parser-in.ml"

# 269 "<standard.mly>"
  

# 4724 "flambda_parser-in.ml"
