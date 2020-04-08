
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WITH
    | UNREACHABLE
    | UNDERSCORE
    | UIDENT of (
# 104 "flambda_parser.mly"
       (string)
# 14 "flambda_parser.ml"
  )
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
    | LIDENT of (
# 82 "flambda_parser.mly"
       (string)
# 40 "flambda_parser.ml"
  )
    | LETK
    | LET
    | LBRACKET
    | LBRACE
    | LANGLE
    | IS_INT
    | INT of (
# 76 "flambda_parser.mly"
       (string * char option)
# 51 "flambda_parser.ml"
  )
    | IN
    | HCF
    | FLOAT of (
# 72 "flambda_parser.mly"
       (string * char option)
# 58 "flambda_parser.ml"
  )
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
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState217
  | MenhirState215
  | MenhirState214
  | MenhirState213
  | MenhirState209
  | MenhirState205
  | MenhirState202
  | MenhirState198
  | MenhirState196
  | MenhirState194
  | MenhirState191
  | MenhirState184
  | MenhirState180
  | MenhirState172
  | MenhirState169
  | MenhirState166
  | MenhirState158
  | MenhirState156
  | MenhirState155
  | MenhirState153
  | MenhirState150
  | MenhirState147
  | MenhirState145
  | MenhirState143
  | MenhirState142
  | MenhirState140
  | MenhirState139
  | MenhirState130
  | MenhirState129
  | MenhirState128
  | MenhirState127
  | MenhirState125
  | MenhirState122
  | MenhirState119
  | MenhirState118
  | MenhirState117
  | MenhirState116
  | MenhirState113
  | MenhirState109
  | MenhirState107
  | MenhirState106
  | MenhirState104
  | MenhirState100
  | MenhirState98
  | MenhirState97
  | MenhirState96
  | MenhirState94
  | MenhirState92
  | MenhirState86
  | MenhirState84
  | MenhirState82
  | MenhirState76
  | MenhirState72
  | MenhirState70
  | MenhirState67
  | MenhirState66
  | MenhirState64
  | MenhirState62
  | MenhirState61
  | MenhirState58
  | MenhirState54
  | MenhirState53
  | MenhirState52
  | MenhirState47
  | MenhirState45
  | MenhirState41
  | MenhirState35
  | MenhirState32
  | MenhirState28
  | MenhirState21
  | MenhirState18
  | MenhirState11
  | MenhirState6
  | MenhirState4
  | MenhirState3
  | MenhirState0

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

# 223 "flambda_parser.ml"

let rec _menhir_goto_segments : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_segments -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv895) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_segments) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv893) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((segs : 'tv_segments) : 'tv_segments) = _v in
    ((let _v : 'tv_symbol_bindings = 
# 137 "flambda_parser.mly"
                    ( Segments segs )
# 238 "flambda_parser.ml"
     in
    _menhir_goto_symbol_bindings _menhir_env _menhir_stack _menhir_s _v) : 'freshtv894)) : 'freshtv896)

and _menhir_goto_segment_and_more_segments : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_segment_and_more_segments -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState180 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv883 * _menhir_state * 'tv_with_closure_elements_opt)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_segment_and_more_segments) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv881 * _menhir_state * 'tv_with_closure_elements_opt)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((more : 'tv_segment_and_more_segments) : 'tv_segment_and_more_segments) = _v in
        ((let (_menhir_stack, _menhir_s, (elts : 'tv_with_closure_elements_opt)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_segment_and_more_segments = 
# 187 "flambda_parser.mly"
    ( let more_bindings, more_elts, segs = more in
      (* Careful! [elts] goes with the *previous* segment, not the one we're
       * building now. *)
      let seg = make_segment more_bindings more_elts in
      [], elts, seg :: segs )
# 263 "flambda_parser.ml"
         in
        _menhir_goto_segment_and_more_segments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv882)) : 'freshtv884)
    | MenhirState184 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv887 * _menhir_state * 'tv_code_or_closure_binding)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_segment_and_more_segments) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv885 * _menhir_state * 'tv_code_or_closure_binding)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((more : 'tv_segment_and_more_segments) : 'tv_segment_and_more_segments) = _v in
        ((let (_menhir_stack, _menhir_s, (binding : 'tv_code_or_closure_binding)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_segment_and_more_segments = 
# 185 "flambda_parser.mly"
    ( let bindings, elts, segs = more in binding :: bindings, elts, segs )
# 280 "flambda_parser.ml"
         in
        _menhir_goto_segment_and_more_segments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv886)) : 'freshtv888)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv891 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_segment_and_more_segments) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv889 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((seg_and_segs : 'tv_segment_and_more_segments) : 'tv_segment_and_more_segments) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_segments = 
# 143 "flambda_parser.mly"
    ( let bindings, elts, segs = seg_and_segs in
      let seg = make_segment bindings elts in
      seg :: segs )
# 299 "flambda_parser.ml"
         in
        _menhir_goto_segments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv890)) : 'freshtv892)
    | _ ->
        _menhir_fail ()

and _menhir_goto_symbol_bindings : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_symbol_bindings -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv879 * _menhir_state) * _menhir_state * 'tv_symbol_bindings) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv875 * _menhir_state) * _menhir_state * 'tv_symbol_bindings) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | APPLY ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | BLOCK ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | CCALL ->
            _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | CLOSURE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | CONT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | FLOAT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v
        | HCF ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | INT _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState191 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | LETK ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | LIDENT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState191 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPAQUE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | PROJECT_VAR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | SWITCH ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | UIDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState191 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNREACHABLE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191) : 'freshtv876)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv877 * _menhir_state) * _menhir_state * 'tv_symbol_bindings) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv878)) : 'freshtv880)

and _menhir_goto_kinds : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_kinds -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState119 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv869 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_kinds) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv867 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((k : 'tv_kinds) : 'tv_kinds) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_return_arity = 
# 287 "flambda_parser.mly"
                    ( Some k )
# 380 "flambda_parser.ml"
         in
        _menhir_goto_return_arity _menhir_env _menhir_stack _menhir_s _v) : 'freshtv868)) : 'freshtv870)
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv873 * _menhir_state * 'tv_kind)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_kinds) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv871 * _menhir_state * 'tv_kind)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((t : 'tv_kinds) : 'tv_kinds) = _v in
        ((let (_menhir_stack, _menhir_s, (k : 'tv_kind)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_kinds = 
# 283 "flambda_parser.mly"
                             ( k :: t )
# 397 "flambda_parser.ml"
         in
        _menhir_goto_kinds _menhir_env _menhir_stack _menhir_s _v) : 'freshtv872)) : 'freshtv874)
    | _ ->
        _menhir_fail ()

and _menhir_goto_tags_to_sizes : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_tags_to_sizes -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv861)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv857)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv855)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, (tags_to_sizes : 'tv_tags_to_sizes)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_switch_sort = 
# 240 "flambda_parser.mly"
                                                        ( Tag { tags_to_sizes } )
# 426 "flambda_parser.ml"
             in
            _menhir_goto_switch_sort _menhir_env _menhir_stack _v) : 'freshtv856)) : 'freshtv858)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv859)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv860)) : 'freshtv862)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv865 * _menhir_state * 'tv_tag_to_size)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv863 * _menhir_state * 'tv_tag_to_size)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (tag_to_size : 'tv_tag_to_size)), _, (tags_to_sizes : 'tv_tags_to_sizes)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_tags_to_sizes = 
# 237 "flambda_parser.mly"
                                                                  ( tag_to_size :: tags_to_sizes )
# 446 "flambda_parser.ml"
         in
        _menhir_goto_tags_to_sizes _menhir_env _menhir_stack _menhir_s _v) : 'freshtv864)) : 'freshtv866)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_kinded_var_within_closure_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_kinded_var_within_closure_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv849) * _menhir_state * 'tv_list_kinded_var_within_closure_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RANGLE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv845) * _menhir_state * 'tv_list_kinded_var_within_closure_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv843) * _menhir_state * 'tv_list_kinded_var_within_closure_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, (v : 'tv_list_kinded_var_within_closure_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_kinded_vars_within_closures = 
# 381 "flambda_parser.mly"
                                                 ( v )
# 474 "flambda_parser.ml"
             in
            _menhir_goto_kinded_vars_within_closures _menhir_env _menhir_stack _v) : 'freshtv844)) : 'freshtv846)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv847) * _menhir_state * 'tv_list_kinded_var_within_closure_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv848)) : 'freshtv850)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv853 * _menhir_state * 'tv_kinded_var_within_closure) * _menhir_state * 'tv_list_kinded_var_within_closure_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv851 * _menhir_state * 'tv_kinded_var_within_closure) * _menhir_state * 'tv_list_kinded_var_within_closure_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_kinded_var_within_closure)), _, (xs : 'tv_list_kinded_var_within_closure_)) = _menhir_stack in
        let _v : 'tv_list_kinded_var_within_closure_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 493 "flambda_parser.ml"
         in
        _menhir_goto_list_kinded_var_within_closure_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv852)) : 'freshtv854)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_AND_let_binding_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_AND_let_binding_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv837 * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | WITH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | IN ->
            _menhir_reduce123 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState194) : 'freshtv838)
    | MenhirState202 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv841 * _menhir_state * 'tv_let_binding)) * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv839 * _menhir_state * 'tv_let_binding)) * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_let_binding)), _, (xs : 'tv_separated_nonempty_list_AND_let_binding_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_AND_let_binding_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 527 "flambda_parser.ml"
         in
        _menhir_goto_separated_nonempty_list_AND_let_binding_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv840)) : 'freshtv842)
    | _ ->
        _menhir_fail ()

and _menhir_goto_infix_binop : _menhir_env -> 'ttv_tail -> 'tv_infix_binop -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv835 * _menhir_state * 'tv_simple) * 'tv_infix_binop) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
    | INT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState169 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState169 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState169 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169) : 'freshtv836)

and _menhir_goto_separated_nonempty_list_SEMICOLON_closure_element_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_closure_element_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv829) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_closure_element_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv827) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_closure_element_) : 'tv_separated_nonempty_list_SEMICOLON_closure_element_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__ = 
# 144 "<standard.mly>"
    ( x )
# 569 "flambda_parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_closure_element__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv828)) : 'freshtv830)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv833 * _menhir_state * 'tv_closure_element)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_closure_element_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv831 * _menhir_state * 'tv_closure_element)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_closure_element_) : 'tv_separated_nonempty_list_SEMICOLON_closure_element_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_closure_element)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_closure_element_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 586 "flambda_parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_closure_element_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv832)) : 'freshtv834)
    | _ ->
        _menhir_fail ()

and _menhir_goto_recursive : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_recursive -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv823 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EXN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | STUB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LIDENT _ ->
            _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47) : 'freshtv824)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv825 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98) : 'freshtv826)
    | _ ->
        _menhir_fail ()

and _menhir_goto_variable_opt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_variable_opt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv821) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_variable_opt) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv819) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((v : 'tv_variable_opt) : 'tv_variable_opt) = _v in
    ((let _v : 'tv_kinded_variable_opt = 
# 404 "flambda_parser.mly"
                     ( v, None )
# 640 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv817) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_kinded_variable_opt) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv815 * _menhir_state * 'tv_kinded_variable_opt) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv811 * _menhir_state * 'tv_kinded_variable_opt) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BLOCK ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | CLOSURE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | FLOAT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
        | INT _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState205 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LIDENT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState205 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPAQUE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | PROJECT_VAR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | UIDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState205 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205) : 'freshtv812)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv813 * _menhir_state * 'tv_kinded_variable_opt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv814)) : 'freshtv816)) : 'freshtv818)) : 'freshtv820)) : 'freshtv822)

and _menhir_goto_exn_and_stub : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_exn_and_stub -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv809 * _menhir_state * 'tv_exn_and_stub) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv810)

and _menhir_goto_with_closure_elements_opt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_with_closure_elements_opt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState184 | MenhirState180 | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv791 * _menhir_state * 'tv_with_closure_elements_opt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEGMENT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv785 * _menhir_state * 'tv_with_closure_elements_opt) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CODE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState180
            | SYMBOL ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState180
            | WITH ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState180
            | IN | SEGMENT ->
                _menhir_reduce123 _menhir_env (Obj.magic _menhir_stack) MenhirState180
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180) : 'freshtv786)
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv787 * _menhir_state * 'tv_with_closure_elements_opt) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (elts : 'tv_with_closure_elements_opt)) = _menhir_stack in
            let _v : 'tv_segment_and_more_segments = 
# 192 "flambda_parser.mly"
                                      ( [], elts, [] )
# 736 "flambda_parser.ml"
             in
            _menhir_goto_segment_and_more_segments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv788)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv789 * _menhir_state * 'tv_with_closure_elements_opt) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv790)) : 'freshtv792)
    | MenhirState194 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv797 * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) * _menhir_state * 'tv_with_closure_elements_opt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv793 * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) * _menhir_state * 'tv_with_closure_elements_opt) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APPLY ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | BLOCK ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | CCALL ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | CLOSURE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | CONT ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | FLOAT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _v
            | HCF ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | INT _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState196 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | LETK ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | LIDENT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState196 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | PROJECT_VAR ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | SWITCH ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | UIDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState196 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState196) : 'freshtv794)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv795 * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) * _menhir_state * 'tv_with_closure_elements_opt) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv796)) : 'freshtv798)
    | MenhirState198 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv807 * _menhir_state * 'tv_separated_nonempty_list_AND_code_or_closure_binding_) * _menhir_state * 'tv_with_closure_elements_opt) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv805 * _menhir_state * 'tv_separated_nonempty_list_AND_code_or_closure_binding_) * _menhir_state * 'tv_with_closure_elements_opt) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (bindings : 'tv_separated_nonempty_list_AND_code_or_closure_binding_)), _, (closure_elements : 'tv_with_closure_elements_opt)) = _menhir_stack in
        let _v : 'tv_single_segment = 
# 151 "flambda_parser.mly"
    ( make_segment bindings closure_elements )
# 810 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv803) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_single_segment) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv801) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_single_segment) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv799) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((seg : 'tv_single_segment) : 'tv_single_segment) = _v in
        ((let _v : 'tv_segments = 
# 141 "flambda_parser.mly"
                         ( [ seg ] )
# 827 "flambda_parser.ml"
         in
        _menhir_goto_segments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv800)) : 'freshtv802)) : 'freshtv804)) : 'freshtv806)) : 'freshtv808)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_closure_element__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv783 * _menhir_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv779 * _menhir_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv777 * _menhir_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__)) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_with_closure_elements_opt = let elements = 
# 232 "<standard.mly>"
    ( xs )
# 854 "flambda_parser.ml"
         in
        
# 346 "flambda_parser.mly"
    ( Some elements )
# 859 "flambda_parser.ml"
         in
        _menhir_goto_with_closure_elements_opt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv778)) : 'freshtv780)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv781 * _menhir_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv782)) : 'freshtv784)

and _menhir_goto_list_kinded_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_kinded_variable_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv771 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv767 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv765 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (v : 'tv_list_kinded_variable_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_kinded_args = 
# 377 "flambda_parser.mly"
                                       ( v )
# 892 "flambda_parser.ml"
             in
            _menhir_goto_kinded_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv766)) : 'freshtv768)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv769 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv770)) : 'freshtv772)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv775 * _menhir_state * 'tv_kinded_variable) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv773 * _menhir_state * 'tv_kinded_variable) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_kinded_variable)), _, (xs : 'tv_list_kinded_variable_)) = _menhir_stack in
        let _v : 'tv_list_kinded_variable_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 911 "flambda_parser.ml"
         in
        _menhir_goto_list_kinded_variable_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv774)) : 'freshtv776)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_simple_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_simple_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState130 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv747 * _menhir_state * 'tv_simple) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv745 * _menhir_state * 'tv_simple) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_simple)), _, (xs : 'tv_list_simple_)) = _menhir_stack in
        let _v : 'tv_list_simple_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 930 "flambda_parser.ml"
         in
        _menhir_goto_list_simple_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv746)) : 'freshtv748)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv755 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv751 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv749 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (s : 'tv_list_simple_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_simple_args = 
# 413 "flambda_parser.mly"
                              ( s )
# 951 "flambda_parser.ml"
             in
            _menhir_goto_simple_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv750)) : 'freshtv752)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv753 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv754)) : 'freshtv756)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv763 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv759 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv757 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (t : 'tv_tag)), _, (elts : 'tv_list_simple_)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (
# 115 "flambda_parser.mly"
      (Fexpr.named)
# 980 "flambda_parser.ml"
            ) = 
# 265 "flambda_parser.mly"
                                               ( Prim (Block (t, Immutable, elts)) )
# 984 "flambda_parser.ml"
             in
            _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv758)) : 'freshtv760)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv761 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv762)) : 'freshtv764)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_of_kind_value_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_of_kind_value_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv729 * _menhir_state * (
# 116 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 1006 "flambda_parser.ml"
        )) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv727 * _menhir_state * (
# 116 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 1012 "flambda_parser.ml"
        )) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 116 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 1017 "flambda_parser.ml"
        ))), _, (xs : 'tv_list_of_kind_value_)) = _menhir_stack in
        let _v : 'tv_list_of_kind_value_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1022 "flambda_parser.ml"
         in
        _menhir_goto_list_of_kind_value_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv728)) : 'freshtv730)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv743 * _menhir_state * 'tv_symbol)) * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv739 * _menhir_state * 'tv_symbol)) * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv737 * _menhir_state * 'tv_symbol)) * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s, (s : 'tv_symbol)), _), _, (t : 'tv_tag)), _, (elts : 'tv_list_of_kind_value_)) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _v : (
# 112 "flambda_parser.mly"
      (Fexpr.static_structure)
# 1045 "flambda_parser.ml"
            ) = 
# 386 "flambda_parser.mly"
    ( ( s, None, Block (t, Immutable, elts) ) )
# 1049 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv735) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 112 "flambda_parser.mly"
      (Fexpr.static_structure)
# 1057 "flambda_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv733 * _menhir_state) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 112 "flambda_parser.mly"
      (Fexpr.static_structure)
# 1065 "flambda_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv731 * _menhir_state) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((s : (
# 112 "flambda_parser.mly"
      (Fexpr.static_structure)
# 1073 "flambda_parser.ml"
            )) : (
# 112 "flambda_parser.mly"
      (Fexpr.static_structure)
# 1077 "flambda_parser.ml"
            )) = _v in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_symbol_bindings = 
# 136 "flambda_parser.mly"
                                ( Simple s )
# 1084 "flambda_parser.ml"
             in
            _menhir_goto_symbol_bindings _menhir_env _menhir_stack _menhir_s _v) : 'freshtv732)) : 'freshtv734)) : 'freshtv736)) : 'freshtv738)) : 'freshtv740)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv741 * _menhir_state * 'tv_symbol)) * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv742)) : 'freshtv744)
    | _ ->
        _menhir_fail ()

and _menhir_goto_return_arity : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_return_arity -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((((('freshtv719 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((((('freshtv715 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APPLY ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | BLOCK ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | CCALL ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | CLOSURE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | CONT ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | FLOAT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
            | HCF ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | INT _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState125 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | LETK ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | LIDENT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState125 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | PROJECT_VAR ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | SWITCH ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | UIDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState125 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125) : 'freshtv716)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((((('freshtv717 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv718)) : 'freshtv720)
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv725 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv721 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142) : 'freshtv722)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv723 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv724)) : 'freshtv726)
    | _ ->
        _menhir_fail ()

and _menhir_reduce37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_kind = 
# 279 "flambda_parser.mly"
    ( None )
# 1189 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv713) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_kind) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv711 * _menhir_state * 'tv_kind) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv707 * _menhir_state * 'tv_kind) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL | MINUSGREATER ->
            _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | COMMA ->
            _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122) : 'freshtv708)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv709 * _menhir_state * 'tv_kind) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv710)) : 'freshtv712)) : 'freshtv714)

and _menhir_reduce45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_kinds = 
# 282 "flambda_parser.mly"
    ( [] )
# 1228 "flambda_parser.ml"
     in
    _menhir_goto_kinds _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce114 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_tags_to_sizes = 
# 235 "flambda_parser.mly"
    ( [] )
# 1237 "flambda_parser.ml"
     in
    _menhir_goto_tags_to_sizes _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 76 "flambda_parser.mly"
       (string * char option)
# 1244 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv703 * Lexing.position * _menhir_state * (
# 76 "flambda_parser.mly"
       (string * char option)
# 1256 "flambda_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv699 * Lexing.position * _menhir_state * (
# 76 "flambda_parser.mly"
       (string * char option)
# 1266 "flambda_parser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 76 "flambda_parser.mly"
       (string * char option)
# 1272 "flambda_parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv697 * Lexing.position * _menhir_state * (
# 76 "flambda_parser.mly"
       (string * char option)
# 1280 "flambda_parser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            let (_endpos_size_ : Lexing.position) = _endpos in
            let ((size : (
# 76 "flambda_parser.mly"
       (string * char option)
# 1286 "flambda_parser.ml"
            )) : (
# 76 "flambda_parser.mly"
       (string * char option)
# 1290 "flambda_parser.ml"
            )) = _v in
            let (_startpos_size_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _endpos_tag_, _menhir_s, (tag : (
# 76 "flambda_parser.mly"
       (string * char option)
# 1296 "flambda_parser.ml"
            )), _startpos_tag_) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_tag_to_size = 
# 228 "flambda_parser.mly"
                               (
  let (tag, _) = tag in
  let (size, _) = size in
  int_of_string tag, int_of_string size
)
# 1306 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv695) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_tag_to_size) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv693 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COMMA ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv687 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | INT _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RBRACKET ->
                    _menhir_reduce114 _menhir_env (Obj.magic _menhir_stack) MenhirState18
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18) : 'freshtv688)
            | RBRACKET ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv689 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (tag_to_size : 'tv_tag_to_size)) = _menhir_stack in
                let _v : 'tv_tags_to_sizes = 
# 236 "flambda_parser.mly"
                              ( [ tag_to_size ] )
# 1339 "flambda_parser.ml"
                 in
                _menhir_goto_tags_to_sizes _menhir_env _menhir_stack _menhir_s _v) : 'freshtv690)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv691 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv692)) : 'freshtv694)) : 'freshtv696)) : 'freshtv698)) : 'freshtv700)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv701 * Lexing.position * _menhir_state * (
# 76 "flambda_parser.mly"
       (string * char option)
# 1356 "flambda_parser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv702)) : 'freshtv704)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv705 * Lexing.position * _menhir_state * (
# 76 "flambda_parser.mly"
       (string * char option)
# 1367 "flambda_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv706)

and _menhir_goto_switch_sort : _menhir_env -> 'ttv_tail -> 'tv_switch_sort -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv685 * _menhir_state) * 'tv_switch_sort) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | INT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv686)

and _menhir_goto_unop : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_unop -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv683 * _menhir_state * 'tv_unop) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
    | INT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState158 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState158 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState158 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158) : 'freshtv684)

and _menhir_goto_name : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_name -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv681 * _menhir_state) * _menhir_state * 'tv_name) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | MINUSGREATER ->
        _menhir_reduce97 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153) : 'freshtv682)

and _menhir_goto_kinded_vars_within_closures : _menhir_env -> 'ttv_tail -> 'tv_kinded_vars_within_closures -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((((((('freshtv679 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | MINUSGREATER ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv675 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116) : 'freshtv676)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv677 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv678)) : 'freshtv680)

and _menhir_reduce50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_kinded_var_within_closure_ = 
# 211 "<standard.mly>"
    ( [] )
# 1464 "flambda_parser.ml"
     in
    _menhir_goto_list_kinded_var_within_closure_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run42 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 82 "flambda_parser.mly"
       (string)
# 1471 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv673) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 82 "flambda_parser.mly"
       (string)
# 1482 "flambda_parser.ml"
    )) : (
# 82 "flambda_parser.mly"
       (string)
# 1486 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_var_within_closure = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 462 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 1494 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv671) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_var_within_closure) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv655 * _menhir_state) * _menhir_state * 'tv_var_within_closure) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv653 * _menhir_state) * _menhir_state * 'tv_var_within_closure) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (var : 'tv_var_within_closure)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_unop = 
# 247 "flambda_parser.mly"
                                           ( Project_var var )
# 1512 "flambda_parser.ml"
         in
        _menhir_goto_unop _menhir_env _menhir_stack _menhir_s _v) : 'freshtv654)) : 'freshtv656)
    | MenhirState92 | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv661 * _menhir_state * 'tv_var_within_closure) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv657 * _menhir_state * 'tv_var_within_closure) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FLOAT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
            | INT _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIDENT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86) : 'freshtv658)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv659 * _menhir_state * 'tv_var_within_closure) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv660)) : 'freshtv662)
    | MenhirState113 | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv669 * _menhir_state * 'tv_var_within_closure) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv667 * _menhir_state * 'tv_var_within_closure) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (var : 'tv_var_within_closure)) = _menhir_stack in
        let _v : 'tv_kinded_var_within_closure = 
# 408 "flambda_parser.mly"
                             ( { var; kind = None } )
# 1555 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv665) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_kinded_var_within_closure) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv663 * _menhir_state * 'tv_kinded_var_within_closure) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RANGLE ->
            _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113) : 'freshtv664)) : 'freshtv666)) : 'freshtv668)) : 'freshtv670)
    | _ ->
        _menhir_fail ()) : 'freshtv672)) : 'freshtv674)

and _menhir_goto_of_kind_value : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 116 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 1581 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv651 * _menhir_state * (
# 116 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 1589 "flambda_parser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv652)

and _menhir_goto_option___anonymous_1_ : _menhir_env -> 'ttv_tail -> 'tv_option___anonymous_1_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((('freshtv649 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | LIDENT _ ->
        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106) : 'freshtv650)

and _menhir_goto_option___anonymous_0_ : _menhir_env -> 'ttv_tail -> 'tv_option___anonymous_0_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv647 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NEWER_VERSION_OF ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv641) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState104 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104) : 'freshtv642)
    | LIDENT _ | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv643) = Obj.magic _menhir_stack in
        ((let _v : 'tv_option___anonymous_1_ = 
# 114 "<standard.mly>"
    ( None )
# 1650 "flambda_parser.ml"
         in
        _menhir_goto_option___anonymous_1_ _menhir_env _menhir_stack _v) : 'freshtv644)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv645 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv646)) : 'freshtv648)

and _menhir_goto_named : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 115 "flambda_parser.mly"
      (Fexpr.named)
# 1664 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 | MenhirState217 | MenhirState61 | MenhirState196 | MenhirState191 | MenhirState172 | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv625 * _menhir_state * (
# 115 "flambda_parser.mly"
      (Fexpr.named)
# 1674 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv621 * _menhir_state * (
# 115 "flambda_parser.mly"
      (Fexpr.named)
# 1684 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APPLY ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | BLOCK ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | CCALL ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | CLOSURE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | CONT ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | FLOAT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
            | HCF ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | INT _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | LETK ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | LIDENT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | PROJECT_VAR ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | SWITCH ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | UIDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172) : 'freshtv622)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv623 * _menhir_state * (
# 115 "flambda_parser.mly"
      (Fexpr.named)
# 1732 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv624)) : 'freshtv626)
    | MenhirState205 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv639 * _menhir_state * 'tv_kinded_variable_opt)) * _menhir_state * (
# 115 "flambda_parser.mly"
      (Fexpr.named)
# 1741 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv637 * _menhir_state * 'tv_kinded_variable_opt)) * _menhir_state * (
# 115 "flambda_parser.mly"
      (Fexpr.named)
# 1747 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (v : 'tv_kinded_variable_opt)), _, (defining_expr : (
# 115 "flambda_parser.mly"
      (Fexpr.named)
# 1752 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_let_binding = 
# 338 "flambda_parser.mly"
      ( let (var, kind) = v in { var; kind; defining_expr } )
# 1758 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv635) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_let_binding) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv633 * _menhir_state * 'tv_let_binding) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv627 * _menhir_state * 'tv_let_binding) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run188 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState202 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNDERSCORE ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState202
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState202) : 'freshtv628)
        | IN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv629 * _menhir_state * 'tv_let_binding) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_let_binding)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_AND_let_binding_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1791 "flambda_parser.ml"
             in
            _menhir_goto_separated_nonempty_list_AND_let_binding_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv630)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv631 * _menhir_state * 'tv_let_binding) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv632)) : 'freshtv634)) : 'freshtv636)) : 'freshtv638)) : 'freshtv640)
    | _ ->
        _menhir_fail ()

and _menhir_goto_simple : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simple -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv553 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv549 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SEMICOLON ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | RBRACE ->
                _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28) : 'freshtv550)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv551 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv552)) : 'freshtv554)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv567 * _menhir_state * 'tv_var_within_closure)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv565 * _menhir_state * 'tv_var_within_closure)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (var : 'tv_var_within_closure)), _, (value : 'tv_simple)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_closure_element = 
# 350 "flambda_parser.mly"
                                                     ( { var; value; } )
# 1847 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv563) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_closure_element) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv561 * _menhir_state * 'tv_closure_element) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv555 * _menhir_state * 'tv_closure_element) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92) : 'freshtv556)
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv557 * _menhir_state * 'tv_closure_element) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_closure_element)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_closure_element_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1878 "flambda_parser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_closure_element_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv558)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv559 * _menhir_state * 'tv_closure_element) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv560)) : 'freshtv562)) : 'freshtv564)) : 'freshtv566)) : 'freshtv568)
    | MenhirState147 | MenhirState130 | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv569 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FLOAT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
        | INT _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LIDENT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAREN ->
            _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130) : 'freshtv570)
    | MenhirState158 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv573 * _menhir_state * 'tv_unop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv571 * _menhir_state * 'tv_unop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (u : 'tv_unop)), _, (a : 'tv_simple)) = _menhir_stack in
        let _v : (
# 115 "flambda_parser.mly"
      (Fexpr.named)
# 1917 "flambda_parser.ml"
        ) = 
# 262 "flambda_parser.mly"
                        ( Prim (Unop (u, a)) )
# 1921 "flambda_parser.ml"
         in
        _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv572)) : 'freshtv574)
    | MenhirState6 | MenhirState217 | MenhirState61 | MenhirState205 | MenhirState196 | MenhirState191 | MenhirState172 | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv601 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv579 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv575 * _menhir_state * 'tv_simple)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | FLOAT _v ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
                | INT _v ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState166 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LIDENT _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState166 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UIDENT _v ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState166 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166) : 'freshtv576)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv577 * _menhir_state * 'tv_simple)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv578)) : 'freshtv580)
        | MINUS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv583) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv581) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_infix_binop = 
# 252 "flambda_parser.mly"
          ( Minus )
# 1971 "flambda_parser.ml"
             in
            _menhir_goto_infix_binop _menhir_env _menhir_stack _v) : 'freshtv582)) : 'freshtv584)
        | MINUSDOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv587) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv585) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_infix_binop = 
# 253 "flambda_parser.mly"
             ( Minusdot )
# 1984 "flambda_parser.ml"
             in
            _menhir_goto_infix_binop _menhir_env _menhir_stack _v) : 'freshtv586)) : 'freshtv588)
        | PLUS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv591) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv589) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_infix_binop = 
# 250 "flambda_parser.mly"
         ( Plus )
# 1997 "flambda_parser.ml"
             in
            _menhir_goto_infix_binop _menhir_env _menhir_stack _v) : 'freshtv590)) : 'freshtv592)
        | PLUSDOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv595) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv593) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_infix_binop = 
# 251 "flambda_parser.mly"
            ( Plusdot )
# 2010 "flambda_parser.ml"
             in
            _menhir_goto_infix_binop _menhir_env _menhir_stack _v) : 'freshtv594)) : 'freshtv596)
        | AND | IN | SEMICOLON | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv597 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (s : 'tv_simple)) = _menhir_stack in
            let _v : (
# 115 "flambda_parser.mly"
      (Fexpr.named)
# 2020 "flambda_parser.ml"
            ) = 
# 261 "flambda_parser.mly"
               ( Simple s )
# 2024 "flambda_parser.ml"
             in
            _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv598)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv599 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv600)) : 'freshtv602)
    | MenhirState166 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv615 * _menhir_state * 'tv_simple))) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv611 * _menhir_state * 'tv_simple))) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv609 * _menhir_state * 'tv_simple))) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (a : 'tv_simple)), _, (f : 'tv_simple)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _v : 'tv_binop = 
# 258 "flambda_parser.mly"
    ( Binop (Block_load (Block Value, Immutable), a, f) )
# 2053 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv607) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_binop) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv605) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_binop) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv603) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((b : 'tv_binop) : 'tv_binop) = _v in
            ((let _v : (
# 115 "flambda_parser.mly"
      (Fexpr.named)
# 2070 "flambda_parser.ml"
            ) = 
# 264 "flambda_parser.mly"
              ( Prim b )
# 2074 "flambda_parser.ml"
             in
            _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv604)) : 'freshtv606)) : 'freshtv608)) : 'freshtv610)) : 'freshtv612)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv613 * _menhir_state * 'tv_simple))) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv614)) : 'freshtv616)
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv619 * _menhir_state * 'tv_simple) * 'tv_infix_binop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv617 * _menhir_state * 'tv_simple) * 'tv_infix_binop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (a1 : 'tv_simple)), (b : 'tv_infix_binop)), _, (a2 : 'tv_simple)) = _menhir_stack in
        let _v : (
# 115 "flambda_parser.mly"
      (Fexpr.named)
# 2093 "flambda_parser.ml"
        ) = 
# 263 "flambda_parser.mly"
                                            ( Prim (Infix_binop (b, a1, a2)) )
# 2097 "flambda_parser.ml"
         in
        _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv618)) : 'freshtv620)
    | _ ->
        _menhir_fail ()

and _menhir_reduce79 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_recursive = 
# 223 "flambda_parser.mly"
    ( Nonrecursive )
# 2108 "flambda_parser.ml"
     in
    _menhir_goto_recursive _menhir_env _menhir_stack _menhir_s _v

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv547) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_recursive = 
# 224 "flambda_parser.mly"
        ( Recursive )
# 2122 "flambda_parser.ml"
     in
    _menhir_goto_recursive _menhir_env _menhir_stack _menhir_s _v) : 'freshtv548)

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv545) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_variable_opt = 
# 453 "flambda_parser.mly"
               ( None )
# 2136 "flambda_parser.ml"
     in
    _menhir_goto_variable_opt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv546)

and _menhir_run188 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 82 "flambda_parser.mly"
       (string)
# 2143 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv543) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 82 "flambda_parser.mly"
       (string)
# 2154 "flambda_parser.ml"
    )) : (
# 82 "flambda_parser.mly"
       (string)
# 2158 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_variable_opt = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 454 "flambda_parser.mly"
               ( Some (e, make_loc (_startpos, _endpos)) )
# 2166 "flambda_parser.ml"
     in
    _menhir_goto_variable_opt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv544)

and _menhir_goto_const : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_const -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv541) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_const) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv539) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : 'tv_const) : 'tv_const) = _v in
    ((let _v : 'tv_simple = 
# 429 "flambda_parser.mly"
              ( Const c )
# 2183 "flambda_parser.ml"
     in
    _menhir_goto_simple _menhir_env _menhir_stack _menhir_s _v) : 'freshtv540)) : 'freshtv542)

and _menhir_goto_andk : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_andk -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState215 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv535 * _menhir_state) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv533 * _menhir_state) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (h : 'tv_continuation_handler)), _, (t : 'tv_andk)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_andk = 
# 373 "flambda_parser.mly"
                                          ( h :: t )
# 2201 "flambda_parser.ml"
         in
        _menhir_goto_andk _menhir_env _menhir_stack _menhir_s _v) : 'freshtv534)) : 'freshtv536)
    | MenhirState213 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv537 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | APPLY ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState217
        | BLOCK ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState217
        | CCALL ->
            _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState217
        | CLOSURE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState217
        | CONT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState217
        | FLOAT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState217 _v
        | HCF ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState217
        | INT _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState217 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState217
        | LETK ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState217
        | LIDENT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState217 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPAQUE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState217
        | PROJECT_VAR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState217
        | SWITCH ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState217
        | UIDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState217 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNREACHABLE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState217
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState217) : 'freshtv538)
    | _ ->
        _menhir_fail ()

and _menhir_reduce16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_exn_and_stub = 
# 358 "flambda_parser.mly"
    ( false, false )
# 2254 "flambda_parser.ml"
     in
    _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv527 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv525 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 361 "flambda_parser.mly"
             ( true, true )
# 2276 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv526)) : 'freshtv528)
    | LIDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv529 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 359 "flambda_parser.mly"
         ( false, true )
# 2287 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv530)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv531 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv532)

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STUB ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv519 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv517 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 362 "flambda_parser.mly"
             ( true, true )
# 2316 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv518)) : 'freshtv520)
    | LIDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv521 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 360 "flambda_parser.mly"
        ( true, false )
# 2327 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv522)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv523 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv524)

and _menhir_goto_separated_nonempty_list_AND_code_or_closure_binding_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_AND_code_or_closure_binding_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv511 * _menhir_state * 'tv_separated_nonempty_list_AND_code_or_closure_binding_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | WITH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState198
        | IN ->
            _menhir_reduce123 _menhir_env (Obj.magic _menhir_stack) MenhirState198
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState198) : 'freshtv512)
    | MenhirState209 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv515 * _menhir_state * 'tv_code_or_closure_binding)) * _menhir_state * 'tv_separated_nonempty_list_AND_code_or_closure_binding_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv513 * _menhir_state * 'tv_code_or_closure_binding)) * _menhir_state * 'tv_separated_nonempty_list_AND_code_or_closure_binding_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_code_or_closure_binding)), _, (xs : 'tv_separated_nonempty_list_AND_code_or_closure_binding_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_AND_code_or_closure_binding_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 2366 "flambda_parser.ml"
         in
        _menhir_goto_separated_nonempty_list_AND_code_or_closure_binding_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv514)) : 'freshtv516)
    | _ ->
        _menhir_fail ()

and _menhir_reduce123 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_with_closure_elements_opt = 
# 342 "flambda_parser.mly"
    ( None )
# 2377 "flambda_parser.ml"
     in
    _menhir_goto_with_closure_elements_opt _menhir_env _menhir_stack _menhir_s _v

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv507 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv505) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState84 in
            ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__ = 
# 142 "<standard.mly>"
    ( [] )
# 2402 "flambda_parser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_SEMICOLON_closure_element__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv506)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv508)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv509 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv510)

and _menhir_run94 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | UIDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

and _menhir_run97 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | REC ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | LIDENT _ ->
        _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_goto_simple_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simple_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv495 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv493 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (c : 'tv_continuation)), _, (s : 'tv_simple_args)) = _menhir_stack in
        let _1 = () in
        let _v : (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 2459 "flambda_parser.ml"
        ) = 
# 293 "flambda_parser.mly"
                                          ( Apply_cont (c, None, s) )
# 2463 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv494)) : 'freshtv496)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv497 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | MINUSGREATER ->
            _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140) : 'freshtv498)
    | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv503 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv499 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155) : 'freshtv500)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv501 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv502)) : 'freshtv504)
    | _ ->
        _menhir_fail ()

and _menhir_goto_kinded_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_kinded_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv489 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_kinded_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv485 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_kinded_args) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APPLY ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | BLOCK ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | CCALL ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | CLOSURE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | CONT ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | FLOAT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | HCF ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | INT _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | LETK ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | LIDENT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | PROJECT_VAR ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | SWITCH ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | UIDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61) : 'freshtv486)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv487 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_kinded_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv488)) : 'freshtv490)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv491 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) * _menhir_state * 'tv_kinded_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107) : 'freshtv492)
    | _ ->
        _menhir_fail ()

and _menhir_reduce52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_kinded_variable_ = 
# 211 "<standard.mly>"
    ( [] )
# 2587 "flambda_parser.ml"
     in
    _menhir_goto_list_kinded_variable_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_SEMICOLON_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_SEMICOLON_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv483) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_option_SEMICOLON_) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv481) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : 'tv_option_SEMICOLON_) : 'tv_option_SEMICOLON_) = _v in
    ((let _v : 'tv_switch = 
# 274 "flambda_parser.mly"
                      ( [] )
# 2604 "flambda_parser.ml"
     in
    _menhir_goto_switch _menhir_env _menhir_stack _menhir_s _v) : 'freshtv482)) : 'freshtv484)

and _menhir_reduce56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_simple_ = 
# 211 "<standard.mly>"
    ( [] )
# 2613 "flambda_parser.ml"
     in
    _menhir_goto_list_simple_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_of_kind_value_ = 
# 211 "<standard.mly>"
    ( [] )
# 2622 "flambda_parser.ml"
     in
    _menhir_goto_list_of_kind_value_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run73 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 76 "flambda_parser.mly"
       (string * char option)
# 2629 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv479) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 76 "flambda_parser.mly"
       (string * char option)
# 2640 "flambda_parser.ml"
    )) : (
# 76 "flambda_parser.mly"
       (string * char option)
# 2644 "flambda_parser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _v : (
# 116 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 2650 "flambda_parser.ml"
    ) = let _endpos = _endpos_i_ in
    let _startpos = _startpos_i_ in
    
# 396 "flambda_parser.mly"
            ( Tagged_immediate ( make_tagged_immediate ~loc:(_startpos, _endpos) i ) )
# 2656 "flambda_parser.ml"
     in
    _menhir_goto_of_kind_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv480)

and _menhir_reduce81 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_return_arity = 
# 286 "flambda_parser.mly"
    ( None )
# 2665 "flambda_parser.ml"
     in
    _menhir_goto_return_arity _menhir_env _menhir_stack _menhir_s _v

and _menhir_run119 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL | MINUSGREATER ->
        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | COMMA ->
        _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv477) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 2694 "flambda_parser.ml"
    ) = 
# 292 "flambda_parser.mly"
                ( Invalid Treat_as_unreachable )
# 2698 "flambda_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv478)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 104 "flambda_parser.mly"
       (string)
# 2705 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv475) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 104 "flambda_parser.mly"
       (string)
# 2716 "flambda_parser.ml"
    )) : (
# 104 "flambda_parser.mly"
       (string)
# 2720 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_symbol = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 441 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 2728 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv473) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_symbol) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 | MenhirState217 | MenhirState61 | MenhirState205 | MenhirState196 | MenhirState191 | MenhirState172 | MenhirState169 | MenhirState166 | MenhirState125 | MenhirState158 | MenhirState147 | MenhirState130 | MenhirState129 | MenhirState86 | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv449 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv447 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (s : 'tv_symbol)) = _menhir_stack in
        let _v : 'tv_simple = 
# 427 "flambda_parser.mly"
               ( Symbol s )
# 2745 "flambda_parser.ml"
         in
        _menhir_goto_simple _menhir_env _menhir_stack _menhir_s _v) : 'freshtv448)) : 'freshtv450)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv457 * _menhir_state) * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv453 * _menhir_state) * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BLOCK ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv451 * _menhir_state * 'tv_symbol)) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState66 in
                ((let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | INT _v ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv452)
            | CLOSURE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66) : 'freshtv454)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv455 * _menhir_state) * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv456)) : 'freshtv458)
    | MenhirState76 | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv461 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv459 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (s : 'tv_symbol)) = _menhir_stack in
        let _v : (
# 116 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 2796 "flambda_parser.ml"
        ) = 
# 394 "flambda_parser.mly"
               ( Symbol s )
# 2800 "flambda_parser.ml"
         in
        _menhir_goto_of_kind_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv460)) : 'freshtv462)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv467 * _menhir_state) * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv463 * _menhir_state) * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CLOSURE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv464)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv465 * _menhir_state) * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv466)) : 'freshtv468)
    | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv471 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv469 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (s : 'tv_symbol)) = _menhir_stack in
        let _v : 'tv_name = 
# 422 "flambda_parser.mly"
               ( (Symbol s:name) )
# 2837 "flambda_parser.ml"
         in
        _menhir_goto_name _menhir_env _menhir_stack _menhir_s _v) : 'freshtv470)) : 'freshtv472)
    | _ ->
        _menhir_fail ()) : 'freshtv474)) : 'freshtv476)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IS_INT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv435) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv433) = Obj.magic _menhir_stack in
        ((let _1 = () in
        let _v : 'tv_switch_sort = 
# 242 "flambda_parser.mly"
           ( Is_int )
# 2859 "flambda_parser.ml"
         in
        _menhir_goto_switch_sort _menhir_env _menhir_stack _v) : 'freshtv434)) : 'freshtv436)
    | TAG ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv441) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv437) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RBRACKET ->
                _menhir_reduce114 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11) : 'freshtv438)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv439) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv440)) : 'freshtv442)
    | FLOAT _ | INT _ | LIDENT _ | UIDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv443) = Obj.magic _menhir_stack in
        ((let _v : 'tv_switch_sort = 
# 241 "flambda_parser.mly"
    ( Int )
# 2894 "flambda_parser.ml"
         in
        _menhir_goto_switch_sort _menhir_env _menhir_stack _v) : 'freshtv444)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv445 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv446)

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState41 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv431) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_unop = 
# 246 "flambda_parser.mly"
           ( Opaque_identity )
# 2928 "flambda_parser.ml"
     in
    _menhir_goto_unop _menhir_env _menhir_stack _menhir_s _v) : 'freshtv432)

and _menhir_run22 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 82 "flambda_parser.mly"
       (string)
# 2935 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv429) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 82 "flambda_parser.mly"
       (string)
# 2946 "flambda_parser.ml"
    )) : (
# 82 "flambda_parser.mly"
       (string)
# 2950 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_variable = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 449 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 2958 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv427) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_variable) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 | MenhirState217 | MenhirState61 | MenhirState205 | MenhirState196 | MenhirState191 | MenhirState172 | MenhirState169 | MenhirState166 | MenhirState158 | MenhirState125 | MenhirState147 | MenhirState130 | MenhirState129 | MenhirState86 | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv353 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv351 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
        let _v : 'tv_simple = 
# 428 "flambda_parser.mly"
                 ( Var v )
# 2975 "flambda_parser.ml"
         in
        _menhir_goto_simple _menhir_env _menhir_stack _menhir_s _v) : 'freshtv352)) : 'freshtv354)
    | MenhirState58 | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv361 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv359 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (param : 'tv_variable)) = _menhir_stack in
        let _v : 'tv_kinded_variable = 
# 400 "flambda_parser.mly"
                     ( { param; kind = None } )
# 2987 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv357) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_kinded_variable) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv355 * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAREN ->
            _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv356)) : 'freshtv358)) : 'freshtv360)) : 'freshtv362)
    | MenhirState104 | MenhirState98 | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv399 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv397 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
        let _v : 'tv_code_id = 
# 433 "flambda_parser.mly"
                 ( v )
# 3016 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv395) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_code_id) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState67 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv381 * _menhir_state) * _menhir_state * 'tv_code_id) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv379 * _menhir_state) * _menhir_state * 'tv_code_id) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (code_id : 'tv_code_id)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_closure = 
# 354 "flambda_parser.mly"
                                ( code_id )
# 3034 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv377) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_closure) = _v in
            ((match _menhir_s with
            | MenhirState96 | MenhirState66 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv371 * _menhir_state) * _menhir_state * 'tv_symbol)) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_closure) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv369 * _menhir_state) * _menhir_state * 'tv_symbol)) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let ((code_id : 'tv_closure) : 'tv_closure) = _v in
                ((let ((_menhir_stack, _menhir_s), _, (symbol : 'tv_symbol)) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : 'tv_static_closure_binding = 
# 219 "flambda_parser.mly"
                                                       ( { symbol; code_id } )
# 3056 "flambda_parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv367) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_static_closure_binding) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv365) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_static_closure_binding) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv363) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((closure_binding : 'tv_static_closure_binding) : 'tv_static_closure_binding) = _v in
                ((let _v : 'tv_code_or_closure_binding = 
# 197 "flambda_parser.mly"
                                             ( `Closure closure_binding )
# 3073 "flambda_parser.ml"
                 in
                _menhir_goto_code_or_closure_binding _menhir_env _menhir_stack _menhir_s _v) : 'freshtv364)) : 'freshtv366)) : 'freshtv368)) : 'freshtv370)) : 'freshtv372)
            | MenhirState6 | MenhirState217 | MenhirState61 | MenhirState205 | MenhirState196 | MenhirState191 | MenhirState125 | MenhirState172 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv375) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_closure) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv373) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((c : 'tv_closure) : 'tv_closure) = _v in
                ((let _v : (
# 115 "flambda_parser.mly"
      (Fexpr.named)
# 3088 "flambda_parser.ml"
                ) = 
# 266 "flambda_parser.mly"
                ( Closure { code_id = c } )
# 3092 "flambda_parser.ml"
                 in
                _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv374)) : 'freshtv376)
            | _ ->
                _menhir_fail ()) : 'freshtv378)) : 'freshtv380)) : 'freshtv382)
        | MenhirState98 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv389 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | AT ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv383) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LIDENT _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100) : 'freshtv384)
            | LIDENT _ | LPAREN | NEWER_VERSION_OF ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv385) = Obj.magic _menhir_stack in
                ((let _v : 'tv_option___anonymous_0_ = 
# 114 "<standard.mly>"
    ( None )
# 3121 "flambda_parser.ml"
                 in
                _menhir_goto_option___anonymous_0_ _menhir_env _menhir_stack _v) : 'freshtv386)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv387 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv388)) : 'freshtv390)
        | MenhirState104 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv393) * _menhir_state * 'tv_code_id) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv391) * _menhir_state * 'tv_code_id) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, (id : 'tv_code_id)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_option___anonymous_1_ = let x = 
# 205 "flambda_parser.mly"
                                                             ( id )
# 3141 "flambda_parser.ml"
             in
            
# 116 "<standard.mly>"
    ( Some x )
# 3146 "flambda_parser.ml"
             in
            _menhir_goto_option___anonymous_1_ _menhir_env _menhir_stack _v) : 'freshtv392)) : 'freshtv394)
        | _ ->
            _menhir_fail ()) : 'freshtv396)) : 'freshtv398)) : 'freshtv400)
    | MenhirState76 | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv403 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv401 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
        let _v : (
# 116 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 3160 "flambda_parser.ml"
        ) = 
# 395 "flambda_parser.mly"
                 ( Dynamically_computed v )
# 3164 "flambda_parser.ml"
         in
        _menhir_goto_of_kind_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv402)) : 'freshtv404)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv413 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv411 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
        let _v : 'tv_closure_id = 
# 437 "flambda_parser.mly"
                 ( v )
# 3176 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv409) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_closure_id) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv407) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_closure_id) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv405) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((cid : 'tv_closure_id) : 'tv_closure_id) = _v in
        ((let _1 = () in
        let _v : 'tv_option___anonymous_0_ = let x = 
# 204 "flambda_parser.mly"
                                             ( cid )
# 3194 "flambda_parser.ml"
         in
        
# 116 "<standard.mly>"
    ( Some x )
# 3199 "flambda_parser.ml"
         in
        _menhir_goto_option___anonymous_0_ _menhir_env _menhir_stack _v) : 'freshtv406)) : 'freshtv408)) : 'freshtv410)) : 'freshtv412)) : 'freshtv414)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv421 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LANGLE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv415) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RANGLE ->
                _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109) : 'freshtv416)
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv417) = Obj.magic _menhir_stack in
            ((let _v : 'tv_kinded_vars_within_closures = 
# 382 "flambda_parser.mly"
    ( [] )
# 3228 "flambda_parser.ml"
             in
            _menhir_goto_kinded_vars_within_closures _menhir_env _menhir_stack _v) : 'freshtv418)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv419 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv420)) : 'freshtv422)
    | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv425 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv423 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
        let _v : 'tv_name = 
# 423 "flambda_parser.mly"
                 ( (Var v:name) )
# 3247 "flambda_parser.ml"
         in
        _menhir_goto_name _menhir_env _menhir_stack _menhir_s _v) : 'freshtv424)) : 'freshtv426)
    | _ ->
        _menhir_fail ()) : 'freshtv428)) : 'freshtv430)

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | REC ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | EXN | LIDENT _ | STUB ->
        _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CODE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LIDENT _v ->
        _menhir_run188 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SEGMENT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv347) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState62 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CODE ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SYMBOL ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | WITH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | IN | SEGMENT ->
            _menhir_reduce123 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82) : 'freshtv348)
    | SYMBOL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv349) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState62 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | UIDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64) : 'freshtv350)
    | UNDERSCORE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run23 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 76 "flambda_parser.mly"
       (string * char option)
# 3322 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv345) = Obj.magic _menhir_stack in
    let (_endpos_c_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : (
# 76 "flambda_parser.mly"
       (string * char option)
# 3333 "flambda_parser.ml"
    )) : (
# 76 "flambda_parser.mly"
       (string * char option)
# 3337 "flambda_parser.ml"
    )) = _v in
    let (_startpos_c_ : Lexing.position) = _startpos in
    ((let _v : 'tv_const = 
# 417 "flambda_parser.mly"
            ( make_const_int c )
# 3343 "flambda_parser.ml"
     in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v) : 'freshtv346)

and _menhir_run126 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv343) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 3357 "flambda_parser.ml"
    ) = 
# 291 "flambda_parser.mly"
        ( Invalid Halt_and_catch_fire )
# 3361 "flambda_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv344)

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 72 "flambda_parser.mly"
       (string * char option)
# 3368 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv341) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : (
# 72 "flambda_parser.mly"
       (string * char option)
# 3378 "flambda_parser.ml"
    )) : (
# 72 "flambda_parser.mly"
       (string * char option)
# 3382 "flambda_parser.ml"
    )) = _v in
    ((let _v : 'tv_const = 
# 418 "flambda_parser.mly"
              ( make_const_float c )
# 3387 "flambda_parser.ml"
     in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v) : 'freshtv342)

and _menhir_run127 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run135 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv337 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv333) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 82 "flambda_parser.mly"
       (string)
# 3436 "flambda_parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv331) = Obj.magic _menhir_stack in
            let (_endpos_s_ : Lexing.position) = _endpos in
            let ((s : (
# 82 "flambda_parser.mly"
       (string)
# 3446 "flambda_parser.ml"
            )) : (
# 82 "flambda_parser.mly"
       (string)
# 3450 "flambda_parser.ml"
            )) = _v in
            let (_startpos_s_ : Lexing.position) = _startpos in
            ((let _v : 'tv_csymbol = let _endpos = _endpos_s_ in
            let _startpos = _startpos_s_ in
            
# 445 "flambda_parser.mly"
               ( s, make_loc (_startpos, _endpos) )
# 3458 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv329) = _menhir_stack in
            let (_v : 'tv_csymbol) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv327 * _menhir_state)) * 'tv_csymbol) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RBRACKET ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv323 * _menhir_state)) * 'tv_csymbol) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LPAREN ->
                    _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState139
                | COLON | MINUSGREATER ->
                    _menhir_reduce97 _menhir_env (Obj.magic _menhir_stack) MenhirState139
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139) : 'freshtv324)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv325 * _menhir_state)) * 'tv_csymbol) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv326)) : 'freshtv328)) : 'freshtv330)) : 'freshtv332)) : 'freshtv334)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv335 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)) : 'freshtv338)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv339 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv340)

and _menhir_run145 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState145 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145

and _menhir_run150 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState150 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState150 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150

and _menhir_reduce2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_andk = 
# 374 "flambda_parser.mly"
    ( [] )
# 3538 "flambda_parser.ml"
     in
    _menhir_goto_andk _menhir_env _menhir_stack _menhir_s _v

and _menhir_run214 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState214
    | STUB ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState214
    | LIDENT _ ->
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState214
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState214

and _menhir_goto_code_or_closure_binding : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_code_or_closure_binding -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState82 | MenhirState184 | MenhirState180 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv313 * _menhir_state * 'tv_code_or_closure_binding) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv309 * _menhir_state * 'tv_code_or_closure_binding) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CODE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | SYMBOL ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | WITH ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | IN | SEGMENT ->
                _menhir_reduce123 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState184) : 'freshtv310)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv311 * _menhir_state * 'tv_code_or_closure_binding) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv312)) : 'freshtv314)
    | MenhirState209 | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv321 * _menhir_state * 'tv_code_or_closure_binding) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv315 * _menhir_state * 'tv_code_or_closure_binding) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CODE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | SYMBOL ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState209) : 'freshtv316)
        | IN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv317 * _menhir_state * 'tv_code_or_closure_binding) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_code_or_closure_binding)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_AND_code_or_closure_binding_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3621 "flambda_parser.ml"
             in
            _menhir_goto_separated_nonempty_list_AND_code_or_closure_binding_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv318)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv319 * _menhir_state * 'tv_code_or_closure_binding) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)) : 'freshtv322)
    | _ ->
        _menhir_fail ()

and _menhir_reduce97 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_simple_args = 
# 412 "flambda_parser.mly"
    ( [] )
# 3639 "flambda_parser.ml"
     in
    _menhir_goto_simple_args _menhir_env _menhir_stack _menhir_s _v

and _menhir_run129 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
    | INT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_kinded_args = 
# 378 "flambda_parser.mly"
    ( [] )
# 3669 "flambda_parser.ml"
     in
    _menhir_goto_kinded_args _menhir_env _menhir_stack _menhir_s _v

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_goto_switch : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_switch -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv299 * _menhir_state * 'tv_switch_case)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv297 * _menhir_state * 'tv_switch_case)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (h : 'tv_switch_case)), _, (t : 'tv_switch)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_switch = 
# 276 "flambda_parser.mly"
                                         ( h :: t )
# 3702 "flambda_parser.ml"
         in
        _menhir_goto_switch _menhir_env _menhir_stack _menhir_s _v) : 'freshtv298)) : 'freshtv300)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv307 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv303 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv301 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), (sort : 'tv_switch_sort)), _, (scrutinee : 'tv_simple)), _, (cases : 'tv_switch)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 3724 "flambda_parser.ml"
            ) = 
# 295 "flambda_parser.mly"
    ( Switch {scrutinee; sort; cases} )
# 3728 "flambda_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv302)) : 'freshtv304)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv305 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv306)) : 'freshtv308)
    | _ ->
        _menhir_fail ()

and _menhir_reduce71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_option_SEMICOLON_ = 
# 114 "<standard.mly>"
    ( None )
# 3746 "flambda_parser.ml"
     in
    _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv295) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let x = () in
    let _v : 'tv_option_SEMICOLON_ = 
# 116 "<standard.mly>"
    ( Some x )
# 3760 "flambda_parser.ml"
     in
    _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv296)

and _menhir_run30 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 76 "flambda_parser.mly"
       (string * char option)
# 3767 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv293) = Obj.magic _menhir_stack in
    let (_endpos_tag_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((tag : (
# 76 "flambda_parser.mly"
       (string * char option)
# 3778 "flambda_parser.ml"
    )) : (
# 76 "flambda_parser.mly"
       (string * char option)
# 3782 "flambda_parser.ml"
    )) = _v in
    let (_startpos_tag_ : Lexing.position) = _startpos in
    ((let _v : 'tv_tag = let _endpos = _endpos_tag_ in
    let _startpos = _startpos_tag_ in
    
# 390 "flambda_parser.mly"
            ( make_tag ~loc:(make_loc (_startpos, _endpos)) tag )
# 3790 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv291) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_tag) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState35 | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv277 * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv273 * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState32 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv274)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv275 * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv276)) : 'freshtv278)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv283 * _menhir_state * 'tv_symbol)) * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv279 * _menhir_state * 'tv_symbol)) * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIDENT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72) : 'freshtv280)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv281 * _menhir_state * 'tv_symbol)) * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)) : 'freshtv284)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv289 * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv285 * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FLOAT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
            | INT _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState147 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIDENT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState147 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState147 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147) : 'freshtv286)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv287 * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv288)) : 'freshtv290)
    | _ ->
        _menhir_fail ()) : 'freshtv292)) : 'freshtv294)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_option_exn_continuation_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_exn_continuation_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv269 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | APPLY ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | BLOCK ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | CCALL ->
            _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | CLOSURE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | CONT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | FLOAT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | HCF ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | INT _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState6 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | LETK ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | LIDENT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState6 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPAQUE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | PROJECT_VAR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | SWITCH ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | UIDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState6 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNREACHABLE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv270)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((('freshtv271 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | EQUAL ->
            _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118) : 'freshtv272)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 3961 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState172 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv205 * _menhir_state * (
# 115 "flambda_parser.mly"
      (Fexpr.named)
# 3971 "flambda_parser.ml"
        ))) * _menhir_state * (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 3975 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv203 * _menhir_state * (
# 115 "flambda_parser.mly"
      (Fexpr.named)
# 3981 "flambda_parser.ml"
        ))) * _menhir_state * (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 3985 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (defining_expr : (
# 115 "flambda_parser.mly"
      (Fexpr.named)
# 3990 "flambda_parser.ml"
        ))), _, (body : (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 3994 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4000 "flambda_parser.ml"
        ) = 
# 298 "flambda_parser.mly"
      ( Let { bindings = [ { var = None; kind = None; defining_expr; } ];
              closure_elements = None; 
              body } )
# 4006 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv204)) : 'freshtv206)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((((((('freshtv215 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) * _menhir_state * (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4014 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((((((('freshtv213 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) * _menhir_state * (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4020 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((((((((((_menhir_stack, _menhir_s), _, (recursive : 'tv_recursive)), _, (id : 'tv_code_id)), (closure_id : 'tv_option___anonymous_0_)), (newer_version_of : 'tv_option___anonymous_1_)), _, (params : 'tv_kinded_args)), _, (closure_var : 'tv_variable)), (vars_within_closures : 'tv_kinded_vars_within_closures)), _, (ret_cont : 'tv_continuation)), _, (exn_cont : 'tv_option_exn_continuation_)), _, (ret_arity : 'tv_return_arity)), _, (expr : (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4025 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _13 = () in
        let _9 = () in
        let _1 = () in
        let _v : 'tv_code_binding = 
# 213 "flambda_parser.mly"
    ( { id; closure_id; newer_version_of; params; closure_var;
        vars_within_closures; ret_cont; ret_arity; exn_cont; recursive; expr; }
       : code_binding )
# 4035 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_code_binding) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv209) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_code_binding) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv207) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((code_binding : 'tv_code_binding) : 'tv_code_binding) = _v in
        ((let _v : 'tv_code_or_closure_binding = 
# 196 "flambda_parser.mly"
                                ( `Code code_binding )
# 4052 "flambda_parser.ml"
         in
        _menhir_goto_code_or_closure_binding _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)) : 'freshtv210)) : 'freshtv212)) : 'freshtv214)) : 'freshtv216)
    | MenhirState191 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv225 * _menhir_state) * _menhir_state * 'tv_symbol_bindings)) * _menhir_state * (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4060 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv223 * _menhir_state) * _menhir_state * 'tv_symbol_bindings)) * _menhir_state * (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4066 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (bindings : 'tv_symbol_bindings)), _, (body : (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4071 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_let_symbol = 
# 132 "flambda_parser.mly"
                                                      ( { bindings; body } )
# 4078 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv221) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_let_symbol) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv219) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_let_symbol) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv217) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((ls : 'tv_let_symbol) : 'tv_let_symbol) = _v in
        ((let _v : (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4095 "flambda_parser.ml"
        ) = 
# 304 "flambda_parser.mly"
                     ( Let_symbol ls )
# 4099 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv218)) : 'freshtv220)) : 'freshtv222)) : 'freshtv224)) : 'freshtv226)
    | MenhirState196 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv235 * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) * _menhir_state * 'tv_with_closure_elements_opt)) * _menhir_state * (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4107 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv233 * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) * _menhir_state * 'tv_with_closure_elements_opt)) * _menhir_state * (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4113 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (bindings : 'tv_separated_nonempty_list_AND_let_binding_)), _, (closure_elements : 'tv_with_closure_elements_opt)), _, (body : (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4118 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _3 = () in
        let _v : 'tv_let_ = 
# 333 "flambda_parser.mly"
    ( { bindings; closure_elements; body } )
# 4124 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv231) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_let_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv229 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_let_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv227 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((l : 'tv_let_) : 'tv_let_) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4143 "flambda_parser.ml"
        ) = 
# 296 "flambda_parser.mly"
                 ( Let l )
# 4147 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv228)) : 'freshtv230)) : 'freshtv232)) : 'freshtv234)) : 'freshtv236)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv249 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_kinded_args)) * _menhir_state * (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4155 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv245 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_kinded_args)) * _menhir_state * (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4165 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv243 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_kinded_args)) * _menhir_state * (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4172 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s, (exn_and_stub : 'tv_exn_and_stub)), _, (name : 'tv_continuation)), _, (params : 'tv_kinded_args)), _, (handler : (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4177 "flambda_parser.ml"
            ))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _v : 'tv_continuation_handler = 
# 368 "flambda_parser.mly"
    ( let is_exn_handler, stub = exn_and_stub in
      { name; params; stub; is_exn_handler; handler } )
# 4185 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv241) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_continuation_handler) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState47 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv237 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AND ->
                    _menhir_run214 _menhir_env (Obj.magic _menhir_stack) MenhirState213
                | APPLY | BLOCK | CCALL | CLOSURE | CONT | FLOAT _ | HCF | INT _ | LET | LETK | LIDENT _ | OPAQUE | PROJECT_VAR | SWITCH | UIDENT _ | UNREACHABLE ->
                    _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState213
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState213) : 'freshtv238)
            | MenhirState214 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv239 * _menhir_state) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AND ->
                    _menhir_run214 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                | APPLY | BLOCK | CCALL | CLOSURE | CONT | FLOAT _ | HCF | INT _ | LET | LETK | LIDENT _ | OPAQUE | PROJECT_VAR | SWITCH | UIDENT _ | UNREACHABLE ->
                    _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState215) : 'freshtv240)
            | _ ->
                _menhir_fail ()) : 'freshtv242)) : 'freshtv244)) : 'freshtv246)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv247 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_kinded_args)) * _menhir_state * (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4230 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)) : 'freshtv250)
    | MenhirState217 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv253 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) * _menhir_state * (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4239 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv251 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) * _menhir_state * (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4245 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s), _, (recursive : 'tv_recursive)), _, (handler : 'tv_continuation_handler)), _, (t : 'tv_andk)), _, (body : (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4250 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4256 "flambda_parser.ml"
        ) = 
# 302 "flambda_parser.mly"
     ( let handlers = handler :: t in
       Let_cont { recursive; body; handlers } )
# 4261 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv252)) : 'freshtv254)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv267 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4269 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv263 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4279 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv261 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4285 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (return_cont : 'tv_continuation)), _, (exception_cont : 'tv_option_exn_continuation_)), _, (body : (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4290 "flambda_parser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _v : (
# 111 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 4296 "flambda_parser.ml"
            ) = 
# 124 "flambda_parser.mly"
    ( { return_cont; exception_cont; body } )
# 4300 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv259) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 111 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 4308 "flambda_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv257) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 111 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 4316 "flambda_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv255) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 111 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 4324 "flambda_parser.ml"
            )) : (
# 111 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 4328 "flambda_parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv256)) : 'freshtv258)) : 'freshtv260)) : 'freshtv262)) : 'freshtv264)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv265 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4338 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv266)) : 'freshtv268)
    | _ ->
        _menhir_fail ()

and _menhir_reduce77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_option_exn_continuation_ = 
# 114 "<standard.mly>"
    ( None )
# 4350 "flambda_parser.ml"
     in
    _menhir_goto_option_exn_continuation_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState4 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState217 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv51 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState215 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState214 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState213 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv57 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState209 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * 'tv_code_or_closure_binding)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState205 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * 'tv_kinded_variable_opt)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState202 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state * 'tv_let_binding)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState198 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state * 'tv_separated_nonempty_list_AND_code_or_closure_binding_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState196 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv67 * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) * _menhir_state * 'tv_with_closure_elements_opt)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState194 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69 * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState191 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv71 * _menhir_state) * _menhir_state * 'tv_symbol_bindings)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState184 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * 'tv_code_or_closure_binding)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState180 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state * 'tv_with_closure_elements_opt)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState172 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * (
# 115 "flambda_parser.mly"
      (Fexpr.named)
# 4440 "flambda_parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * 'tv_simple) * 'tv_infix_binop) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState166 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv81 * _menhir_state * 'tv_simple))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState158 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state * 'tv_unop) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv85 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv87 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state) * _menhir_state * 'tv_name) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv93 * _menhir_state) * _menhir_state * 'tv_tag)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState143 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv97 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv99 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv101 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv103 * _menhir_state)) * 'tv_csymbol)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState130 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv105 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv109 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((((('freshtv113 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv115 * _menhir_state * 'tv_kind)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState119 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((('freshtv119 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv121 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv123 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125 * _menhir_state * 'tv_kinded_var_within_closure) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv128)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv129 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) * _menhir_state * 'tv_kinded_args) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv131 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv134)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv136)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv137 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv141 * _menhir_state) * _menhir_state * 'tv_symbol)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv143 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv145 * _menhir_state * 'tv_closure_element)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv147 * _menhir_state * 'tv_var_within_closure)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv149 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv151 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv153 * _menhir_state * (
# 116 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 4631 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv155 * _menhir_state * 'tv_symbol)) * _menhir_state) * _menhir_state * 'tv_tag)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv157 * _menhir_state * 'tv_symbol)) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv161 * _menhir_state) * _menhir_state * 'tv_symbol)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv165 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv167 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_kinded_args)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169 * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv173 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175 * _menhir_state * 'tv_exn_and_stub) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv177 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv178)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv183 * _menhir_state * 'tv_switch_case)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv185 * _menhir_state * 'tv_tag)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv187 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv189 * _menhir_state) * 'tv_switch_sort) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv191 * _menhir_state * 'tv_tag_to_size)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv193)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv194)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv195 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv197 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv199 * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv201) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv202)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 82 "flambda_parser.mly"
       (string)
# 4757 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv49) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 82 "flambda_parser.mly"
       (string)
# 4768 "flambda_parser.ml"
    )) : (
# 82 "flambda_parser.mly"
       (string)
# 4772 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_continuation = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 458 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 4780 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv47) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_continuation) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3 * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | APPLY | BLOCK | CCALL | CLOSURE | CONT | FLOAT _ | HCF | INT _ | LET | LETK | LIDENT _ | OPAQUE | PROJECT_VAR | SWITCH | UIDENT _ | UNREACHABLE ->
            _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv4)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv21 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv19 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (cont : 'tv_continuation)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_exn_continuation = 
# 128 "flambda_parser.mly"
                             ( cont )
# 4812 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_exn_continuation) = _v in
        ((match _menhir_s with
        | MenhirState143 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv7 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_exn_continuation) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv5 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((e : 'tv_exn_continuation) : 'tv_exn_continuation) = _v in
            ((let (((((_menhir_stack, _menhir_s), (func : 'tv_csymbol)), _, (args : 'tv_simple_args)), _, (ra : 'tv_return_arity)), _, (r : 'tv_continuation)) = _menhir_stack in
            let _7 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4836 "flambda_parser.ml"
            ) = 
# 307 "flambda_parser.mly"
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
# 4850 "flambda_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv6)) : 'freshtv8)
        | MenhirState156 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv11 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_exn_continuation) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv9 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((e : 'tv_exn_continuation) : 'tv_exn_continuation) = _v in
            ((let ((((_menhir_stack, _menhir_s), _, (func : 'tv_name)), _, (args : 'tv_simple_args)), _, (r : 'tv_continuation)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : (
# 113 "flambda_parser.mly"
      (Fexpr.expr)
# 4868 "flambda_parser.ml"
            ) = 
# 320 "flambda_parser.mly"
     ( Apply {
          func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind = Function Indirect_unknown_arity;
       })
# 4878 "flambda_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv10)) : 'freshtv12)
        | MenhirState3 | MenhirState117 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv15) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_exn_continuation) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv13) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((x : 'tv_exn_continuation) : 'tv_exn_continuation) = _v in
            ((let _v : 'tv_option_exn_continuation_ = 
# 116 "<standard.mly>"
    ( Some x )
# 4893 "flambda_parser.ml"
             in
            _menhir_goto_option_exn_continuation_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv14)) : 'freshtv16)
        | _ ->
            _menhir_fail ()) : 'freshtv18)) : 'freshtv20)) : 'freshtv22)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv35 * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv33 * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (i : 'tv_tag)), _, (c : 'tv_continuation)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_switch_case = 
# 270 "flambda_parser.mly"
                                          ( i,c )
# 4908 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_switch_case) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state * 'tv_switch_case) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_switch_case) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState35 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SEMICOLON ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | RBRACE ->
                _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35) : 'freshtv24)
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv25 * _menhir_state * 'tv_switch_case) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (c : 'tv_switch_case)) = _menhir_stack in
            let _v : 'tv_switch = 
# 275 "flambda_parser.mly"
                    ( [c] )
# 4943 "flambda_parser.ml"
             in
            _menhir_goto_switch _menhir_env _menhir_stack _menhir_s _v) : 'freshtv26)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv27 * _menhir_state * 'tv_switch_case) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)) : 'freshtv30)) : 'freshtv32)) : 'freshtv34)) : 'freshtv36)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LBRACE ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53) : 'freshtv38)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv39 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_0_) * 'tv_option___anonymous_1_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | COLON | EQUAL ->
            _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117) : 'freshtv40)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | AND | EOF | IN | RBRACE | WITH ->
            _menhir_reduce97 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128) : 'freshtv42)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv43 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143) : 'freshtv44)
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv45 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156) : 'freshtv46)
    | _ ->
        _menhir_fail ()) : 'freshtv48)) : 'freshtv50)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and flambda_unit : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 111 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 5037 "flambda_parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 464 "flambda_parser.mly"
  

# 5066 "flambda_parser.ml"

# 269 "<standard.mly>"
  

# 5071 "flambda_parser.ml"
