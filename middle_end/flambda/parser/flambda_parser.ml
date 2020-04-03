
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WITH
    | UNREACHABLE
    | UNDERSCORE
    | UIDENT of (
# 82 "flambda_parser.mly"
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
# 60 "flambda_parser.mly"
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
# 54 "flambda_parser.mly"
       (string * char option)
# 51 "flambda_parser.ml"
  )
    | IN
    | HCF
    | FLOAT of (
# 50 "flambda_parser.mly"
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
  | MenhirState221
  | MenhirState219
  | MenhirState218
  | MenhirState217
  | MenhirState212
  | MenhirState209
  | MenhirState203
  | MenhirState201
  | MenhirState199
  | MenhirState194
  | MenhirState191
  | MenhirState187
  | MenhirState181
  | MenhirState179
  | MenhirState177
  | MenhirState175
  | MenhirState170
  | MenhirState161
  | MenhirState158
  | MenhirState155
  | MenhirState147
  | MenhirState145
  | MenhirState144
  | MenhirState142
  | MenhirState139
  | MenhirState136
  | MenhirState134
  | MenhirState132
  | MenhirState131
  | MenhirState129
  | MenhirState128
  | MenhirState119
  | MenhirState118
  | MenhirState117
  | MenhirState116
  | MenhirState114
  | MenhirState111
  | MenhirState108
  | MenhirState107
  | MenhirState106
  | MenhirState105
  | MenhirState102
  | MenhirState98
  | MenhirState96
  | MenhirState95
  | MenhirState93
  | MenhirState89
  | MenhirState87
  | MenhirState86
  | MenhirState85
  | MenhirState83
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

# 202 "flambda_parser.ml"

let rec _menhir_goto_segments : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_segments -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv917) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_segments) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv915) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((segs : 'tv_segments) : 'tv_segments) = _v in
    ((let _v : 'tv_symbol_bindings = 
# 115 "flambda_parser.mly"
                    ( Segments segs )
# 217 "flambda_parser.ml"
     in
    _menhir_goto_symbol_bindings _menhir_env _menhir_stack _menhir_s _v) : 'freshtv916)) : 'freshtv918)

and _menhir_goto_separated_nonempty_list_AND___anonymous_0_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_AND___anonymous_0_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState175 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv909 * _menhir_state) * _menhir_state * 'tv_segment)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_AND___anonymous_0_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv907 * _menhir_state) * _menhir_state * 'tv_segment)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_AND___anonymous_0_) : 'tv_separated_nonempty_list_AND___anonymous_0_) = _v in
        ((let ((_menhir_stack, _menhir_s), _, (seg : 'tv_segment)) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_separated_nonempty_list_AND___anonymous_0_ = let x = 
# 120 "flambda_parser.mly"
                                                               ( seg )
# 239 "flambda_parser.ml"
         in
        
# 243 "<standard.mly>"
    ( x :: xs )
# 244 "flambda_parser.ml"
         in
        _menhir_goto_separated_nonempty_list_AND___anonymous_0_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv908)) : 'freshtv910)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv913) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_AND___anonymous_0_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv911) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((segs : 'tv_separated_nonempty_list_AND___anonymous_0_) : 'tv_separated_nonempty_list_AND___anonymous_0_) = _v in
        ((let _v : 'tv_segments = 
# 120 "flambda_parser.mly"
                                                                        ( segs )
# 259 "flambda_parser.ml"
         in
        _menhir_goto_segments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv912)) : 'freshtv914)
    | _ ->
        _menhir_fail ()

and _menhir_goto_with_closure_elements_opt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_with_closure_elements_opt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState177 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv899 * _menhir_state * 'tv_code_bindings_and_closures) * _menhir_state * 'tv_with_closure_elements_opt) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv897 * _menhir_state * 'tv_code_bindings_and_closures) * _menhir_state * 'tv_with_closure_elements_opt) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (cbcs : 'tv_code_bindings_and_closures)), _, (closure_elements : 'tv_with_closure_elements_opt)) = _menhir_stack in
        let _v : 'tv_segment = 
# 126 "flambda_parser.mly"
    ( let (code_bindings, closures) = cbcs in
      { code_bindings; closures; closure_elements } )
# 279 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv895) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_segment) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState82 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv889 * _menhir_state) * _menhir_state * 'tv_segment) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | AND ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv883 * _menhir_state) * _menhir_state * 'tv_segment) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | SEGMENT ->
                    _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175) : 'freshtv884)
            | IN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv885 * _menhir_state) * _menhir_state * 'tv_segment) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _, (seg : 'tv_segment)) = _menhir_stack in
                let _1 = () in
                let _v : 'tv_separated_nonempty_list_AND___anonymous_0_ = let x = 
# 120 "flambda_parser.mly"
                                                               ( seg )
# 313 "flambda_parser.ml"
                 in
                
# 241 "<standard.mly>"
    ( [ x ] )
# 318 "flambda_parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_AND___anonymous_0_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv886)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv887 * _menhir_state) * _menhir_state * 'tv_segment) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv888)) : 'freshtv890)
        | MenhirState62 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv893 * _menhir_state * 'tv_segment) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv891 * _menhir_state * 'tv_segment) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (seg : 'tv_segment)) = _menhir_stack in
            let _v : 'tv_segments = 
# 119 "flambda_parser.mly"
                  ( [ seg ] )
# 337 "flambda_parser.ml"
             in
            _menhir_goto_segments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv892)) : 'freshtv894)
        | _ ->
            _menhir_fail ()) : 'freshtv896)) : 'freshtv898)) : 'freshtv900)
    | MenhirState201 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv905 * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) * _menhir_state * 'tv_with_closure_elements_opt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv901 * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) * _menhir_state * 'tv_with_closure_elements_opt) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APPLY ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState203
            | BLOCK ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState203
            | CCALL ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState203
            | CLOSURE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState203
            | CONT ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState203
            | FLOAT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
            | HCF ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState203
            | INT _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState203
            | LETK ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState203
            | LIDENT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState203
            | PROJECT_VAR ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState203
            | SWITCH ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState203
            | UIDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState203
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState203) : 'freshtv902)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv903 * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) * _menhir_state * 'tv_with_closure_elements_opt) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv904)) : 'freshtv906)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_closure_element__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv881 * _menhir_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv877 * _menhir_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv875 * _menhir_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (xs : 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__)) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_with_closure_elements_opt = let elements = 
# 232 "<standard.mly>"
    ( xs )
# 421 "flambda_parser.ml"
         in
        
# 290 "flambda_parser.mly"
    ( Some elements )
# 426 "flambda_parser.ml"
         in
        _menhir_goto_with_closure_elements_opt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv876)) : 'freshtv878)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv879 * _menhir_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv880)) : 'freshtv882)

and _menhir_goto_symbol_bindings : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_symbol_bindings -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv873 * _menhir_state) * _menhir_state * 'tv_symbol_bindings) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv869 * _menhir_state) * _menhir_state * 'tv_symbol_bindings) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | APPLY ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | BLOCK ->
            _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | CCALL ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | CLOSURE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | CONT ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | FLOAT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v
        | HCF ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | INT _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState199 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | LETK ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | LIDENT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState199 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPAQUE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | PROJECT_VAR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | SWITCH ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | UIDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState199 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNREACHABLE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState199) : 'freshtv870)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv871 * _menhir_state) * _menhir_state * 'tv_symbol_bindings) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv872)) : 'freshtv874)

and _menhir_goto_kinds : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_kinds -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv863 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_kinds) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv861 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((k : 'tv_kinds) : 'tv_kinds) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_return_arity = 
# 231 "flambda_parser.mly"
                    ( Some k )
# 512 "flambda_parser.ml"
         in
        _menhir_goto_return_arity _menhir_env _menhir_stack _menhir_s _v) : 'freshtv862)) : 'freshtv864)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv867 * _menhir_state * 'tv_kind)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_kinds) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv865 * _menhir_state * 'tv_kind)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((t : 'tv_kinds) : 'tv_kinds) = _v in
        ((let (_menhir_stack, _menhir_s, (k : 'tv_kind)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_kinds = 
# 227 "flambda_parser.mly"
                             ( k :: t )
# 529 "flambda_parser.ml"
         in
        _menhir_goto_kinds _menhir_env _menhir_stack _menhir_s _v) : 'freshtv866)) : 'freshtv868)
    | _ ->
        _menhir_fail ()

and _menhir_goto_tags_to_sizes : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_tags_to_sizes -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv855)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv851)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv849)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, (tags_to_sizes : 'tv_tags_to_sizes)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_switch_sort = 
# 184 "flambda_parser.mly"
                                                        ( Tag { tags_to_sizes } )
# 558 "flambda_parser.ml"
             in
            _menhir_goto_switch_sort _menhir_env _menhir_stack _v) : 'freshtv850)) : 'freshtv852)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv853)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv854)) : 'freshtv856)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv859 * _menhir_state * 'tv_tag_to_size)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv857 * _menhir_state * 'tv_tag_to_size)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (tag_to_size : 'tv_tag_to_size)), _, (tags_to_sizes : 'tv_tags_to_sizes)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_tags_to_sizes = 
# 181 "flambda_parser.mly"
                                                                  ( tag_to_size :: tags_to_sizes )
# 578 "flambda_parser.ml"
         in
        _menhir_goto_tags_to_sizes _menhir_env _menhir_stack _menhir_s _v) : 'freshtv858)) : 'freshtv860)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_kinded_var_within_closure_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_kinded_var_within_closure_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv843) * _menhir_state * 'tv_list_kinded_var_within_closure_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RANGLE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv839) * _menhir_state * 'tv_list_kinded_var_within_closure_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv837) * _menhir_state * 'tv_list_kinded_var_within_closure_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, (v : 'tv_list_kinded_var_within_closure_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_kinded_vars_within_closures = 
# 325 "flambda_parser.mly"
                                                 ( v )
# 606 "flambda_parser.ml"
             in
            _menhir_goto_kinded_vars_within_closures _menhir_env _menhir_stack _v) : 'freshtv838)) : 'freshtv840)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv841) * _menhir_state * 'tv_list_kinded_var_within_closure_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv842)) : 'freshtv844)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv847 * _menhir_state * 'tv_kinded_var_within_closure) * _menhir_state * 'tv_list_kinded_var_within_closure_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv845 * _menhir_state * 'tv_kinded_var_within_closure) * _menhir_state * 'tv_list_kinded_var_within_closure_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_kinded_var_within_closure)), _, (xs : 'tv_list_kinded_var_within_closure_)) = _menhir_stack in
        let _v : 'tv_list_kinded_var_within_closure_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 625 "flambda_parser.ml"
         in
        _menhir_goto_list_kinded_var_within_closure_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv846)) : 'freshtv848)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_AND_let_binding_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_AND_let_binding_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv831 * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | WITH ->
            _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | IN ->
            _menhir_reduce127 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState201) : 'freshtv832)
    | MenhirState209 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv835 * _menhir_state * 'tv_let_binding)) * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv833 * _menhir_state * 'tv_let_binding)) * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_let_binding)), _, (xs : 'tv_separated_nonempty_list_AND_let_binding_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_AND_let_binding_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 659 "flambda_parser.ml"
         in
        _menhir_goto_separated_nonempty_list_AND_let_binding_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv834)) : 'freshtv836)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_closure_element_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_closure_element_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState179 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv825) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_closure_element_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv823) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_SEMICOLON_closure_element_) : 'tv_separated_nonempty_list_SEMICOLON_closure_element_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__ = 
# 144 "<standard.mly>"
    ( x )
# 680 "flambda_parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_closure_element__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv824)) : 'freshtv826)
    | MenhirState187 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv829 * _menhir_state * 'tv_closure_element)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_closure_element_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv827 * _menhir_state * 'tv_closure_element)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_SEMICOLON_closure_element_) : 'tv_separated_nonempty_list_SEMICOLON_closure_element_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_closure_element)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_closure_element_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 697 "flambda_parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_closure_element_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv828)) : 'freshtv830)
    | _ ->
        _menhir_fail ()

and _menhir_goto_infix_binop : _menhir_env -> 'ttv_tail -> 'tv_infix_binop -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv821 * _menhir_state * 'tv_simple) * 'tv_infix_binop) = Obj.magic _menhir_stack in
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158) : 'freshtv822)

and _menhir_goto_variable_opt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_variable_opt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv819) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_variable_opt) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv817) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((v : 'tv_variable_opt) : 'tv_variable_opt) = _v in
    ((let _v : 'tv_kinded_variable_opt = 
# 348 "flambda_parser.mly"
                     ( v, None )
# 737 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv815) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_kinded_variable_opt) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv813 * _menhir_state * 'tv_kinded_variable_opt) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv809 * _menhir_state * 'tv_kinded_variable_opt) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BLOCK ->
            _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | CLOSURE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | FLOAT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _v
        | INT _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState212 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LIDENT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState212 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPAQUE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | PROJECT_VAR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | UIDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState212 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState212) : 'freshtv810)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv811 * _menhir_state * 'tv_kinded_variable_opt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv812)) : 'freshtv814)) : 'freshtv816)) : 'freshtv818)) : 'freshtv820)

and _menhir_goto_exn_and_stub : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_exn_and_stub -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv807 * _menhir_state * 'tv_exn_and_stub) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv808)

and _menhir_reduce127 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_with_closure_elements_opt = 
# 286 "flambda_parser.mly"
    ( None )
# 803 "flambda_parser.ml"
     in
    _menhir_goto_with_closure_elements_opt _menhir_env _menhir_stack _menhir_s _v

and _menhir_run178 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv803 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState179 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv801) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState179 in
            ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__ = 
# 142 "<standard.mly>"
    ( [] )
# 828 "flambda_parser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_SEMICOLON_closure_element__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv802)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179) : 'freshtv804)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv805 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv806)

and _menhir_goto_recursive : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_recursive -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv797 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EXN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | STUB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LIDENT _ ->
            _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47) : 'freshtv798)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv799 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87) : 'freshtv800)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_kinded_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_kinded_variable_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv791 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv787 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv785 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (v : 'tv_list_kinded_variable_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_kinded_args = 
# 321 "flambda_parser.mly"
                                       ( v )
# 900 "flambda_parser.ml"
             in
            _menhir_goto_kinded_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv786)) : 'freshtv788)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv789 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv790)) : 'freshtv792)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv795 * _menhir_state * 'tv_kinded_variable) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv793 * _menhir_state * 'tv_kinded_variable) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_kinded_variable)), _, (xs : 'tv_list_kinded_variable_)) = _menhir_stack in
        let _v : 'tv_list_kinded_variable_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 919 "flambda_parser.ml"
         in
        _menhir_goto_list_kinded_variable_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv794)) : 'freshtv796)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_simple_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_simple_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState119 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv767 * _menhir_state * 'tv_simple) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv765 * _menhir_state * 'tv_simple) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_simple)), _, (xs : 'tv_list_simple_)) = _menhir_stack in
        let _v : 'tv_list_simple_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 938 "flambda_parser.ml"
         in
        _menhir_goto_list_simple_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv766)) : 'freshtv768)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv775 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv771 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv769 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (s : 'tv_list_simple_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_simple_args = 
# 357 "flambda_parser.mly"
                              ( s )
# 959 "flambda_parser.ml"
             in
            _menhir_goto_simple_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv770)) : 'freshtv772)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv773 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv774)) : 'freshtv776)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv783 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv779 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv777 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (t : 'tv_tag)), _, (elts : 'tv_list_simple_)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (
# 93 "flambda_parser.mly"
      (Fexpr.named)
# 988 "flambda_parser.ml"
            ) = 
# 209 "flambda_parser.mly"
                                               ( Prim (Block (t, Immutable, elts)) )
# 992 "flambda_parser.ml"
             in
            _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv778)) : 'freshtv780)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv781 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv782)) : 'freshtv784)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_of_kind_value_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_of_kind_value_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv749 * _menhir_state * (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 1014 "flambda_parser.ml"
        )) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv747 * _menhir_state * (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 1020 "flambda_parser.ml"
        )) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 1025 "flambda_parser.ml"
        ))), _, (xs : 'tv_list_of_kind_value_)) = _menhir_stack in
        let _v : 'tv_list_of_kind_value_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1030 "flambda_parser.ml"
         in
        _menhir_goto_list_of_kind_value_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv748)) : 'freshtv750)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv763 * _menhir_state * 'tv_symbol)) * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv759 * _menhir_state * 'tv_symbol)) * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv757 * _menhir_state * 'tv_symbol)) * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s, (s : 'tv_symbol)), _), _, (t : 'tv_tag)), _, (elts : 'tv_list_of_kind_value_)) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _v : (
# 90 "flambda_parser.mly"
      (Fexpr.static_structure)
# 1053 "flambda_parser.ml"
            ) = 
# 330 "flambda_parser.mly"
    ( ( s, None, Block (t, Immutable, elts) ) )
# 1057 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv755) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 90 "flambda_parser.mly"
      (Fexpr.static_structure)
# 1065 "flambda_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv753 * _menhir_state) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 90 "flambda_parser.mly"
      (Fexpr.static_structure)
# 1073 "flambda_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv751 * _menhir_state) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((s : (
# 90 "flambda_parser.mly"
      (Fexpr.static_structure)
# 1081 "flambda_parser.ml"
            )) : (
# 90 "flambda_parser.mly"
      (Fexpr.static_structure)
# 1085 "flambda_parser.ml"
            )) = _v in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_symbol_bindings = 
# 114 "flambda_parser.mly"
                                ( Simple s )
# 1092 "flambda_parser.ml"
             in
            _menhir_goto_symbol_bindings _menhir_env _menhir_stack _menhir_s _v) : 'freshtv752)) : 'freshtv754)) : 'freshtv756)) : 'freshtv758)) : 'freshtv760)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv761 * _menhir_state * 'tv_symbol)) * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv762)) : 'freshtv764)
    | _ ->
        _menhir_fail ()

and _menhir_goto_return_arity : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_return_arity -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((((('freshtv739 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((((('freshtv735 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APPLY ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | BLOCK ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | CCALL ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | CLOSURE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | CONT ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | FLOAT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | HCF ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | INT _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | LETK ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | LIDENT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | PROJECT_VAR ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | SWITCH ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | UIDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114) : 'freshtv736)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((((('freshtv737 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv738)) : 'freshtv740)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv745 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv741 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState131 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131) : 'freshtv742)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv743 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv744)) : 'freshtv746)
    | _ ->
        _menhir_fail ()

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_kind = 
# 223 "flambda_parser.mly"
    ( None )
# 1197 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv733) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_kind) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv731 * _menhir_state * 'tv_kind) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv727 * _menhir_state * 'tv_kind) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL | MINUSGREATER ->
            _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | COMMA ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111) : 'freshtv728)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv729 * _menhir_state * 'tv_kind) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv730)) : 'freshtv732)) : 'freshtv734)

and _menhir_reduce47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_kinds = 
# 226 "flambda_parser.mly"
    ( [] )
# 1236 "flambda_parser.ml"
     in
    _menhir_goto_kinds _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce118 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_tags_to_sizes = 
# 179 "flambda_parser.mly"
    ( [] )
# 1245 "flambda_parser.ml"
     in
    _menhir_goto_tags_to_sizes _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 54 "flambda_parser.mly"
       (string * char option)
# 1252 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv723 * Lexing.position * _menhir_state * (
# 54 "flambda_parser.mly"
       (string * char option)
# 1264 "flambda_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv719 * Lexing.position * _menhir_state * (
# 54 "flambda_parser.mly"
       (string * char option)
# 1274 "flambda_parser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 54 "flambda_parser.mly"
       (string * char option)
# 1280 "flambda_parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv717 * Lexing.position * _menhir_state * (
# 54 "flambda_parser.mly"
       (string * char option)
# 1288 "flambda_parser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            let (_endpos_size_ : Lexing.position) = _endpos in
            let ((size : (
# 54 "flambda_parser.mly"
       (string * char option)
# 1294 "flambda_parser.ml"
            )) : (
# 54 "flambda_parser.mly"
       (string * char option)
# 1298 "flambda_parser.ml"
            )) = _v in
            let (_startpos_size_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _endpos_tag_, _menhir_s, (tag : (
# 54 "flambda_parser.mly"
       (string * char option)
# 1304 "flambda_parser.ml"
            )), _startpos_tag_) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_tag_to_size = 
# 172 "flambda_parser.mly"
                               (
  let (tag, _) = tag in
  let (size, _) = size in
  int_of_string tag, int_of_string size
)
# 1314 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv715) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_tag_to_size) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv713 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COMMA ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv707 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | INT _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RBRACKET ->
                    _menhir_reduce118 _menhir_env (Obj.magic _menhir_stack) MenhirState18
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18) : 'freshtv708)
            | RBRACKET ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv709 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (tag_to_size : 'tv_tag_to_size)) = _menhir_stack in
                let _v : 'tv_tags_to_sizes = 
# 180 "flambda_parser.mly"
                              ( [ tag_to_size ] )
# 1347 "flambda_parser.ml"
                 in
                _menhir_goto_tags_to_sizes _menhir_env _menhir_stack _menhir_s _v) : 'freshtv710)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv711 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv712)) : 'freshtv714)) : 'freshtv716)) : 'freshtv718)) : 'freshtv720)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv721 * Lexing.position * _menhir_state * (
# 54 "flambda_parser.mly"
       (string * char option)
# 1364 "flambda_parser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv722)) : 'freshtv724)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv725 * Lexing.position * _menhir_state * (
# 54 "flambda_parser.mly"
       (string * char option)
# 1375 "flambda_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv726)

and _menhir_goto_switch_sort : _menhir_env -> 'ttv_tail -> 'tv_switch_sort -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv705 * _menhir_state) * 'tv_switch_sort) = Obj.magic _menhir_stack in
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv706)

and _menhir_goto_unop : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_unop -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv703 * _menhir_state * 'tv_unop) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
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
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147) : 'freshtv704)

and _menhir_goto_name : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_name -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv701 * _menhir_state) * _menhir_state * 'tv_name) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | MINUSGREATER ->
        _menhir_reduce101 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142) : 'freshtv702)

and _menhir_goto_kinded_vars_within_closures : _menhir_env -> 'ttv_tail -> 'tv_kinded_vars_within_closures -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((((((('freshtv699 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | MINUSGREATER ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv695 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105) : 'freshtv696)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv697 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv698)) : 'freshtv700)

and _menhir_reduce52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_kinded_var_within_closure_ = 
# 211 "<standard.mly>"
    ( [] )
# 1472 "flambda_parser.ml"
     in
    _menhir_goto_list_kinded_var_within_closure_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run42 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 60 "flambda_parser.mly"
       (string)
# 1479 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv693) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 60 "flambda_parser.mly"
       (string)
# 1490 "flambda_parser.ml"
    )) : (
# 60 "flambda_parser.mly"
       (string)
# 1494 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_var_within_closure = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 406 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 1502 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv691) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_var_within_closure) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv675 * _menhir_state) * _menhir_state * 'tv_var_within_closure) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv673 * _menhir_state) * _menhir_state * 'tv_var_within_closure) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (var : 'tv_var_within_closure)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_unop = 
# 191 "flambda_parser.mly"
                                           ( Project_var var )
# 1520 "flambda_parser.ml"
         in
        _menhir_goto_unop _menhir_env _menhir_stack _menhir_s _v) : 'freshtv674)) : 'freshtv676)
    | MenhirState102 | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv683 * _menhir_state * 'tv_var_within_closure) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv681 * _menhir_state * 'tv_var_within_closure) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (var : 'tv_var_within_closure)) = _menhir_stack in
        let _v : 'tv_kinded_var_within_closure = 
# 352 "flambda_parser.mly"
                             ( { var; kind = None } )
# 1532 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv679) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_kinded_var_within_closure) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv677 * _menhir_state * 'tv_kinded_var_within_closure) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RANGLE ->
            _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102) : 'freshtv678)) : 'freshtv680)) : 'freshtv682)) : 'freshtv684)
    | MenhirState187 | MenhirState179 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv689 * _menhir_state * 'tv_var_within_closure) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv685 * _menhir_state * 'tv_var_within_closure) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FLOAT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
            | INT _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState181 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIDENT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState181 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState181 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState181) : 'freshtv686)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv687 * _menhir_state * 'tv_var_within_closure) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv688)) : 'freshtv690)
    | _ ->
        _menhir_fail ()) : 'freshtv692)) : 'freshtv694)

and _menhir_goto_of_kind_value : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 1589 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv671 * _menhir_state * (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 1597 "flambda_parser.ml"
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
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv672)

and _menhir_goto_option___anonymous_2_ : _menhir_env -> 'ttv_tail -> 'tv_option___anonymous_2_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((('freshtv669 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | LIDENT _ ->
        _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv670)

and _menhir_goto_option___anonymous_1_ : _menhir_env -> 'ttv_tail -> 'tv_option___anonymous_1_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv667 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NEWER_VERSION_OF ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv661) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93) : 'freshtv662)
    | LIDENT _ | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv663) = Obj.magic _menhir_stack in
        ((let _v : 'tv_option___anonymous_2_ = 
# 114 "<standard.mly>"
    ( None )
# 1658 "flambda_parser.ml"
         in
        _menhir_goto_option___anonymous_2_ _menhir_env _menhir_stack _v) : 'freshtv664)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv665 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv666)) : 'freshtv668)

and _menhir_goto_named : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 93 "flambda_parser.mly"
      (Fexpr.named)
# 1672 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 | MenhirState221 | MenhirState61 | MenhirState203 | MenhirState199 | MenhirState161 | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv645 * _menhir_state * (
# 93 "flambda_parser.mly"
      (Fexpr.named)
# 1682 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv641 * _menhir_state * (
# 93 "flambda_parser.mly"
      (Fexpr.named)
# 1692 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APPLY ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | BLOCK ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | CCALL ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | CLOSURE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | CONT ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | FLOAT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
            | HCF ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | INT _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | LETK ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | LIDENT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | PROJECT_VAR ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | SWITCH ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | UIDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161) : 'freshtv642)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv643 * _menhir_state * (
# 93 "flambda_parser.mly"
      (Fexpr.named)
# 1740 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv644)) : 'freshtv646)
    | MenhirState212 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv659 * _menhir_state * 'tv_kinded_variable_opt)) * _menhir_state * (
# 93 "flambda_parser.mly"
      (Fexpr.named)
# 1749 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv657 * _menhir_state * 'tv_kinded_variable_opt)) * _menhir_state * (
# 93 "flambda_parser.mly"
      (Fexpr.named)
# 1755 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (v : 'tv_kinded_variable_opt)), _, (defining_expr : (
# 93 "flambda_parser.mly"
      (Fexpr.named)
# 1760 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_let_binding = 
# 282 "flambda_parser.mly"
      ( let (var, kind) = v in { var; kind; defining_expr } )
# 1766 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv655) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_let_binding) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv653 * _menhir_state * 'tv_let_binding) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv647 * _menhir_state * 'tv_let_binding) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run196 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState209 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNDERSCORE ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState209) : 'freshtv648)
        | IN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv649 * _menhir_state * 'tv_let_binding) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_let_binding)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_AND_let_binding_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1799 "flambda_parser.ml"
             in
            _menhir_goto_separated_nonempty_list_AND_let_binding_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv650)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv651 * _menhir_state * 'tv_let_binding) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv652)) : 'freshtv654)) : 'freshtv656)) : 'freshtv658)) : 'freshtv660)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_AND_static_closure_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_AND_static_closure_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState170 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv625 * _menhir_state * 'tv_static_closure)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_AND_static_closure_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv623 * _menhir_state * 'tv_static_closure)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_AND_static_closure_) : 'tv_separated_nonempty_list_AND_static_closure_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_static_closure)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_AND_static_closure_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1829 "flambda_parser.ml"
         in
        _menhir_goto_separated_nonempty_list_AND_static_closure_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv624)) : 'freshtv626)
    | MenhirState62 | MenhirState191 | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv639) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_AND_static_closure_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv637) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((scs : 'tv_separated_nonempty_list_AND_static_closure_) : 'tv_separated_nonempty_list_AND_static_closure_) = _v in
        ((let _v : 'tv_static_closures = 
# 159 "flambda_parser.mly"
                                                       ( scs )
# 1844 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv635) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_static_closures) = _v in
        ((match _menhir_s with
        | MenhirState62 | MenhirState82 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv629) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_static_closures) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv627) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((closures : 'tv_static_closures) : 'tv_static_closures) = _v in
            ((let _v : 'tv_code_bindings_and_closures = 
# 132 "flambda_parser.mly"
                               ( [], closures )
# 1863 "flambda_parser.ml"
             in
            _menhir_goto_code_bindings_and_closures _menhir_env _menhir_stack _menhir_s _v) : 'freshtv628)) : 'freshtv630)
        | MenhirState191 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv633 * _menhir_state * 'tv_code_bindings)) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_static_closures) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv631 * _menhir_state * 'tv_code_bindings)) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((closures : 'tv_static_closures) : 'tv_static_closures) = _v in
            ((let (_menhir_stack, _menhir_s, (cbs : 'tv_code_bindings)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_code_bindings_and_closures = 
# 133 "flambda_parser.mly"
                                                         ( cbs, closures; )
# 1880 "flambda_parser.ml"
             in
            _menhir_goto_code_bindings_and_closures _menhir_env _menhir_stack _menhir_s _v) : 'freshtv632)) : 'freshtv634)
        | _ ->
            _menhir_fail ()) : 'freshtv636)) : 'freshtv638)) : 'freshtv640)
    | _ ->
        _menhir_fail ()

and _menhir_goto_simple : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simple -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv555 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv551 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SEMICOLON ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | RBRACE ->
                _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28) : 'freshtv552)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv553 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv554)) : 'freshtv556)
    | MenhirState136 | MenhirState119 | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv557 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FLOAT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
        | INT _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LIDENT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAREN ->
            _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119) : 'freshtv558)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv561 * _menhir_state * 'tv_unop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv559 * _menhir_state * 'tv_unop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (u : 'tv_unop)), _, (a : 'tv_simple)) = _menhir_stack in
        let _v : (
# 93 "flambda_parser.mly"
      (Fexpr.named)
# 1950 "flambda_parser.ml"
        ) = 
# 206 "flambda_parser.mly"
                        ( Prim (Unop (u, a)) )
# 1954 "flambda_parser.ml"
         in
        _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv560)) : 'freshtv562)
    | MenhirState6 | MenhirState221 | MenhirState61 | MenhirState212 | MenhirState203 | MenhirState199 | MenhirState161 | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv589 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv567 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv563 * _menhir_state * 'tv_simple)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | FLOAT _v ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
                | INT _v ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LIDENT _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UIDENT _v ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155) : 'freshtv564)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv565 * _menhir_state * 'tv_simple)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv566)) : 'freshtv568)
        | MINUS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv571) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv569) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_infix_binop = 
# 196 "flambda_parser.mly"
          ( Minus )
# 2004 "flambda_parser.ml"
             in
            _menhir_goto_infix_binop _menhir_env _menhir_stack _v) : 'freshtv570)) : 'freshtv572)
        | MINUSDOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv575) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv573) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_infix_binop = 
# 197 "flambda_parser.mly"
             ( Minusdot )
# 2017 "flambda_parser.ml"
             in
            _menhir_goto_infix_binop _menhir_env _menhir_stack _v) : 'freshtv574)) : 'freshtv576)
        | PLUS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv579) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv577) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_infix_binop = 
# 194 "flambda_parser.mly"
         ( Plus )
# 2030 "flambda_parser.ml"
             in
            _menhir_goto_infix_binop _menhir_env _menhir_stack _v) : 'freshtv578)) : 'freshtv580)
        | PLUSDOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv583) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv581) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_infix_binop = 
# 195 "flambda_parser.mly"
            ( Plusdot )
# 2043 "flambda_parser.ml"
             in
            _menhir_goto_infix_binop _menhir_env _menhir_stack _v) : 'freshtv582)) : 'freshtv584)
        | AND | IN | SEMICOLON | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv585 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (s : 'tv_simple)) = _menhir_stack in
            let _v : (
# 93 "flambda_parser.mly"
      (Fexpr.named)
# 2053 "flambda_parser.ml"
            ) = 
# 205 "flambda_parser.mly"
               ( Simple s )
# 2057 "flambda_parser.ml"
             in
            _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv586)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv587 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv588)) : 'freshtv590)
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv603 * _menhir_state * 'tv_simple))) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv599 * _menhir_state * 'tv_simple))) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv597 * _menhir_state * 'tv_simple))) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (a : 'tv_simple)), _, (f : 'tv_simple)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _v : 'tv_binop = 
# 202 "flambda_parser.mly"
    ( Binop (Block_load (Block Value, Immutable), a, f) )
# 2086 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv595) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_binop) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv593) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_binop) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv591) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((b : 'tv_binop) : 'tv_binop) = _v in
            ((let _v : (
# 93 "flambda_parser.mly"
      (Fexpr.named)
# 2103 "flambda_parser.ml"
            ) = 
# 208 "flambda_parser.mly"
              ( Prim b )
# 2107 "flambda_parser.ml"
             in
            _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv592)) : 'freshtv594)) : 'freshtv596)) : 'freshtv598)) : 'freshtv600)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv601 * _menhir_state * 'tv_simple))) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv602)) : 'freshtv604)
    | MenhirState158 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv607 * _menhir_state * 'tv_simple) * 'tv_infix_binop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv605 * _menhir_state * 'tv_simple) * 'tv_infix_binop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (a1 : 'tv_simple)), (b : 'tv_infix_binop)), _, (a2 : 'tv_simple)) = _menhir_stack in
        let _v : (
# 93 "flambda_parser.mly"
      (Fexpr.named)
# 2126 "flambda_parser.ml"
        ) = 
# 207 "flambda_parser.mly"
                                            ( Prim (Infix_binop (b, a1, a2)) )
# 2130 "flambda_parser.ml"
         in
        _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv606)) : 'freshtv608)
    | MenhirState181 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv621 * _menhir_state * 'tv_var_within_closure)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv619 * _menhir_state * 'tv_var_within_closure)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (var : 'tv_var_within_closure)), _, (value : 'tv_simple)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_closure_element = 
# 294 "flambda_parser.mly"
                                                     ( { var; value; } )
# 2143 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv617) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_closure_element) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv615 * _menhir_state * 'tv_closure_element) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv609 * _menhir_state * 'tv_closure_element) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187) : 'freshtv610)
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv611 * _menhir_state * 'tv_closure_element) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_closure_element)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_closure_element_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2174 "flambda_parser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_closure_element_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv612)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv613 * _menhir_state * 'tv_closure_element) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv614)) : 'freshtv616)) : 'freshtv618)) : 'freshtv620)) : 'freshtv622)
    | _ ->
        _menhir_fail ()

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv549) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_variable_opt = 
# 397 "flambda_parser.mly"
               ( None )
# 2197 "flambda_parser.ml"
     in
    _menhir_goto_variable_opt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv550)

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CODE ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | SYMBOL ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_run196 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 60 "flambda_parser.mly"
       (string)
# 2219 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv547) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 60 "flambda_parser.mly"
       (string)
# 2230 "flambda_parser.ml"
    )) : (
# 60 "flambda_parser.mly"
       (string)
# 2234 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_variable_opt = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 398 "flambda_parser.mly"
               ( Some (e, make_loc (_startpos, _endpos)) )
# 2242 "flambda_parser.ml"
     in
    _menhir_goto_variable_opt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv548)

and _menhir_goto_const : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_const -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv545) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_const) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv543) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : 'tv_const) : 'tv_const) = _v in
    ((let _v : 'tv_simple = 
# 373 "flambda_parser.mly"
              ( Const c )
# 2259 "flambda_parser.ml"
     in
    _menhir_goto_simple _menhir_env _menhir_stack _menhir_s _v) : 'freshtv544)) : 'freshtv546)

and _menhir_goto_andk : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_andk -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState219 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv539 * _menhir_state) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv537 * _menhir_state) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (h : 'tv_continuation_handler)), _, (t : 'tv_andk)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_andk = 
# 317 "flambda_parser.mly"
                                          ( h :: t )
# 2277 "flambda_parser.ml"
         in
        _menhir_goto_andk _menhir_env _menhir_stack _menhir_s _v) : 'freshtv538)) : 'freshtv540)
    | MenhirState217 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv541 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | APPLY ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState221
        | BLOCK ->
            _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState221
        | CCALL ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState221
        | CLOSURE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState221
        | CONT ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState221
        | FLOAT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState221 _v
        | HCF ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState221
        | INT _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState221 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState221
        | LETK ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState221
        | LIDENT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState221 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPAQUE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState221
        | PROJECT_VAR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState221
        | SWITCH ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState221
        | UIDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState221 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNREACHABLE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState221
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState221) : 'freshtv542)
    | _ ->
        _menhir_fail ()

and _menhir_reduce18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_exn_and_stub = 
# 302 "flambda_parser.mly"
    ( false, false )
# 2330 "flambda_parser.ml"
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
        let (_menhir_stack : 'freshtv531 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv529 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 305 "flambda_parser.mly"
             ( true, true )
# 2352 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv530)) : 'freshtv532)
    | LIDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv533 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 303 "flambda_parser.mly"
         ( false, true )
# 2363 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv534)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv535 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv536)

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STUB ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv523 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv521 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 306 "flambda_parser.mly"
             ( true, true )
# 2392 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv522)) : 'freshtv524)
    | LIDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv525 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 304 "flambda_parser.mly"
        ( true, false )
# 2403 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv526)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv527 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv528)

and _menhir_goto_code_bindings_and_closures : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_code_bindings_and_closures -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv519 * _menhir_state * 'tv_code_bindings_and_closures) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | WITH ->
        _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | AND | IN ->
        _menhir_reduce127 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177) : 'freshtv520)

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | UIDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_reduce81 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_recursive = 
# 167 "flambda_parser.mly"
    ( Nonrecursive )
# 2449 "flambda_parser.ml"
     in
    _menhir_goto_recursive _menhir_env _menhir_stack _menhir_s _v

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv517) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_recursive = 
# 168 "flambda_parser.mly"
        ( Recursive )
# 2463 "flambda_parser.ml"
     in
    _menhir_goto_recursive _menhir_env _menhir_stack _menhir_s _v) : 'freshtv518)

and _menhir_goto_simple_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simple_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv507 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv505 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (c : 'tv_continuation)), _, (s : 'tv_simple_args)) = _menhir_stack in
        let _1 = () in
        let _v : (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 2481 "flambda_parser.ml"
        ) = 
# 237 "flambda_parser.mly"
                                          ( Apply_cont (c, None, s) )
# 2485 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv506)) : 'freshtv508)
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv509 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | MINUSGREATER ->
            _menhir_reduce83 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129) : 'freshtv510)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv515 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv511 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144) : 'freshtv512)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv513 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv514)) : 'freshtv516)
    | _ ->
        _menhir_fail ()

and _menhir_goto_kinded_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_kinded_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv501 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_kinded_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv497 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_kinded_args) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APPLY ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | BLOCK ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | CCALL ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | CLOSURE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | CONT ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | FLOAT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | HCF ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState61
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61) : 'freshtv498)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv499 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_kinded_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv500)) : 'freshtv502)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv503 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) * _menhir_state * 'tv_kinded_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv504)
    | _ ->
        _menhir_fail ()

and _menhir_reduce54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_kinded_variable_ = 
# 211 "<standard.mly>"
    ( [] )
# 2609 "flambda_parser.ml"
     in
    _menhir_goto_list_kinded_variable_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_SEMICOLON_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_SEMICOLON_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv495) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_option_SEMICOLON_) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv493) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : 'tv_option_SEMICOLON_) : 'tv_option_SEMICOLON_) = _v in
    ((let _v : 'tv_switch = 
# 218 "flambda_parser.mly"
                      ( [] )
# 2626 "flambda_parser.ml"
     in
    _menhir_goto_switch _menhir_env _menhir_stack _menhir_s _v) : 'freshtv494)) : 'freshtv496)

and _menhir_reduce58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_simple_ = 
# 211 "<standard.mly>"
    ( [] )
# 2635 "flambda_parser.ml"
     in
    _menhir_goto_list_simple_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_of_kind_value_ = 
# 211 "<standard.mly>"
    ( [] )
# 2644 "flambda_parser.ml"
     in
    _menhir_goto_list_of_kind_value_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run73 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 54 "flambda_parser.mly"
       (string * char option)
# 2651 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv491) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 54 "flambda_parser.mly"
       (string * char option)
# 2662 "flambda_parser.ml"
    )) : (
# 54 "flambda_parser.mly"
       (string * char option)
# 2666 "flambda_parser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _v : (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 2672 "flambda_parser.ml"
    ) = let _endpos = _endpos_i_ in
    let _startpos = _startpos_i_ in
    
# 340 "flambda_parser.mly"
            ( Tagged_immediate ( make_tagged_immediate ~loc:(_startpos, _endpos) i ) )
# 2678 "flambda_parser.ml"
     in
    _menhir_goto_of_kind_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv492)

and _menhir_reduce83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_return_arity = 
# 230 "flambda_parser.mly"
    ( None )
# 2687 "flambda_parser.ml"
     in
    _menhir_goto_return_arity _menhir_env _menhir_stack _menhir_s _v

and _menhir_run108 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL | MINUSGREATER ->
        _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | COMMA ->
        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv489) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 2716 "flambda_parser.ml"
    ) = 
# 236 "flambda_parser.mly"
                ( Invalid Treat_as_unreachable )
# 2720 "flambda_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv490)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 82 "flambda_parser.mly"
       (string)
# 2727 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv487) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 82 "flambda_parser.mly"
       (string)
# 2738 "flambda_parser.ml"
    )) : (
# 82 "flambda_parser.mly"
       (string)
# 2742 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_symbol = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 385 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 2750 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv485) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_symbol) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 | MenhirState221 | MenhirState61 | MenhirState212 | MenhirState203 | MenhirState199 | MenhirState181 | MenhirState161 | MenhirState158 | MenhirState155 | MenhirState114 | MenhirState147 | MenhirState136 | MenhirState119 | MenhirState118 | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv461 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv459 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (s : 'tv_symbol)) = _menhir_stack in
        let _v : 'tv_simple = 
# 371 "flambda_parser.mly"
               ( Symbol s )
# 2767 "flambda_parser.ml"
         in
        _menhir_goto_simple _menhir_env _menhir_stack _menhir_s _v) : 'freshtv460)) : 'freshtv462)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv469 * _menhir_state) * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv465 * _menhir_state) * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BLOCK ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv463 * _menhir_state * 'tv_symbol)) = Obj.magic _menhir_stack in
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
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv464)
            | CLOSURE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66) : 'freshtv466)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv467 * _menhir_state) * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv468)) : 'freshtv470)
    | MenhirState76 | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv473 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv471 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (s : 'tv_symbol)) = _menhir_stack in
        let _v : (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 2818 "flambda_parser.ml"
        ) = 
# 338 "flambda_parser.mly"
               ( Symbol s )
# 2822 "flambda_parser.ml"
         in
        _menhir_goto_of_kind_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv472)) : 'freshtv474)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv479 * _menhir_state) * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv475 * _menhir_state) * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CLOSURE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85) : 'freshtv476)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv477 * _menhir_state) * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv478)) : 'freshtv480)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv483 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv481 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (s : 'tv_symbol)) = _menhir_stack in
        let _v : 'tv_name = 
# 366 "flambda_parser.mly"
               ( (Symbol s:name) )
# 2859 "flambda_parser.ml"
         in
        _menhir_goto_name _menhir_env _menhir_stack _menhir_s _v) : 'freshtv482)) : 'freshtv484)
    | _ ->
        _menhir_fail ()) : 'freshtv486)) : 'freshtv488)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IS_INT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv447) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv445) = Obj.magic _menhir_stack in
        ((let _1 = () in
        let _v : 'tv_switch_sort = 
# 186 "flambda_parser.mly"
           ( Is_int )
# 2881 "flambda_parser.ml"
         in
        _menhir_goto_switch_sort _menhir_env _menhir_stack _v) : 'freshtv446)) : 'freshtv448)
    | TAG ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv453) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv449) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RBRACKET ->
                _menhir_reduce118 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11) : 'freshtv450)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv451) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv452)) : 'freshtv454)
    | FLOAT _ | INT _ | LIDENT _ | UIDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv455) = Obj.magic _menhir_stack in
        ((let _v : 'tv_switch_sort = 
# 185 "flambda_parser.mly"
    ( Int )
# 2916 "flambda_parser.ml"
         in
        _menhir_goto_switch_sort _menhir_env _menhir_stack _v) : 'freshtv456)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv457 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv458)

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
    let (_menhir_stack : 'freshtv443) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_unop = 
# 190 "flambda_parser.mly"
           ( Opaque_identity )
# 2950 "flambda_parser.ml"
     in
    _menhir_goto_unop _menhir_env _menhir_stack _menhir_s _v) : 'freshtv444)

and _menhir_run22 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 60 "flambda_parser.mly"
       (string)
# 2957 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv441) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 60 "flambda_parser.mly"
       (string)
# 2968 "flambda_parser.ml"
    )) : (
# 60 "flambda_parser.mly"
       (string)
# 2972 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_variable = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 393 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 2980 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv439) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_variable) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 | MenhirState221 | MenhirState61 | MenhirState212 | MenhirState203 | MenhirState199 | MenhirState181 | MenhirState161 | MenhirState158 | MenhirState155 | MenhirState147 | MenhirState114 | MenhirState136 | MenhirState119 | MenhirState118 | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv361 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv359 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
        let _v : 'tv_simple = 
# 372 "flambda_parser.mly"
                 ( Var v )
# 2997 "flambda_parser.ml"
         in
        _menhir_goto_simple _menhir_env _menhir_stack _menhir_s _v) : 'freshtv360)) : 'freshtv362)
    | MenhirState58 | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv369 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv367 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (param : 'tv_variable)) = _menhir_stack in
        let _v : 'tv_kinded_variable = 
# 344 "flambda_parser.mly"
                     ( { param; kind = None } )
# 3009 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv365) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_kinded_variable) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv363 * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAREN ->
            _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv364)) : 'freshtv366)) : 'freshtv368)) : 'freshtv370)
    | MenhirState93 | MenhirState87 | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv411 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv409 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
        let _v : 'tv_code_id = 
# 377 "flambda_parser.mly"
                 ( v )
# 3038 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv407) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_code_id) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState67 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv393 * _menhir_state) * _menhir_state * 'tv_code_id) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv391 * _menhir_state) * _menhir_state * 'tv_code_id) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (code_id : 'tv_code_id)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_closure = 
# 298 "flambda_parser.mly"
                                ( code_id )
# 3056 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv389) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_closure) = _v in
            ((match _menhir_s with
            | MenhirState85 | MenhirState66 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv383 * _menhir_state) * _menhir_state * 'tv_symbol)) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_closure) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv381 * _menhir_state) * _menhir_state * 'tv_symbol)) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let ((clo : 'tv_closure) : 'tv_closure) = _v in
                ((let ((_menhir_stack, _menhir_s), _, (sym : 'tv_symbol)) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : 'tv_static_closure = 
# 163 "flambda_parser.mly"
                                                ( sym, clo )
# 3078 "flambda_parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv379) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_static_closure) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv377 * _menhir_state * 'tv_static_closure) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AND ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv371 * _menhir_state * 'tv_static_closure) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | SYMBOL ->
                        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170) : 'freshtv372)
                | IN | WITH ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv373 * _menhir_state * 'tv_static_closure) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, (x : 'tv_static_closure)) = _menhir_stack in
                    let _v : 'tv_separated_nonempty_list_AND_static_closure_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3109 "flambda_parser.ml"
                     in
                    _menhir_goto_separated_nonempty_list_AND_static_closure_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv374)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv375 * _menhir_state * 'tv_static_closure) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv376)) : 'freshtv378)) : 'freshtv380)) : 'freshtv382)) : 'freshtv384)
            | MenhirState6 | MenhirState221 | MenhirState61 | MenhirState212 | MenhirState203 | MenhirState199 | MenhirState114 | MenhirState161 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv387) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_closure) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv385) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((c : 'tv_closure) : 'tv_closure) = _v in
                ((let _v : (
# 93 "flambda_parser.mly"
      (Fexpr.named)
# 3131 "flambda_parser.ml"
                ) = 
# 210 "flambda_parser.mly"
                ( Closure { code_id = c } )
# 3135 "flambda_parser.ml"
                 in
                _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv386)) : 'freshtv388)
            | _ ->
                _menhir_fail ()) : 'freshtv390)) : 'freshtv392)) : 'freshtv394)
        | MenhirState87 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv401 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | AT ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv395) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LIDENT _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89) : 'freshtv396)
            | LIDENT _ | LPAREN | NEWER_VERSION_OF ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv397) = Obj.magic _menhir_stack in
                ((let _v : 'tv_option___anonymous_1_ = 
# 114 "<standard.mly>"
    ( None )
# 3164 "flambda_parser.ml"
                 in
                _menhir_goto_option___anonymous_1_ _menhir_env _menhir_stack _v) : 'freshtv398)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv399 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv400)) : 'freshtv402)
        | MenhirState93 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv405) * _menhir_state * 'tv_code_id) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv403) * _menhir_state * 'tv_code_id) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, (id : 'tv_code_id)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_option___anonymous_2_ = let x = 
# 145 "flambda_parser.mly"
                                                             ( id )
# 3184 "flambda_parser.ml"
             in
            
# 116 "<standard.mly>"
    ( Some x )
# 3189 "flambda_parser.ml"
             in
            _menhir_goto_option___anonymous_2_ _menhir_env _menhir_stack _v) : 'freshtv404)) : 'freshtv406)
        | _ ->
            _menhir_fail ()) : 'freshtv408)) : 'freshtv410)) : 'freshtv412)
    | MenhirState76 | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv415 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv413 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
        let _v : (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 3203 "flambda_parser.ml"
        ) = 
# 339 "flambda_parser.mly"
                 ( Dynamically_computed v )
# 3207 "flambda_parser.ml"
         in
        _menhir_goto_of_kind_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv414)) : 'freshtv416)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv425 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv423 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
        let _v : 'tv_closure_id = 
# 381 "flambda_parser.mly"
                 ( v )
# 3219 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv421) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_closure_id) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv419) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_closure_id) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv417) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((cid : 'tv_closure_id) : 'tv_closure_id) = _v in
        ((let _1 = () in
        let _v : 'tv_option___anonymous_1_ = let x = 
# 144 "flambda_parser.mly"
                                             ( cid )
# 3237 "flambda_parser.ml"
         in
        
# 116 "<standard.mly>"
    ( Some x )
# 3242 "flambda_parser.ml"
         in
        _menhir_goto_option___anonymous_1_ _menhir_env _menhir_stack _v) : 'freshtv418)) : 'freshtv420)) : 'freshtv422)) : 'freshtv424)) : 'freshtv426)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv433 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LANGLE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv427) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RANGLE ->
                _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98) : 'freshtv428)
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv429) = Obj.magic _menhir_stack in
            ((let _v : 'tv_kinded_vars_within_closures = 
# 326 "flambda_parser.mly"
    ( [] )
# 3271 "flambda_parser.ml"
             in
            _menhir_goto_kinded_vars_within_closures _menhir_env _menhir_stack _v) : 'freshtv430)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv431 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv432)) : 'freshtv434)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv437 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv435 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
        let _v : 'tv_name = 
# 367 "flambda_parser.mly"
                 ( (Var v:name) )
# 3290 "flambda_parser.ml"
         in
        _menhir_goto_name _menhir_env _menhir_stack _menhir_s _v) : 'freshtv436)) : 'freshtv438)
    | _ ->
        _menhir_fail ()) : 'freshtv440)) : 'freshtv442)

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | REC ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | EXN | LIDENT _ | STUB ->
        _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState45
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
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LIDENT _v ->
        _menhir_run196 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SEGMENT ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | SYMBOL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv357) = Obj.magic _menhir_stack in
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64) : 'freshtv358)
    | UNDERSCORE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run23 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 54 "flambda_parser.mly"
       (string * char option)
# 3347 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv355) = Obj.magic _menhir_stack in
    let (_endpos_c_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : (
# 54 "flambda_parser.mly"
       (string * char option)
# 3358 "flambda_parser.ml"
    )) : (
# 54 "flambda_parser.mly"
       (string * char option)
# 3362 "flambda_parser.ml"
    )) = _v in
    let (_startpos_c_ : Lexing.position) = _startpos in
    ((let _v : 'tv_const = 
# 361 "flambda_parser.mly"
            ( make_const_int c )
# 3368 "flambda_parser.ml"
     in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v) : 'freshtv356)

and _menhir_run115 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv353) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 3382 "flambda_parser.ml"
    ) = 
# 235 "flambda_parser.mly"
        ( Invalid Halt_and_catch_fire )
# 3386 "flambda_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv354)

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 50 "flambda_parser.mly"
       (string * char option)
# 3393 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv351) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : (
# 50 "flambda_parser.mly"
       (string * char option)
# 3403 "flambda_parser.ml"
    )) : (
# 50 "flambda_parser.mly"
       (string * char option)
# 3407 "flambda_parser.ml"
    )) = _v in
    ((let _v : 'tv_const = 
# 362 "flambda_parser.mly"
              ( make_const_float c )
# 3412 "flambda_parser.ml"
     in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v) : 'freshtv352)

and _menhir_run116 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116

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

and _menhir_run124 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv347 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv343) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 60 "flambda_parser.mly"
       (string)
# 3461 "flambda_parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv341) = Obj.magic _menhir_stack in
            let (_endpos_s_ : Lexing.position) = _endpos in
            let ((s : (
# 60 "flambda_parser.mly"
       (string)
# 3471 "flambda_parser.ml"
            )) : (
# 60 "flambda_parser.mly"
       (string)
# 3475 "flambda_parser.ml"
            )) = _v in
            let (_startpos_s_ : Lexing.position) = _startpos in
            ((let _v : 'tv_csymbol = let _endpos = _endpos_s_ in
            let _startpos = _startpos_s_ in
            
# 389 "flambda_parser.mly"
               ( s, make_loc (_startpos, _endpos) )
# 3483 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv339) = _menhir_stack in
            let (_v : 'tv_csymbol) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv337 * _menhir_state)) * 'tv_csymbol) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RBRACKET ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv333 * _menhir_state)) * 'tv_csymbol) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LPAREN ->
                    _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState128
                | COLON | MINUSGREATER ->
                    _menhir_reduce101 _menhir_env (Obj.magic _menhir_stack) MenhirState128
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128) : 'freshtv334)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv335 * _menhir_state)) * 'tv_csymbol) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)) : 'freshtv338)) : 'freshtv340)) : 'freshtv342)) : 'freshtv344)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv345 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv346)) : 'freshtv348)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv349 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv350)

and _menhir_run134 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState134 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134

and _menhir_run139 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState139 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState139 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139

and _menhir_reduce2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_andk = 
# 318 "flambda_parser.mly"
    ( [] )
# 3563 "flambda_parser.ml"
     in
    _menhir_goto_andk _menhir_env _menhir_stack _menhir_s _v

and _menhir_run218 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState218
    | STUB ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState218
    | LIDENT _ ->
        _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) MenhirState218
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState218

and _menhir_goto_separated_nonempty_list_AND_code_binding_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_AND_code_binding_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState62 | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv327) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_AND_code_binding_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv325) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((cbs : 'tv_separated_nonempty_list_AND_code_binding_) : 'tv_separated_nonempty_list_AND_code_binding_) = _v in
        ((let _v : 'tv_code_bindings = 
# 137 "flambda_parser.mly"
                                                     ( cbs )
# 3599 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv323) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_code_bindings) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv321 * _menhir_state * 'tv_code_bindings) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv315 * _menhir_state * 'tv_code_bindings) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SYMBOL ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191) : 'freshtv316)
        | IN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv317 * _menhir_state * 'tv_code_bindings) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (cbs : 'tv_code_bindings)) = _menhir_stack in
            let _v : 'tv_code_bindings_and_closures = 
# 131 "flambda_parser.mly"
                        ( cbs, [] )
# 3630 "flambda_parser.ml"
             in
            _menhir_goto_code_bindings_and_closures _menhir_env _menhir_stack _menhir_s _v) : 'freshtv318)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv319 * _menhir_state * 'tv_code_bindings) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)) : 'freshtv322)) : 'freshtv324)) : 'freshtv326)) : 'freshtv328)
    | MenhirState194 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv331 * _menhir_state * 'tv_code_binding)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_AND_code_binding_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv329 * _menhir_state * 'tv_code_binding)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_AND_code_binding_) : 'tv_separated_nonempty_list_AND_code_binding_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_code_binding)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_AND_code_binding_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3654 "flambda_parser.ml"
         in
        _menhir_goto_separated_nonempty_list_AND_code_binding_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv330)) : 'freshtv332)
    | _ ->
        _menhir_fail ()

and _menhir_run86 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | REC ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LIDENT _ ->
        _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_reduce101 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_simple_args = 
# 356 "flambda_parser.mly"
    ( [] )
# 3680 "flambda_parser.ml"
     in
    _menhir_goto_simple_args _menhir_env _menhir_stack _menhir_s _v

and _menhir_run118 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | INT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_kinded_args = 
# 322 "flambda_parser.mly"
    ( [] )
# 3710 "flambda_parser.ml"
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
        _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState54
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
        let (_menhir_stack : (('freshtv305 * _menhir_state * 'tv_switch_case)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv303 * _menhir_state * 'tv_switch_case)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (h : 'tv_switch_case)), _, (t : 'tv_switch)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_switch = 
# 220 "flambda_parser.mly"
                                         ( h :: t )
# 3743 "flambda_parser.ml"
         in
        _menhir_goto_switch _menhir_env _menhir_stack _menhir_s _v) : 'freshtv304)) : 'freshtv306)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv313 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv309 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv307 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), (sort : 'tv_switch_sort)), _, (scrutinee : 'tv_simple)), _, (cases : 'tv_switch)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 3765 "flambda_parser.ml"
            ) = 
# 239 "flambda_parser.mly"
    ( Switch {scrutinee; sort; cases} )
# 3769 "flambda_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv308)) : 'freshtv310)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv311 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv312)) : 'freshtv314)
    | _ ->
        _menhir_fail ()

and _menhir_reduce73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_option_SEMICOLON_ = 
# 114 "<standard.mly>"
    ( None )
# 3787 "flambda_parser.ml"
     in
    _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv301) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let x = () in
    let _v : 'tv_option_SEMICOLON_ = 
# 116 "<standard.mly>"
    ( Some x )
# 3801 "flambda_parser.ml"
     in
    _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv302)

and _menhir_run30 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 54 "flambda_parser.mly"
       (string * char option)
# 3808 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv299) = Obj.magic _menhir_stack in
    let (_endpos_tag_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((tag : (
# 54 "flambda_parser.mly"
       (string * char option)
# 3819 "flambda_parser.ml"
    )) : (
# 54 "flambda_parser.mly"
       (string * char option)
# 3823 "flambda_parser.ml"
    )) = _v in
    let (_startpos_tag_ : Lexing.position) = _startpos in
    ((let _v : 'tv_tag = let _endpos = _endpos_tag_ in
    let _startpos = _startpos_tag_ in
    
# 334 "flambda_parser.mly"
            ( make_tag ~loc:(make_loc (_startpos, _endpos)) tag )
# 3831 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv297) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_tag) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState35 | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv283 * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv279 * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState32 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv280)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv281 * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)) : 'freshtv284)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv289 * _menhir_state * 'tv_symbol)) * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv285 * _menhir_state * 'tv_symbol)) * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
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
                _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72) : 'freshtv286)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv287 * _menhir_state * 'tv_symbol)) * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv288)) : 'freshtv290)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv295 * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv291 * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FLOAT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | INT _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState136 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIDENT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState136 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState136 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState136
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136) : 'freshtv292)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv293 * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv294)) : 'freshtv296)
    | _ ->
        _menhir_fail ()) : 'freshtv298)) : 'freshtv300)

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
        let (_menhir_stack : ('freshtv275 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | APPLY ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | BLOCK ->
            _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | CCALL ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | CLOSURE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | CONT ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | FLOAT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | HCF ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState6
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv276)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((('freshtv277 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | EQUAL ->
            _menhir_reduce83 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107) : 'freshtv278)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4002 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv207 * _menhir_state * (
# 93 "flambda_parser.mly"
      (Fexpr.named)
# 4012 "flambda_parser.ml"
        ))) * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4016 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv205 * _menhir_state * (
# 93 "flambda_parser.mly"
      (Fexpr.named)
# 4022 "flambda_parser.ml"
        ))) * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4026 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (defining_expr : (
# 93 "flambda_parser.mly"
      (Fexpr.named)
# 4031 "flambda_parser.ml"
        ))), _, (body : (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4035 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4041 "flambda_parser.ml"
        ) = 
# 242 "flambda_parser.mly"
      ( Let { bindings = [ { var = None; kind = None; defining_expr; } ];
              closure_elements = None; 
              body } )
# 4047 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv206)) : 'freshtv208)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((((((('freshtv221 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4055 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((((((('freshtv219 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4061 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((((((((((_menhir_stack, _menhir_s), _, (recursive : 'tv_recursive)), _, (id : 'tv_code_id)), (closure_id : 'tv_option___anonymous_1_)), (newer_version_of : 'tv_option___anonymous_2_)), _, (params : 'tv_kinded_args)), _, (closure_var : 'tv_variable)), (vars_within_closures : 'tv_kinded_vars_within_closures)), _, (ret_cont : 'tv_continuation)), _, (exn_cont : 'tv_option_exn_continuation_)), _, (ret_arity : 'tv_return_arity)), _, (expr : (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4066 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _13 = () in
        let _9 = () in
        let _1 = () in
        let _v : 'tv_code_binding = 
# 153 "flambda_parser.mly"
    ( { id; closure_id; newer_version_of; params; closure_var;
        vars_within_closures; ret_cont; ret_arity; exn_cont; recursive; expr; }
       : code_binding )
# 4076 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv217) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_code_binding) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv215 * _menhir_state * 'tv_code_binding) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv209 * _menhir_state * 'tv_code_binding) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CODE ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState194) : 'freshtv210)
        | IN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv211 * _menhir_state * 'tv_code_binding) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_code_binding)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_AND_code_binding_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4107 "flambda_parser.ml"
             in
            _menhir_goto_separated_nonempty_list_AND_code_binding_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv212)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv213 * _menhir_state * 'tv_code_binding) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv214)) : 'freshtv216)) : 'freshtv218)) : 'freshtv220)) : 'freshtv222)
    | MenhirState199 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv231 * _menhir_state) * _menhir_state * 'tv_symbol_bindings)) * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4122 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv229 * _menhir_state) * _menhir_state * 'tv_symbol_bindings)) * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4128 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (bindings : 'tv_symbol_bindings)), _, (body : (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4133 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_let_symbol = 
# 110 "flambda_parser.mly"
                                                      ( { bindings; body } )
# 4140 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv227) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_let_symbol) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv225) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_let_symbol) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv223) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((ls : 'tv_let_symbol) : 'tv_let_symbol) = _v in
        ((let _v : (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4157 "flambda_parser.ml"
        ) = 
# 248 "flambda_parser.mly"
                     ( Let_symbol ls )
# 4161 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv224)) : 'freshtv226)) : 'freshtv228)) : 'freshtv230)) : 'freshtv232)
    | MenhirState203 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv241 * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) * _menhir_state * 'tv_with_closure_elements_opt)) * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4169 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv239 * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) * _menhir_state * 'tv_with_closure_elements_opt)) * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4175 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (bindings : 'tv_separated_nonempty_list_AND_let_binding_)), _, (closure_elements : 'tv_with_closure_elements_opt)), _, (body : (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4180 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _3 = () in
        let _v : 'tv_let_ = 
# 277 "flambda_parser.mly"
    ( { bindings; closure_elements; body } )
# 4186 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv237) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_let_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv235 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_let_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv233 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((l : 'tv_let_) : 'tv_let_) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4205 "flambda_parser.ml"
        ) = 
# 240 "flambda_parser.mly"
                 ( Let l )
# 4209 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv234)) : 'freshtv236)) : 'freshtv238)) : 'freshtv240)) : 'freshtv242)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv255 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_kinded_args)) * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4217 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv251 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_kinded_args)) * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4227 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv249 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_kinded_args)) * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4234 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s, (exn_and_stub : 'tv_exn_and_stub)), _, (name : 'tv_continuation)), _, (params : 'tv_kinded_args)), _, (handler : (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4239 "flambda_parser.ml"
            ))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _v : 'tv_continuation_handler = 
# 312 "flambda_parser.mly"
    ( let is_exn_handler, stub = exn_and_stub in
      { name; params; stub; is_exn_handler; handler } )
# 4247 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv247) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_continuation_handler) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState47 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv243 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AND ->
                    _menhir_run218 _menhir_env (Obj.magic _menhir_stack) MenhirState217
                | APPLY | BLOCK | CCALL | CLOSURE | CONT | FLOAT _ | HCF | INT _ | LET | LETK | LIDENT _ | OPAQUE | PROJECT_VAR | SWITCH | UIDENT _ | UNREACHABLE ->
                    _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState217
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState217) : 'freshtv244)
            | MenhirState218 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv245 * _menhir_state) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AND ->
                    _menhir_run218 _menhir_env (Obj.magic _menhir_stack) MenhirState219
                | APPLY | BLOCK | CCALL | CLOSURE | CONT | FLOAT _ | HCF | INT _ | LET | LETK | LIDENT _ | OPAQUE | PROJECT_VAR | SWITCH | UIDENT _ | UNREACHABLE ->
                    _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState219
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState219) : 'freshtv246)
            | _ ->
                _menhir_fail ()) : 'freshtv248)) : 'freshtv250)) : 'freshtv252)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv253 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_kinded_args)) * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4292 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)) : 'freshtv256)
    | MenhirState221 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv259 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4301 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv257 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4307 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s), _, (recursive : 'tv_recursive)), _, (handler : 'tv_continuation_handler)), _, (t : 'tv_andk)), _, (body : (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4312 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4318 "flambda_parser.ml"
        ) = 
# 246 "flambda_parser.mly"
     ( let handlers = handler :: t in
       Let_cont { recursive; body; handlers } )
# 4323 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv258)) : 'freshtv260)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv273 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4331 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv269 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4341 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv267 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4347 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (return_cont : 'tv_continuation)), _, (exception_cont : 'tv_option_exn_continuation_)), _, (body : (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4352 "flambda_parser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _v : (
# 89 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 4358 "flambda_parser.ml"
            ) = 
# 102 "flambda_parser.mly"
    ( { return_cont; exception_cont; body } )
# 4362 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv265) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 89 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 4370 "flambda_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv263) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 89 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 4378 "flambda_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv261) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 89 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 4386 "flambda_parser.ml"
            )) : (
# 89 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 4390 "flambda_parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv262)) : 'freshtv264)) : 'freshtv266)) : 'freshtv268)) : 'freshtv270)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv271 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4400 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv272)) : 'freshtv274)
    | _ ->
        _menhir_fail ()

and _menhir_reduce79 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_option_exn_continuation_ = 
# 114 "<standard.mly>"
    ( None )
# 4412 "flambda_parser.ml"
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
    | MenhirState221 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv51 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState219 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState218 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState217 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv57 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState212 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * 'tv_kinded_variable_opt)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState209 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * 'tv_let_binding)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState203 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv63 * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) * _menhir_state * 'tv_with_closure_elements_opt)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState201 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state * 'tv_separated_nonempty_list_AND_let_binding_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState199 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv67 * _menhir_state) * _menhir_state * 'tv_symbol_bindings)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState194 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * 'tv_code_binding)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState191 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * 'tv_code_bindings)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState187 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * 'tv_closure_element)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState181 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state * 'tv_var_within_closure)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState179 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState177 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * _menhir_state * 'tv_code_bindings_and_closures) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState175 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv81 * _menhir_state) * _menhir_state * 'tv_segment)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState170 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * 'tv_static_closure)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * (
# 93 "flambda_parser.mly"
      (Fexpr.named)
# 4522 "flambda_parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState158 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state * 'tv_simple) * 'tv_infix_binop) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv89 * _menhir_state * 'tv_simple))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91 * _menhir_state * 'tv_unop) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv93 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv95 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv97 * _menhir_state) * _menhir_state * 'tv_name) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv101 * _menhir_state) * _menhir_state * 'tv_tag)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv105 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv107 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv109 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv111 * _menhir_state)) * 'tv_csymbol)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState119 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv117 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((((('freshtv121 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv123 * _menhir_state * 'tv_kind)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((('freshtv127 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv129 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv131 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133 * _menhir_state * 'tv_kinded_var_within_closure) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv136)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv137 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) * _menhir_state * 'tv_kinded_args) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv139 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv142)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv143) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv144)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv145 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv149 * _menhir_state) * _menhir_state * 'tv_symbol)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv151 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv153 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv155 * _menhir_state * (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 4698 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv157 * _menhir_state * 'tv_symbol)) * _menhir_state) * _menhir_state * 'tv_tag)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv159 * _menhir_state * 'tv_symbol)) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv161 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv163 * _menhir_state) * _menhir_state * 'tv_symbol)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv165 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv169 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_kinded_args)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171 * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv175 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * _menhir_state * 'tv_exn_and_stub) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv178)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv179 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv185 * _menhir_state * 'tv_switch_case)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv187 * _menhir_state * 'tv_tag)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv189 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv191 * _menhir_state) * 'tv_switch_sort) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv193 * _menhir_state * 'tv_tag_to_size)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv195)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv196)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv197 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv199 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv201 * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv203) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv204)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 60 "flambda_parser.mly"
       (string)
# 4824 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv49) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 60 "flambda_parser.mly"
       (string)
# 4835 "flambda_parser.ml"
    )) : (
# 60 "flambda_parser.mly"
       (string)
# 4839 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_continuation = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 402 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 4847 "flambda_parser.ml"
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
            _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack) MenhirState3
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
# 106 "flambda_parser.mly"
                             ( cont )
# 4879 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_exn_continuation) = _v in
        ((match _menhir_s with
        | MenhirState132 ->
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
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4903 "flambda_parser.ml"
            ) = 
# 251 "flambda_parser.mly"
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
# 4917 "flambda_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv6)) : 'freshtv8)
        | MenhirState145 ->
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
# 91 "flambda_parser.mly"
      (Fexpr.expr)
# 4935 "flambda_parser.ml"
            ) = 
# 264 "flambda_parser.mly"
     ( Apply {
          func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind = Function Indirect_unknown_arity;
       })
# 4945 "flambda_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv10)) : 'freshtv12)
        | MenhirState3 | MenhirState106 ->
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
# 4960 "flambda_parser.ml"
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
# 214 "flambda_parser.mly"
                                          ( i,c )
# 4975 "flambda_parser.ml"
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
                _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35) : 'freshtv24)
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv25 * _menhir_state * 'tv_switch_case) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (c : 'tv_switch_case)) = _menhir_stack in
            let _v : 'tv_switch = 
# 219 "flambda_parser.mly"
                    ( [c] )
# 5010 "flambda_parser.ml"
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
            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53) : 'freshtv38)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv39 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_code_id) * 'tv_option___anonymous_1_) * 'tv_option___anonymous_2_) * _menhir_state * 'tv_kinded_args) * _menhir_state * 'tv_variable) * 'tv_kinded_vars_within_closures)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | COLON | EQUAL ->
            _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106) : 'freshtv40)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | AND | EOF | IN | RBRACE | WITH ->
            _menhir_reduce101 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117) : 'freshtv42)
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv43 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132) : 'freshtv44)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv45 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145) : 'freshtv46)
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
# 89 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 5104 "flambda_parser.ml"
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

# 408 "flambda_parser.mly"
  

# 5133 "flambda_parser.ml"

# 269 "<standard.mly>"
  

# 5138 "flambda_parser.ml"
