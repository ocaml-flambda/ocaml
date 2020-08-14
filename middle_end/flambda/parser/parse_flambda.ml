module Lex = Flambda_lex
module Parser = Flambda_parser

type error =
  | Lexing_error of Lex.error * Location.t
  | Parsing_error of string * Location.t

let make_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

let run_parser ~start_symbol filename =
  let ic = open_in filename in
  try
    let pos = { Lexing.pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
    let lb = Lexing.from_channel ic in
    let lb = { lb with lex_start_p = pos; lex_curr_p = pos } in
    let supplier = Parser.MenhirInterpreter.lexer_lexbuf_to_supplier Lex.token lb in
    let start = start_symbol pos in
    let result =
      try Parser.MenhirInterpreter.loop_handle
            (fun ans -> Ok ans)
            (function
              | HandlingError error_state ->
                let s =
                  Parser.MenhirInterpreter.current_state_number error_state
                in
                let msg =
                  try Flambda_parser_messages.message s
                  with Not_found -> Format.sprintf "Unknown error in state %d" s
                in
                let loc =
                  make_loc (Parser.MenhirInterpreter.positions error_state)
                in
                Error (Parsing_error (msg, loc))
              | _ ->
                assert false (* the manual promises that HandlingError is the
                only possible constructor *))
            supplier start
      with
      | Lex.Error (error, loc) ->
        Error (Lexing_error (error, make_loc loc))
    in
    close_in ic;
    result
  with
  | e ->
    let x = Printexc.get_raw_backtrace () in
    close_in ic;
    Printexc.raise_with_backtrace e x

let parse_fexpr = run_parser ~start_symbol:Parser.Incremental.flambda_unit

let make_compilation_unit ~extension ~filename =
  let basename =
    Filename.chop_suffix filename ("." ^ extension)
    |> Filename.basename
  in
  let name = String.capitalize_ascii basename in
  (* CR lmaurer: Adding "caml" to the front is a hacky way to conform to the
     simplifier, which breaks when creating the module block symbol unless the
     compilation unit has exactly this linkage name. It would be better to
     either have the simplifier use the current compilation unit or not
     duplicate the prefixing logic here. *)
  let linkage_name = Linkage_name.create ("caml" ^ name) in
  let id = Ident.create_persistent name in
  Compilation_unit.create id linkage_name

let parse ~backend filename =
  parse_fexpr filename
  |> Result.map (fun fexpr ->
    let comp_unit = make_compilation_unit ~extension:"fl" ~filename in
    let old_comp_unit = Compilation_unit.get_current () in
    Compilation_unit.set_current comp_unit;
    let module_ident = Compilation_unit.get_persistent_ident comp_unit in
    let flambda = Fexpr_to_flambda.conv ~backend ~module_ident fexpr in
    begin
      match old_comp_unit with
      | Some old_comp_unit -> Compilation_unit.set_current old_comp_unit
      | None -> ()
    end;
    flambda
  )
