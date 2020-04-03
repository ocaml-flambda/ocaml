module Lex = Flambda_lex
module Parser = Flambda_parser

type error =
  | Lexing_error of Lex.error * Location.t
  | Parsing_error of Location.t

let make_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

let parse_fexpr filename =
  let ic = open_in filename in
  try
    let pos = { Lexing.pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
    let lb = Lexing.from_channel ic in
    let lb = { lb with lex_start_p = pos; lex_curr_p = pos } in
    let unit =
      try Ok (Parser.flambda_unit Lex.token lb)
      with
      | Parser.Error ->
        let loc = make_loc (Lexing.lexeme_start_p lb, Lexing.lexeme_end_p lb) in
        Error (Parsing_error loc)
      | Lex.Error (error, loc) ->
        Error (Lexing_error (error, make_loc loc))
    in
    close_in ic;
    unit
  with
  | e ->
    let x = Printexc.get_raw_backtrace () in
    close_in ic;
    Printexc.raise_with_backtrace e x

let parse (* ~backend *) filename =
  parse_fexpr filename
  |> Result.map (Fexpr_to_flambda.conv (* ~backend *)) 
