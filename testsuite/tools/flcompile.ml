let backend : (module Flambda_backend_intf.S) = (module Asmgen.Flambda_backend)

let parse_flambda file =
  match Parse_flambda.parse ~backend file with
  | Ok unit ->
    unit
  | Error e ->
    begin match e with
    | Parsing_error (msg, loc) ->
      Format.eprintf
        "%a:@.\
         Syntax error: %s@."
        Location.print_loc loc
        msg
    | Lexing_error (error, loc) ->
      Format.eprintf
        "%a:@.\
         Lex error: %a@."
        Location.print_loc loc
        Flambda_lex.pp_error error
    end;
    exit 1

let _ =
  let () = Warnings.parse_options false "-58" in
  let file = Sys.argv.(1) in
  let unit = parse_flambda file in
  Format.printf "%a@."
    Flambda_unit.print
    unit;
  let { Simplify.unit = actual_fl; _ } =
    Simplify.run ~backend ~round:1 unit
  in
  Format.printf "%a@."
    Flambda_unit.print
    actual_fl
