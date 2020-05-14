let make_compilation_unit file =
  let basename = Filename.chop_suffix file ".fl" |> Filename.basename in
  let name = String.capitalize_ascii basename in
  let linkage_name = Linkage_name.create name in
  let id = Ident.create_persistent name in
  Compilation_unit.create id linkage_name

let parse_flambda file =
  match Parse_flambda.parse_fexpr file with
  | Ok unit ->
    let comp_unit = make_compilation_unit file in
    Compilation_unit.set_current comp_unit;
    Fexpr_to_flambda.conv unit
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
  let file1 = Sys.argv.(1) in
  let file2 = Sys.argv.(2) in
  let unit1 = parse_flambda file1 in
  let unit2 = parse_flambda file2 in
  Format.printf "%a@."
    (Compare.Comparison.print Flambda_unit.print)
    (Compare.flambda_units unit1 unit2)
