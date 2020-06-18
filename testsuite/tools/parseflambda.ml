(* open Flambda *)

let check_invariants program =
  try Flambda_unit.invariant program
  with exn ->
    Format.eprintf "Program which failed invariant check:@ %a\n%!"
      Flambda_unit.print program;
    raise exn

let parse_flambda ~backend file =
    match Parse_flambda.parse_fexpr file with
    | Ok unit ->
      let comp_unit = Parse_flambda.make_compilation_unit file in
      Compilation_unit.set_current comp_unit;
      Format.printf "%a@.@."
        Print_fexpr.flambda_unit unit;
      let fl2 = Fexpr_to_flambda.conv unit in
      Format.printf "flambda:@.%a@.@."
        Flambda_unit.print fl2;
      check_invariants fl2;
      let fl2' = Simplify.run ~backend ~round:1 fl2 in
      Format.printf "simplify:@.%a@."
        Flambda_unit.print fl2';
      fl2'
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
  let file = Sys.argv.(1) in
  let ext = Filename.extension file in
  match ext with
  | ".fl" -> parse_flambda ~backend:(module Asmgen.Flambda2_backend) file
  | _ ->
    Misc.fatal_errorf "Unrecognized extension %s" ext
