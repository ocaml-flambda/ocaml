let check_invariants program =
  try Flambda_unit.invariant program
  with exn ->
    Format.eprintf "Program which failed invariant check:@ %a\n%!"
      Flambda_unit.print program;
    raise exn

module Outcome = struct
  type t = Success | Failure | Error

  let to_exit_code = function
    | Success -> 0
    | Failure -> 1
    | Error -> 2
end

let run_expect_test ~backend filename : Outcome.t =
  match Parse_flambda.parse_expect_test_spec filename with
  | Ok { before; after = expected } ->
    begin
      let comp_unit =
        Parse_flambda.make_compilation_unit ~extension:"flt" ~filename
      in
      Compilation_unit.set_current comp_unit;
      let module_ident = Compilation_unit.get_persistent_ident comp_unit in
      let before_fl = Fexpr_to_flambda.conv ~backend ~module_ident before in
      check_invariants before_fl;
      let { Simplify.unit = actual_fl; _ } =
        Simplify.run ~backend ~round:1 before_fl
      in
      let expected_fl = Fexpr_to_flambda.conv ~backend ~module_ident expected in
      match Compare.flambda_units actual_fl expected_fl with
      | Equivalent ->
        Format.eprintf "PASS@.";
        Success
      | Different { approximant = actual' } ->
        let actual_fexpr = Flambda_to_fexpr.conv actual' in
        let corrected_filename = filename ^ ".corrected" in
        let corrected_out = open_out corrected_filename in
        Format.fprintf (corrected_out |> Format.formatter_of_out_channel)
          "@[<v>%a@ ===>@ %a@]@."
          Print_fexpr.flambda_unit before
          Print_fexpr.flambda_unit actual_fexpr;
        close_out corrected_out;
        Format.eprintf "FAIL - Saving corrected test as %s@."
          corrected_filename;
        Failure
    end
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
    Error

let _ =
  let file = Sys.argv.(1) in
  let ext = Filename.extension file in
  let outcome =
    match ext with
    | ".flt" ->
      run_expect_test ~backend:(module Asmgen.Flambda_backend) file
    | _ ->
      Misc.fatal_errorf "Unrecognized extension %s; expected .flt" ext
  in
  exit (outcome |> Outcome.to_exit_code)

