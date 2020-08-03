type error =
  | Lexing_error of Flambda_lex.error * Location.t
  | Parsing_error of string * Location.t

val parse_fexpr
   : string
  -> (Fexpr.flambda_unit, error) result

val make_compilation_unit
   : extension:string
  -> filename:string
  -> Compilation_unit.t

val parse
   : backend:(module Flambda_backend_intf.S)
  -> string
  -> (Flambda_unit.t, error) result
