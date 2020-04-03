type error =
  | Lexing_error of Flambda_lex.error * Location.t
  | Parsing_error of Location.t

val parse_fexpr
   : string
  -> (Fexpr.flambda_unit, error) result

val parse
   : (* backend:(module Flambda2_backend_intf.S)
  -> *)
     string
  -> (Flambda_unit.t, error) result
