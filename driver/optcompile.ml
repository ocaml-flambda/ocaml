(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The batch compiler *)

open Misc
open Compile_common

let tool_name = "ocamlopt"

let with_info =
  Compile_common.with_info ~native:true ~tool_name

let interface ~source_file ~output_prefix =
  with_info ~source_file ~output_prefix ~dump_ext:"cmi" @@ fun info ->
  Compile_common.interface info

let (|>>) (x, y) f = (x, f y)

(** Native compilation backend for .ml files. *)

(* XXX *)
module Flambda2_backend = struct
  let symbol_for_global' ?comp_unit id =
    let comp_unit =
      match comp_unit with
      | None -> Compilation_unit.get_current_exn ()
      | Some comp_unit -> comp_unit
    in
    Symbol.unsafe_create
      comp_unit
      (Linkage_name.create (Compilenv.symbol_for_global id))

  let closure_symbol _ = failwith "Not yet implemented"

  let division_by_zero =
    symbol_for_global'
      ~comp_unit:(Compilation_unit.predefined_exception ())
      Predef.ident_division_by_zero

  let invalid_argument =
    symbol_for_global'
      ~comp_unit:(Compilation_unit.predefined_exception ())
      Predef.ident_invalid_argument

  let all_predefined_exception_symbols =
    Symbol.Set.of_list [
      division_by_zero;
      invalid_argument;
    ] (* CR mshinwell: and the rest... *)

  let symbol_for_global' id : Symbol.t = symbol_for_global' id

  let size_int = Arch.size_int
  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    Proc.max_arguments_for_tailcalls - 1
end
let flambda2_backend =
  (module Flambda2_backend : Flambda2_backend_intf.S)

let flambda2 i typed =
  if !Clflags.classic_inlining then begin
    Clflags.default_simplify_rounds := 1;
    Clflags.use_inlining_arguments_set Clflags.classic_arguments;
    Clflags.unbox_free_vars_of_closures := false;
    Clflags.unbox_specialised_args := false
  end;
  typed
  |> Profile.(record transl)
      (Translmod.transl_implementation_flambda i.module_name)
  |> Profile.(record generate)
    (fun {Lambda.module_ident; main_module_block_size;
          required_globals; code; } ->
    ((module_ident, main_module_block_size), code)
    |>> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.lambda
    |>> Simplif.simplify_lambda
    |>> print_if i.ppf_dump Clflags.dump_lambda Printlambda.lambda
    |> (fun ((module_ident, main_module_block_size), code) ->
        Asmgen.compile_implementation2 required_globals
          ~backend:flambda2_backend
          ~prefixname:i.output_prefix
          ~size:main_module_block_size
          ~filename:i.source_file
          ~module_ident
          ~module_initializer:code
          ~ppf_dump:i.ppf_dump
          ~middle_end:Flambda2_middle_end.middle_end);
    Compilenv.save_unit_info (cmx i)
    )

let clambda i backend typed =
  Clflags.use_inlining_arguments_set Clflags.classic_arguments;
  typed
  |> Profile.(record transl)
    (Translmod.transl_store_implementation i.module_name)
  |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.program
  |> Profile.(record generate)
    (fun program ->
       let code = Simplif.simplify_lambda program.Lambda.code in
       { program with Lambda.code }
       |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.program
       |> Asmgen.compile_implementation
            ~backend
            ~filename:i.source_file
            ~prefixname:i.output_prefix
            ~middle_end:Closure_middle_end.lambda_to_clambda
            ~ppf_dump:i.ppf_dump;
       Compilenv.save_unit_info (cmx i))

let implementation ~backend ~source_file ~output_prefix =
  let backend info typed =
    Compilenv.reset ?packname:!Clflags.for_package info.module_name;
    if Config.flambda then
      flambda2 info typed
    else clambda info backend typed
  in
  with_info ~source_file ~output_prefix ~dump_ext:"cmx" @@ fun info ->
  Compile_common.implementation info ~backend
