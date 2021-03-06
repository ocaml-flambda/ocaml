(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2020 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

let join_points () = !Clflags.Flambda.join_points
let unbox_along_intra_function_control_flow () =
  !Clflags.Flambda.unbox_along_intra_function_control_flow
let backend_cse_at_toplevel () = !Clflags.Flambda.backend_cse_at_toplevel
let cse_depth () = !Clflags.Flambda.cse_depth

module Expert = struct
  let code_id_and_symbol_scoping_checks () =
    !Clflags.Flambda.Expert.code_id_and_symbol_scoping_checks
  let fallback_inlining_heuristic () =
    !Clflags.Flambda.Expert.fallback_inlining_heuristic
  let inline_effects_in_cmm () =
    !Clflags.Flambda.Expert.inline_effects_in_cmm
  let max_block_size_for_projections () =
    !Clflags.Flambda.Expert.max_block_size_for_projections
end
