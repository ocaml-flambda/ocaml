(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2020 OCamlPro SAS                                    *)
(*   Copyright 2018--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

include Parameter.Make(struct let name_mode = Name_mode.phantom end)

let of_kinded_param param =
  let var = Kinded_parameter.var param in
  let kind = Kinded_parameter.kind param in
  create var kind

let to_kinded_param param =
  let var' = var param in
  let kind' = kind param in
  Kinded_parameter.create var' kind'

