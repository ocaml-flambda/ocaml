(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(** The names of continuations. *)

type t = private Table_by_int_id.Id.t
type exported

include Identifiable.S with type t := t

module Sort : sig
  type t =
    | Normal
    | Return
    | Define_root_symbol
    | Toplevel_return
    | Exn
end

val create : ?sort:Sort.t -> ?name:string -> unit -> t

val rename : t -> t

val name : t -> string

val name_stamp : t -> int

val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

val sort : t -> Sort.t

val is_exn : t -> bool

val export : t -> exported

val import : exported -> t

val map_compilation_unit :
      (Compilation_unit.t -> Compilation_unit.t) -> exported -> exported

module With_args : sig
  type nonrec t = t * Variable.t list
  include Identifiable.S with type t := t
end

val initialise : unit -> unit
