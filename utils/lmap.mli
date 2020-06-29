
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(**

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

module type Thing = sig
  type t

  val equal : t -> t -> bool

  val print : Format.formatter -> t -> unit
end

module type S = sig
  type key

  type (+'a) t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val disjoint_union : 'a t -> 'a t -> 'a t
  val disjoint_union_many: 'a t list -> 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val keys : _ t -> key list
  val data : 'a t -> 'a list
  val bindings : 'a t -> (key * 'a) list
  val of_list : (key * 'a) list -> 'a t
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val get_singleton : 'a t -> (key * 'a) option
  val get_singleton_exn : 'a t -> key * 'a
  val map: ('a -> 'b) -> 'a t -> 'b t
  val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
  val map_sharing: ('a -> 'a) -> 'a t -> 'a t
  val filter_map: 'a t -> f:(key -> 'a -> 'b option) -> 'b t
  val to_seq : 'a t -> (key * 'a) Seq.t
  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
  val of_seq : (key * 'a) Seq.t -> 'a t

  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module Make (T : Thing) : S with type key = T.t
