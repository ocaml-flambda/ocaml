(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                    Mark Shinwell, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Target processor-native integers.

   This module provides operations on the type of
   signed 32-bit integers (on 32-bit target platforms) or
   signed 64-bit integers (on 64-bit target platforms).
   This integer type has exactly the same width as that of a
   pointer type in the C compiler.  All arithmetic operations over
   are taken modulo 2{^32} or 2{^64} depending
   on the word size of the target architecture.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

type t
(** The type of target integers. *)

type targetint = t

val zero : t
(** The target integer 0.*)

val one : t
(** The target integer 1.*)

val minus_one : t
(** The target integer -1.*)

val neg : t -> t
(** Unary negation. *)

val add : t -> t -> t
(** Addition. *)

val sub : t -> t -> t
(** Subtraction. *)

val mul : t -> t -> t
(** Multiplication. *)

val div : t -> t -> t
(** Integer division.  Raise [Division_by_zero] if the second
   argument is zero.  This division rounds the real quotient of
   its arguments towards zero, as specified for {!Stdlib.(/)}. *)

val unsigned_div : t -> t -> t
(** Same as {!div}, except that arguments and result are interpreted as {e
    unsigned} integers. *)

val rem : t -> t -> t
(** Integer remainder.  If [y] is not zero, the result
   of [Targetint.rem x y] satisfies the following properties:
   [Targetint.zero <= Nativeint.rem x y < Targetint.abs y] and
   [x = Targetint.add (Targetint.mul (Targetint.div x y) y)
                      (Targetint.rem x y)].
   If [y = 0], [Targetint.rem x y] raises [Division_by_zero]. *)

val unsigned_rem : t -> t -> t
(** Same as {!rem}, except that arguments and result are interpreted as {e
    unsigned} integers. *)

val succ : t -> t
(** Successor.
   [Targetint.succ x] is [Targetint.add x Targetint.one]. *)

val pred : t -> t
(** Predecessor.
   [Targetint.pred x] is [Targetint.sub x Targetint.one]. *)

val abs : t -> t
(** Return the absolute value of its argument. *)

val size : int
(** The size in bits of a target native integer. *)

type num_bits =
  | Thirty_two
  | Sixty_four (**)
(** The possible numbers of bits of a target native integer. *)

val num_bits : num_bits
(* The number of bits of a target native integer. *)

val max_int : t
(** The greatest representable target integer,
    either 2{^31} - 1 on a 32-bit platform,
    or 2{^63} - 1 on a 64-bit platform. *)

val min_int : t
(** The smallest representable target integer,
   either -2{^31} on a 32-bit platform,
   or -2{^63} on a 64-bit platform. *)

val logand : t -> t -> t
(** Bitwise logical and. *)

val logor : t -> t -> t
(** Bitwise logical or. *)

val logxor : t -> t -> t
(** Bitwise logical exclusive or. *)

val lognot : t -> t
(** Bitwise logical negation. *)

val shift_left : t -> int -> t
(** [Targetint.shift_left x y] shifts [x] to the left by [y] bits.
    The result is unspecified if [y < 0] or [y >= bitsize],
    where [bitsize] is [32] on a 32-bit platform and
    [64] on a 64-bit platform. *)

val shift_right : t -> int -> t
(** [Targetint.shift_right x y] shifts [x] to the right by [y] bits.
    This is an arithmetic shift: the sign bit of [x] is replicated
    and inserted in the vacated bits.
    The result is unspecified if [y < 0] or [y >= bitsize]. *)

val shift_right_logical : t -> int -> t
(** [Targetint.shift_right_logical x y] shifts [x] to the right
    by [y] bits.
    This is a logical shift: zeroes are inserted in the vacated bits
    regardless of the sign of [x].
    The result is unspecified if [y < 0] or [y >= bitsize]. *)

val of_int : int -> t
(** Convert the given integer (type [int]) to a target integer
    (type [t]), modulo the target word size. *)

val of_int_exn : int -> t
(** Convert the given integer (type [int]) to a target integer
    (type [t]).  Raises a fatal error if the conversion is not exact. *)

val to_int : t -> int
(** Convert the given target integer (type [t]) to an
    integer (type [int]).  The high-order bit is lost during
    the conversion. *)

val of_float : float -> t
(** Convert the given floating-point number to a target integer,
   discarding the fractional part (truncate towards 0).
   The result of the conversion is undefined if, after truncation,
   the number is outside the range
   \[{!Targetint.min_int}, {!Targetint.max_int}\]. *)

val to_float : t -> float
(** Convert the given target integer to a floating-point number. *)

val of_int32 : int32 -> t
(** Convert the given 32-bit integer (type [int32])
    to a target integer. *)

val to_int32 : t -> int32
(** Convert the given target integer to a
    32-bit integer (type [int32]).  On 64-bit platforms,
    the 64-bit native integer is taken modulo 2{^32},
    i.e. the top 32 bits are lost.  On 32-bit platforms,
    the conversion is exact. *)

val of_int64 : int64 -> t
(** Convert the given 64-bit integer (type [int64])
    to a target integer, modulo the target word size. *)

val to_int64 : t -> int64
(** Convert the given target integer to a
    64-bit integer (type [int64]). *)

val of_string : string -> t
(** Convert the given string to a target integer.
    The string is read in decimal (by default) or in hexadecimal,
    octal or binary if the string begins with [0x], [0o] or [0b]
    respectively.
    Raise [Failure "int_of_string"] if the given string is not
    a valid representation of an integer, or if the integer represented
    exceeds the range of integers representable in type [nativeint]. *)

val to_string : t -> string
(** Return the string representation of its argument, in decimal. *)

val unsigned_compare: t -> t -> int
(** Same as {!compare}, except that arguments are interpreted as {e unsigned}
    integers. *)

type repr =
  | Int32 of int32
  | Int64 of int64

val repr : t -> repr
(** The concrete representation of a native integer. *)

val min : t -> t -> t
(** Returns the smaller integer. *)

val max : t -> t -> t
(** Returns the larger integer. *)

val get_least_significant_16_bits_then_byte_swap : t -> t
(** Extract the least significant 16 bits from the given target integer,
    exchange the order of the two bytes extracted, then form a new target
    integer by zero-extending those two bytes. *)

val swap_byte_endianness : t -> t

include Identifiable.S with type t := t

module Targetint_set = Set

module Pair : sig
  type nonrec t = t * t

  include Identifiable.S with type t := t
end

val cross_product : Set.t -> Set.t -> Pair.Set.t

module OCaml : sig
  (** Operations using the semantics of the type "int" on the target
      machine.  That is to say, 31-bit arithmetic on 32-bit targets; and
      63-bit arithmetic on 64-bit targets. *)

  type t

  type targetint_ocaml = t

  (* CR mshinwell: Maybe this should move somewhere else
    let max_array_length = max_wosize ()
    let max_string_length = word_size / 8 * max_array_length - 1

  *)

  val min_value : t
  (** The minimum ocaml integer representable on the target. *)

  val max_value : t
  (** The maximum ocaml integer representable on the target. *)

  val max_string_length : t
  (** The maximum string length on the target. *)

  val minus_one : t
  (** The ocaml target integer -1 *)

  val zero : t
  (** The ocaml targe integr 0. *)

  val one : t
  (** The ocaml target integer 1. *)

  val ten : t
  (** The ocaml target integer 10.*)

  val hex_ff : t
  (** The ocaml target integer 0xff. *)

  val (<=) : t -> t -> bool
  val (<) : t -> t -> bool
  (** Comparisons functions on ocaml target integers. *)

  val bottom_byte_to_int : t -> int
  (** Returns the 8 least significant bits of the ocaml target integer as
      a host caml integer (cannot overflow). *)

  val of_char : char -> t
  (** Returns the ocaml target integer corresponding to the ASCII code of the
      given character. *)

  val of_int : int -> t
  (** Convert the given integer (type [int]) to a target ocaml integer
      (type [t]), modulo the target word size minus one (for the tag bit). *)

  val of_int_option : int -> t option
  (** Returns [None] iff the given [int] cannot be represented as a target
      "int"-width integer, else returns the same as {!of_int}. *)

  val to_int : t -> int
  (** Convert the given target integer (type [t]) to an
      integer (type [int]), modulo the [int] size, i.e. high-order bits are
      lost during the conversion. *)

  val to_int_option : t -> int option
  (** Convert the given ocaml target integer (type [t]) to an
      integer (type [int]). Returns [None] if the original ocaml
      target integer does not fit into an integer *)

  val to_int_exn : t -> int
  (** Convert the given ocaml target integer (type [t]) to an
      integer (type [int]).
      @raise Fatal_error if the original ocaml target integer
                         does not fit into an integer *)

  val of_int32 : int32 -> t
  (** Convert the given 32-bit integer (type [int32])
      to a target ocaml integer, modulo the size of a target
      ocaml integer. *)

  val to_int32 : t -> int32
  (** Convert the given ocaml target integer to a
      32-bit integer (type [int32]). On 64-bit platforms,
      the 64-bit native integer is taken modulo 2{^32},
      i.e. the top 32 bits are lost. On 32-bit platforms,
      the conversion is exact. *)

  val of_int64 : int64 -> t
  (** Convert the given 64-bit integer (type [int64])
      to a target native integer, modulo the size of a target
      ocaml integer. *)

  val to_int64 : t -> int64
  (** Convert the given ocaml target integer to a
      64-bit integer (type [int64]). *)

  val of_targetint : targetint -> t
  (** Convert the given target native integer (type [targetint])
      to an ocaml target integer, modulo the size of an ocaml
      target integer. *)

  val to_targetint : t -> targetint
  (** Convert the given ocaml target integer (type [t]) to a target
      native integer (type [targetint]). *)

  val of_float : float -> t
  (** Convert the given floating-point number to an ocaml target integer,
      discarding the fractional part (truncate towards 0).
      The result of the conversion is undefined if, after truncation,
      the number is outside the range
      \[{!Targetint.OCaml.min_value}, {!TargetintOCaml.max_value}\]. *)

  val to_float : t -> float
  (** Convert the given target integer to a floating-point number. *)

  val neg : t -> t
  (** Unary negation. *)

  val get_least_significant_16_bits_then_byte_swap : t -> t
  (** Extract the least significant 16 bits from the given ocaml target
      integer, exchange the order of the two bytes extracted, then form
      a new target integer by zero-extending those two bytes. *)

  val add : t -> t -> t
  (** Addition. *)

  val sub : t -> t -> t
  (** Subtraction. *)

  val mul : t -> t -> t
  (** Multiplication. *)

  val mod_ : t -> t -> t
  val div : t -> t -> t
  (** Integer division and modulo.  Raise [Division_by_zero] if the second
      argument is zero.  This division rounds the real quotient of
      its arguments towards zero, as specified for {!Stdlib.(/)}. *)

  val and_ : t -> t -> t
  (** Bitwise logical and. *)

  val or_ : t -> t -> t
  (** Bitwise logical or. *)

  val xor : t -> t -> t
  (** Bitwise logical exclusive or. *)

  val shift_left : t -> int -> t
  (** [shift_left x y] shifts [x] to the left by [y] bits.
      The result is unspecified if [y < 0] or [y >= bitsize],
      where [bitsize] is [31] on a 32-bit platform and
      [61] on a 64-bit platform. *)

  val shift_right : t -> int -> t
  (** [Targetint.shift_right x y] shifts [x] to the right by [y] bits.
      This is an arithmetic shift: the sign bit of [x] is replicated
      and inserted in the vacated bits.
      The result is unspecified if [y < 0] or [y >= bitsize]. *)

  val shift_right_logical : t -> int -> t
  (** [Targetint.shift_right_logical x y] shifts [x] to the right
      by [y] bits.
      This is a logical shift: zeroes are inserted in the vacated bits
      regardless of the sign of [x].
      The result is unspecified if [y < 0] or [y >= bitsize]. *)

  val min : t -> t -> t
  (** Returns the smaller integer. *)

  val max : t -> t -> t
  (** Returns the larger integer. *)


  (* CR mshinwell: Add an [Array] module *)

  include Identifiable.S with type t := t

  val set_of_targetint_set : Targetint_set.t -> Set.t

  module Pair : sig
    type nonrec t = t * t

    include Identifiable.S with type t := t
  end

  val cross_product : Set.t -> Set.t -> Pair.Set.t

  module Or_unknown : sig
    type nonrec t = private
      | Ok of t
      | Unknown

    val ok : targetint_ocaml -> t
    val unknown : unit -> t

    include Identifiable.S with type t := t
  end
end
