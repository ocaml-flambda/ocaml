(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020--2020 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Report inlining decisions *)

module Decision : sig
  (** This modules defines the various kinds of decisions related to
      inlining that will be reported, together with some additional
      information to better identify to what the decision refers to. *)

  module At_call_site : sig

    type t =
      | C_call
      | Method
      | Unknown_function
      (** Function call where the function's type is unknown. *)
      | Non_inlinable_function of {
          code_id : Code_id.t; (** code id of the callee *)
        }
      (** Function call where the function's type is known,
          but was marked as non-inlinable. *)
      | Inlinable_function of {
          code_id : Code_id.t; (** code id of the callee *)
          decision : Inlining_decision.Call_site_decision.t;
        }
      (** Function call where the function's type is known,
          and was marked as inlinable. *)

  end

  module At_function_declaration : sig

    type pass =
      | Before_simplify
      | After_simplify (**)
    (** There are two decisions made for each function declaration:
        one before simplifying the body, and one after (this is useful
        for e.g. recursive functions). *)

    type t = {
      pass : pass;
      code_id : Code_id.t; (** code id of the function being declared *)
      decision : Inlining_decision.Function_declaration_decision.t;
    }

  end

  type t =
    | At_call_site of At_call_site.t
    | At_function_declaration of At_function_declaration.t (**)

end


val record_decision : dbg:Debuginfo.t -> Decision.t -> unit
(** Record a decision. *)

val output_then_forget_decisions : output_prefix:string -> unit
(** Output the report for all recorded decisions up to that point,
    and clean/forget all decisions.
    Note that this function should be called once for each round of
    simplification. *)


