(* Continuation variables *)
module C = struct
  type t = string
  let compare = String.compare
end
module CM = Map.Make(C)

(* Variables *)
module V = struct
  type t = string
  let compare = String.compare
end
module VM = Map.Make(V)

(* Symbols *)
module S = struct
  type t = string
  let compare = String.compare
end
module SM = Map.Make(S)

(* Code ids *)
module D = struct
  type t = string
  let compare = String.compare
end
module DM = Map.Make(D)

(* Closure ids *)
module U = struct
  type t = string
  let compare = String.compare
end
module UM = Map.Make(U)

(* Variables within closures *)
module W = struct
  type t = string
  let compare = String.compare
end
module WM = Map.Make(W)

type fun_decl_info = {
  decl : Flambda.Function_declaration.t;
  (* The closure id to use with this function declaration.  This isn't formally
   * part of the Flambda syntax, but each code is associated with exactly one
   * closure id that should ever be used for it, so it's convenient to remember
   * it here so that, for instance, a set of closures can be constructed from
   * code ids alone. *)
  closure_id : Closure_id.t;
}

type var_within_closure_info = {
  closure_id : Closure_id.t;
}

type env = {
  done_continuation : Continuation.t;
  error_continuation : Exn_continuation.t;
  continuations : (Continuation.t * int) CM.t;
  exn_continuations : Exn_continuation.t CM.t;
  variables : Variable.t VM.t;
  symbols : Symbol.t SM.t;
  code_ids : Code_id.t DM.t;
  code : fun_decl_info Code_id.Map.t;
  closure_ids : Closure_id.t UM.t;
  vars_within_closures : Var_within_closure.t WM.t;
  var_within_closure_info : var_within_closure_info Var_within_closure.Map.t;
}

let init_env done_continuation error_continuation = {
  done_continuation;
  error_continuation;
  continuations = CM.empty;
  exn_continuations = CM.empty;
  variables = VM.empty;
  symbols = SM.empty;
  code_ids = DM.empty;
  code = Code_id.Map.empty;
  closure_ids = UM.empty;
  vars_within_closures = WM.empty;
  var_within_closure_info = Var_within_closure.Map.empty;
}

let enter_code env = {
  continuations = CM.empty;
  exn_continuations = CM.empty;
  variables = VM.empty;
  done_continuation = env.done_continuation;
  error_continuation = env.error_continuation;
  symbols = env.symbols;
  code_ids = env.code_ids;
  code = env.code;
  closure_ids = env.closure_ids;
  vars_within_closures = env.vars_within_closures;
  var_within_closure_info = env.var_within_closure_info;
}

let fresh_cont env ?sort { Fexpr.txt = c; loc = _ } arity =
  let c' = Continuation.create ?sort () in
  c',
  { env with
    continuations = CM.add c (c', arity) env.continuations }

let fresh_exn_cont env { Fexpr.txt = c; loc = _ } =
  let c' = Continuation.create ~sort:Exn () in
  let e = Exn_continuation.create ~exn_handler:c' ~extra_args:[] in
  e,
  { env with
    exn_continuations = CM.add c e env.exn_continuations }

let fresh_var env { Fexpr.txt = name; loc = _ } =
  let v = Variable.create name ~user_visible:() in
  v,
  { env with
    variables = VM.add name v env.variables }

let fresh_code_id env { Fexpr.txt = name; loc = _ } =
  let c = Code_id.create ~name (Compilation_unit.get_current_exn ()) in
  c,
  { env with
    code_ids = DM.add name c env.code_ids }

let fresh_closure_id env { Fexpr.txt = name; loc = _ } =
  let v = Variable.create name in
  let c = Closure_id.wrap (Compilation_unit.get_current_exn ()) v in
  c,
  { env with
    closure_ids = UM.add name c env.closure_ids }

let fresh_or_existing_closure_id env ({ Fexpr.txt = name; loc = _ } as id) =
  match UM.find_opt name env.closure_ids with
  | None -> fresh_closure_id env id
  | Some closure_id -> closure_id, env

let fresh_var_within_closure env { Fexpr.txt = name; loc = _ } =
  let v = Variable.create name in
  let c = Var_within_closure.wrap (Compilation_unit.get_current_exn ()) v in
  c,
  { env with
    vars_within_closures = WM.add name c env.vars_within_closures }

let declare_symbol (*~backend:_*) (env:env) { Fexpr.txt = name; loc } =
  if SM.mem name env.symbols then
    Misc.fatal_errorf "Redefinition of symbol %s: %a"
      name Lambda.print_scoped_location loc
  else
    (* let module Backend = (val backend : Flambda_backend_intf.S) in
     * let symbol = Backend.symbol_for_global' (Ident.create_persistent name) in *)
    let symbol =
      Symbol.create
        (Compilation_unit.get_current_exn ())
        (Linkage_name.create name)
    in
    symbol,
    { env with
      symbols = SM.add name symbol env.symbols }

let find_with ~descr ~find map { Fexpr.txt = name; loc } =
  match find name map with
  | None ->
    Misc.fatal_errorf "Unbound %s %s: %a"
      descr name Lambda.print_scoped_location loc
  | Some a ->
    a

let get_symbol (env:env) sym =
  find_with ~descr:"symbol" ~find:SM.find_opt env.symbols sym 

let find_cont_id env c =
  find_with ~descr:"continuation id" ~find:CM.find_opt env.continuations c

let find_cont env (c : Fexpr.continuation) =
  match c with
  | Special Done -> env.done_continuation, 1
  | Special Error -> Exn_continuation.exn_handler env.error_continuation, 1
  | Named cont_id -> find_cont_id env cont_id

let find_exn_cont_id env c =
  find_with ~descr:"exn_continuation" ~find:CM.find_opt env.exn_continuations c
  

let find_exn_cont env (c : Fexpr.continuation) =
  match c with
  | Special Done -> Misc.fatal_error "done is not an exception continuation"
  | Special Error -> env.error_continuation
  | Named cont_id -> find_exn_cont_id env cont_id

let find_var env v =
  find_with ~descr:"variable" ~find:VM.find_opt env.variables v

let find_code_id env code_id =
  find_with ~descr:"code id" ~find:DM.find_opt env.code_ids code_id

let find_fun_decl env (code_id : Code_id.t) =
  match Code_id.Map.find_opt code_id env.code with
  | None ->
    Misc.fatal_errorf "Unbound Flambda code id %a"
      Code_id.print code_id
  | Some d ->
    d

let find_closure_id env closure_id =
  find_with ~descr:"closure id" ~find:UM.find_opt env.closure_ids closure_id

let _ = find_closure_id (* not currently used since the user never explicitly
                         * refers to a closure id after declaring it *)

let find_var_within_closure env vwc =
  find_with ~descr:"var within closure" ~find:WM.find_opt
    env.vars_within_closures vwc

let find_var_within_closure_info env (vwc : Var_within_closure.t) =
  match Var_within_closure.Map.find_opt vwc env.var_within_closure_info with
  | None ->
    Misc.fatal_errorf "Unbound Flambda var within closure %a"
      Var_within_closure.print vwc
  | Some i ->
    i

let const (c:Fexpr.const) : Reg_width_const.t =
  match c with
  | Tagged_immediate i ->
    let i = Targetint.of_string i in
    Reg_width_const.tagged_immediate (Target_imm.int (Targetint.OCaml.of_targetint i))
  | Naked_immediate i ->
    let i = Targetint.of_string i in
    Reg_width_const.naked_immediate (Target_imm.int (Targetint.OCaml.of_targetint i))

  (*
   * | Naked_float of Numbers.Float_by_bit_pattern.t
   * | Naked_int32 of Int32.t
   * | Naked_int64 of Int64.t
   * | Naked_nativeint of Targetint.t *)
  | _ ->
    failwith "TODO const"

let simple env (s:Fexpr.simple) : Simple.t =
  match s with
  | Var { txt = v; loc } -> begin
      match VM.find_opt v env.variables with
      | None ->
        Misc.fatal_errorf "Unbound variable %s : %a" v
          Lambda.print_scoped_location loc
      | Some var ->
        Simple.var var
    end
  | Const c ->
    Simple.const (const c)
  | Symbol sym -> begin
      Simple.symbol (get_symbol env sym)
    end

let name env (s:Fexpr.name) : Simple.t =
  match s with
  | Var { txt = v; loc } -> begin
      match VM.find_opt v env.variables with
      | None ->
        Misc.fatal_errorf "Unbound variable %s : %a" v
          Lambda.print_scoped_location loc
      | Some var ->
        Simple.var var
    end
  | Symbol sym -> begin
      Simple.symbol (get_symbol env sym)
    end

let of_kind_value env (v:Fexpr.of_kind_value)
      : Flambda.Static_const.Field_of_block.t =
  match v with
  | Symbol s ->
    Symbol (get_symbol env s)
  | Tagged_immediate i ->
    let i = Targetint.of_string i in
    Tagged_immediate
      (Target_imm.int (Targetint.OCaml.of_targetint i))
  | Dynamically_computed var ->
    let var = find_var env var in
    Dynamically_computed var

let unop env (unop:Fexpr.unop) : Flambda_primitive.unary_primitive =
  match unop with
  | Opaque_identity -> Opaque_identity
  | Project_var var ->
    let var = find_var_within_closure env var in
    let { closure_id; _ } = find_var_within_closure_info env var in
    Project_var { project_from = closure_id; var }

let infix_binop (binop:Fexpr.infix_binop) : Flambda_primitive.binary_primitive =
  match binop with
  | Plus -> Int_arith (Tagged_immediate, Add)
  | Minus -> Int_arith (Tagged_immediate, Sub)
  | Plusdot
  | Minusdot -> failwith "TODO binop"

let binop (binop:Fexpr.binop) : Flambda_primitive.binary_primitive =
  match binop with
  | Block_load (Block Value, Immutable) ->
          (* CR maurerl Add real tag and size to fexpr syntax and use them here
           *)
          Block_load (Block { elt_kind = Value (Anything);
                              tag = Tag.zero;
                              size = Unknown }, Immutable)
  | Block_load (_, _) ->
    failwith "TODO binop"

let convert_mutable_flag (flag : Fexpr.mutable_or_immutable)
      : Effects.mutable_or_immutable =
  match flag with
  | Mutable -> Mutable
  | Immutable -> Immutable

let convert_static_mutable_flag (flag : Fexpr.mutable_or_immutable)
      : Mutable_or_immutable.t =
  match flag with
  | Mutable -> Mutable
  | Immutable -> Immutable

let convert_recursive_flag (flag : Fexpr.is_recursive) : Recursive.t =
  match flag with
  | Recursive -> Recursive
  | Nonrecursive -> Non_recursive

let convert_block_shape ~num_fields =
  List.init num_fields (fun _field : Flambda_primitive.Value_kind.t -> Anything)

let defining_expr env (named:Fexpr.named) : Flambda.Named.t =
  match named with
  | Simple s ->
    Flambda.Named.create_simple (simple env s)
  | Prim (Unop (u, arg)) ->
    let prim : Flambda_primitive.t =
      Unary (unop env u, simple env arg)
    in
    Flambda.Named.create_prim prim Debuginfo.none
  | Prim (Infix_binop (b, a1, a2)) ->
    let prim : Flambda_primitive.t =
      Binary (infix_binop b, simple env a1, simple env a2)
    in
    Flambda.Named.create_prim prim Debuginfo.none
  | Prim (Binop (b, a1, a2)) ->
    let prim : Flambda_primitive.t =
      Binary (binop b, simple env a1, simple env a2)
    in
    Flambda.Named.create_prim prim Debuginfo.none
  | Prim (Block (tag, mutability, args)) ->
    let mutability = convert_mutable_flag mutability in
    let shape = convert_block_shape ~num_fields:(List.length args) in
    let kind : Flambda_primitive.make_block_kind =
      Full_of_values (Tag.Scannable.create_exn tag, shape)
    in
    let prim : Flambda_primitive.t =
      Flambda_primitive.Variadic (
        Make_block (kind, mutability),
        List.map (simple env) args
      )
    in
    Flambda.Named.create_prim prim Debuginfo.none
  | _ -> assert false

let value_kind : Fexpr.kind -> Flambda_kind.t = function
  | Value -> Flambda_kind.value
  | Naked_number naked_number_kind ->
    begin
      match naked_number_kind with
      | Naked_immediate -> Flambda_kind.naked_immediate
      | Naked_float -> Flambda_kind.naked_float
      | Naked_int32 -> Flambda_kind.naked_int32
      | Naked_int64 -> Flambda_kind.naked_int64
      | Naked_nativeint -> Flambda_kind.naked_nativeint
    end
  | Fabricated -> Flambda_kind.fabricated

let value_kind_opt : Fexpr.kind option -> Flambda_kind.t = function
  | Some kind -> value_kind kind
  | None -> Flambda_kind.value

let arity a = Flambda_arity.create (List.map value_kind a)

let set_of_closures env code_ids closure_elements =
  let fun_decls : Function_declarations.t =
    let code_id_to_binding (code_id : Fexpr.code_id)
          : Function_declarations.Binding.t =
      let code_id = find_code_id env code_id in
      let { decl; closure_id } = find_fun_decl env code_id in
      { Function_declarations.Binding.closure_id; func_decl = decl }
    in
    List.map code_id_to_binding code_ids
    |> Function_declarations.create
  in
  let closure_elements = Option.value closure_elements ~default:[] in
  let closure_elements : Simple.t Var_within_closure.Map.t =
    let convert ({ var; value } : Fexpr.closure_element) =
      (find_var_within_closure env var, simple env value)
    in
    List.map convert closure_elements
    |> Var_within_closure.Map.of_list
  in
  Set_of_closures.create fun_decls ~closure_elements

(* Updates the environment with the assigned code id and closure id and
 * the derived Function_declaration.t *)
let add_fun_decl_info env (code_binding : Fexpr.code_binding) : env =
  let code_id, env = fresh_code_id env code_binding.id in
  (* By default, use the code id as the name for the closure as well *)
  let closure_id =
    Option.value code_binding.closure_id ~default:code_binding.id
  in
  (* Two code bindings can use the same closure id (say, if one is a newer
   * version of the other), so use a preexisting closure id if one is in
   * scope with the same name *)
  let closure_id, env = fresh_or_existing_closure_id env closure_id in
  let params_arity =
    let param_kinds =
      List.map (fun ({ kind; _ } : Fexpr.kinded_parameter) ->
        Option.value kind ~default:(Value : Fexpr.kind)
      ) code_binding.params
    in
    arity param_kinds
  in
  let result_arity = match code_binding.ret_arity with
    | Some a -> arity a
    | None -> [ Flambda_kind.value ]
  in
  let recursive = convert_recursive_flag code_binding.recursive in
  let decl =
    Function_declaration.create
      ~code_id
      ~params_arity
      ~result_arity
      ~recursive
      ~dbg:Debuginfo.none
      ~inline:Default_inline
      ~is_a_functor:false
      ~stub:false
  in
  let env =
    let add_var_within_closure env
          ({ var; kind = _} : Fexpr.kinded_var_within_closure) =
      let _, env = fresh_var_within_closure env var in env
    in
    List.fold_left add_var_within_closure env code_binding.vars_within_closures
  in
  let fun_decl_info = { decl; closure_id } in
  { env with code = Code_id.Map.add code_id fun_decl_info env.code }

(* Map over a list to produce a new list, while simultaneously updating an
 * additional argument (such as an environment) being passed along. *)
let map_accum_left (f : 'env -> 'a -> 'b * 'env) (env : 'env) (l : 'a list)
    : 'b list * 'env =
  let next (acc, env) x = let (y, env) = f env x in (y :: acc, env) in
  let (acc, env) = List.fold_left next ([], env) l in
  (List.rev acc, env)

let rec expr env (e : Fexpr.expr) : Flambda.Expr.t =
  match e with
  | Let { bindings = []; _ } ->
    assert false (* should not be possible *)
  | Let { bindings = ({ defining_expr = Closure _ } :: _) as bindings;
          closure_elements; body } ->
      let binding_to_var_and_code_id : Fexpr.let_binding -> _ = function
        | { var = Some var; defining_expr = Closure { code_id; }; _ } ->
          (var, code_id)
        | { var = None; _ } ->
          Misc.fatal_errorf "Variable name required when defining closure"
        | { var = Some { txt = _; loc }; _ } ->
          Misc.fatal_errorf "Cannot use 'and' with non-closure: %a"
            Lambda.print_scoped_location loc
      in
      let vars_and_code_ids = List.map binding_to_var_and_code_id bindings in
      let closure_vars, env =
        let convert_binding env (var, code_id)
            : (Closure_id.t * Var_in_binding_pos.t) * env =
          let var, env = fresh_var env var in
          let var = Var_in_binding_pos.create var Name_mode.normal in
          let { closure_id; _ } : fun_decl_info =
            find_fun_decl env (find_code_id env code_id)
          in
          (closure_id, var), env
        in
        let pairs, env = map_accum_left convert_binding env vars_and_code_ids in
        Closure_id.Map.of_list pairs, env
      in
      let bound = Bindable_let_bound.set_of_closures ~closure_vars in
      let named =
        let code_ids = List.map snd vars_and_code_ids in
        set_of_closures env code_ids closure_elements
        |> Flambda.Named.create_set_of_closures
      in
      let body = expr env body in
      Flambda.Expr.create_pattern_let bound named body
  | Let { bindings = _ :: _ :: _; _ } ->
    Misc.fatal_errorf
      "Multiple let bindings only allowed when defining closures"
  | Let { closure_elements = Some _ } ->
    Misc.fatal_errorf
      "'with' clause only allowed when defining closures"
  | Let { bindings = [{ var = Some var; kind = _; defining_expr = d }];
          body } ->
    let named = defining_expr env d in
    let id, env = fresh_var env var in
    let body = expr env body in
    let var =
      Var_in_binding_pos.create id Name_mode.normal
    in
    Flambda.Expr.create_let var named body
  | Let_cont
      { recursive; body;
        handlers = [handler] } -> begin
      let is_exn_handler = false in
      let name, body_env =
        fresh_cont env handler.name (List.length handler.params)
      in
      let body = expr body_env body in
      let env =
        match recursive with
        | Nonrecursive -> env
        | Recursive -> body_env
      in
      let handler_env, params =
        List.fold_right
          (fun ({ param; kind }:Fexpr.kinded_parameter)
            (env, args) ->
            let var, env = fresh_var env param in
            let param = Kinded_parameter.create var (value_kind_opt kind) in
            env, param :: args)
          handler.params (env, [])
      in
      let handler =
        expr handler_env handler.handler
      in
      let params_and_handler =
        Flambda.Continuation_params_and_handler.create params ~handler
      in
      let handler =
        Flambda.Continuation_handler.create ~params_and_handler
          ~stub:false
          ~is_exn_handler:is_exn_handler
      in
      match recursive with
      | Nonrecursive ->
        Flambda.Let_cont.create_non_recursive name handler ~body
      | Recursive ->
        let handlers = Continuation.Map.singleton name handler in
        Flambda.Let_cont.create_recursive handlers ~body
    end

  | Let_cont _ ->
      failwith "TODO andwhere"

  | Apply_cont (cont, None, args) ->
    let c, arity = find_cont env cont in
    if List.length args <> arity then
      begin
        let cont_str = match cont with
        | Special Done -> "done"
        | Special Error -> "error"
        | Named { txt = cont_id; _ } -> cont_id
        in
        Misc.fatal_errorf "wrong continuation arity %s" cont_str
      end;
    let args = List.map (simple env) args in
    let apply_cont = Flambda.Apply_cont.create c ~args ~dbg:Debuginfo.none in
    Flambda.Expr.create_apply_cont apply_cont

  | Switch { scrutinee; cases } ->
    let arms =
      Target_imm.Map.of_list
        (List.map (fun (case, cont) ->
           let c, arity = find_cont env cont in
           assert(arity = 0);
           Target_imm.int (Targetint.OCaml.of_int case),
             Apply_cont_expr.create c ~args:[] ~dbg:Debuginfo.none)
           cases)
    in
    Flambda.Expr.create_switch
      ~scrutinee:(simple env scrutinee)
      ~arms

  | Let_symbol { bindings = Simple { symbol; kind = _; defining_expr = def }; 
                 body } -> begin
      match def with
      | Block { tag; mutability; elements = args } ->
        let symbol, env = declare_symbol (* ~backend *) env symbol in
        let bound_symbols =
          Flambda.Let_symbol_expr.Bound_symbols.Singleton symbol
        in
        let static_const =
          let mutability = convert_static_mutable_flag mutability in
          let tag = Tag.Scannable.create_exn tag in
          Flambda.Static_const.Block
            (tag, mutability,
             List.map (of_kind_value env) args)
        in
        let body = expr env body in
        Flambda.Let_symbol_expr.create Syntactic bound_symbols static_const body
        |> Flambda.Expr.create_let_symbol
    end

  | Let_symbol { bindings = Segments segments; body } ->
    (* First, sweep through the code definitions in all segments, reserving
     * code ids, closure ids, and vars within closures, but not translating
     * bodies yet (since they may be mutually recursive) *)
    let env =
      let add_fun_decls_in_segment env (seg : Fexpr.segment) : env =
        List.fold_left add_fun_decl_info env seg.code_bindings
      in
      List.fold_left add_fun_decls_in_segment env segments
    in
    (* Now assemble the set of closures in each segment, along with the
     * map from each closure id to the symbol defined for it *)
    let (sets_of_closures
           : ( Flambda.Let_symbol_expr.Closure_binding.t list *
               Set_of_closures.t ) list),
        env =
      let process_segment env (seg : Fexpr.segment)
            : ( Flambda.Let_symbol_expr.Closure_binding.t list *
                Set_of_closures.t ) * env =
        let code_ids =
          List.map (fun ({ code_id; _ } : Fexpr.static_closure_binding) ->
            code_id
          ) seg.closure_bindings
        in
        let set = set_of_closures env code_ids seg.closure_elements in
        let map, env =
          let make_pair env ({ symbol; code_id } : Fexpr.static_closure_binding)
                : Flambda.Let_symbol_expr.Closure_binding.t * env =
            let code_id = find_code_id env code_id in
            let { closure_id; _ } : fun_decl_info = find_fun_decl env code_id in
            let symbol, env = declare_symbol env symbol in
            { symbol; closure_id; }, env
          in
          map_accum_left make_pair env seg.closure_bindings
        in
        (map, set), env
      in
      map_accum_left process_segment env segments
    in
    (* Finally, process the code definitions *)
    let codes : Flambda.Static_const.Code_binding.t list list =
      let process_segment (seg : Fexpr.segment) =
        let process_code ({
              id; newer_version_of; params; closure_var; ret_cont; exn_cont;
              ret_arity; expr = code_expr
            } : Fexpr.code_binding)
              : Flambda.Static_const.Code_binding.t =
          let code_id = find_code_id env id in
          let env = enter_code env in
          let my_closure, env = fresh_var env closure_var in
          let arity =
            match ret_arity with
            | None -> 1
            | Some l -> List.length l
          in
          let return_continuation, env =
            fresh_cont env ret_cont arity
          in
          let exn_continuation, env =
            match exn_cont with
            | None ->
              (* Not bound *)
              let exn_handler = Continuation.create ~sort:Exn () in
              Exn_continuation.create ~exn_handler ~extra_args:[], env
            | Some exn_cont ->
              fresh_exn_cont env exn_cont
          in
          let params, env =
            map_accum_left
              (fun env ({ param; kind }:Fexpr.kinded_parameter) ->
                let var, env = fresh_var env param in
                let param = Kinded_parameter.create var (value_kind_opt kind) in
                param, env)
              env params
          in
          let body = expr env code_expr in
          let dbg = Debuginfo.none in
          let params_and_body =
            Flambda.Function_params_and_body.create
              ~return_continuation
              exn_continuation params ~body ~my_closure ~dbg
          in
          let newer_version_of =
            Option.map (find_code_id env) newer_version_of
          in
          let code : Flambda.Static_const.Code.t = {
                params_and_body = Present params_and_body;
                newer_version_of;
              }
          in 
          { Flambda.Static_const.Code_binding.code_id; code }
        in
        List.map process_code seg.code_bindings
      in
      List.map process_segment segments
    in
    let bound_symbols : Flambda.Let_symbol_expr.Bound_symbols.t =
      let codes_and_sets_of_closures =
        List.map2 (fun code_bindings (closure_symbols, _) ->
          let code_ids =
            List.map (fun { Flambda.Static_const.Code_binding.code_id; _ } ->
              code_id
            ) code_bindings
            |> Code_id.Set.of_list
          in
          ({ code_ids; closure_symbols }
             : Flambda.Let_symbol_expr.Bound_symbols.Code_and_set_of_closures.t)
        ) codes sets_of_closures
      in
      Sets_of_closures codes_and_sets_of_closures
    in
    let static_const : Flambda.Static_const.t =
      let codes_and_sets_of_closures =
        List.map2 (fun code (_, set_of_closures) ->
          ({ code; set_of_closures }
             : Flambda.Static_const.Code_and_set_of_closures.t)
        ) codes sets_of_closures
      in
      Sets_of_closures codes_and_sets_of_closures
    in
    let body = expr env body in
    Flambda.Let_symbol_expr.create Syntactic bound_symbols static_const body
    |> Flambda.Expr.create_let_symbol

  | Apply {
    func;
    call_kind;
    continuation;
    exn_continuation;
    args;
    arities; } ->
    let continuation, _integer_arity = find_cont env continuation in
    let call_kind =
      match call_kind with
      | Function (Direct { code_id }) ->
        let code_id = find_code_id env code_id in
        let fun_decl = find_fun_decl env code_id in
        let closure_id = fun_decl.closure_id in
        let return_arity =
          match arities with
          | None -> Function_declaration.result_arity fun_decl.decl
          | Some { ret_arity; _ } ->
            (* This should be the same as the arity from the decl, of course,
             * but let's use the explicitly-given one where possible *)
            arity ret_arity
        in
        Call_kind.direct_function_call code_id closure_id ~return_arity
      | Function Indirect ->
        begin
          match arities with
          | Some { params_arity; ret_arity } ->
            let param_arity = arity params_arity in
            let return_arity = arity ret_arity in
            Call_kind.indirect_function_call_known_arity
              ~param_arity ~return_arity
          | None ->
            Call_kind.indirect_function_call_unknown_arity ()
        end
      | C_call { alloc } ->
        begin
          match arities with
          | Some { params_arity; ret_arity } ->
            let param_arity = arity params_arity in
            let return_arity = arity ret_arity in
            Call_kind.c_call ~alloc ~param_arity ~return_arity
          | None ->
            Misc.fatal_errorf "Must specify arities for C call"
        end
    in
    let exn_continuation = find_exn_cont env exn_continuation in
    let apply =
      Flambda.Apply.create
        ~callee:(name env func)
        ~continuation
        exn_continuation
        ~args:((List.map (simple env)) args)
        ~call_kind
        (Debuginfo.none)
        ~inline:Default_inline
        ~inlining_depth:0
    in
    Flambda.Expr.create_apply apply

  | _ ->
    failwith "TODO expr"

let conv (* ~backend:_ *) (fexpr : Fexpr.flambda_unit) : Flambda_unit.t =
  let return_continuation = Continuation.create () in
  let exn_continuation = Continuation.create ~sort:Exn () in
  let exn_continuation_as_exn_continuation =
    Exn_continuation.create ~exn_handler:exn_continuation ~extra_args:[]
  in
  let env = init_env return_continuation exn_continuation_as_exn_continuation in
  let body = expr env fexpr.body in
  Flambda_unit.create
    ~imported_symbols:Symbol.Map.empty
    ~return_continuation
    ~exn_continuation
    ~body
