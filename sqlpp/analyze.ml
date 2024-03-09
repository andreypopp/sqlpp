open Syntax
open Ppx_hash_lib.Std.Hash.Builtin
open Ppx_compare_lib.Builtin

type row = (name * ty) list

type pty =
  | Pty of ty
  | Pty_variant of (name * pty list) list
  | Pty_expr of ty * Scope.scope
  | Pty_unknown

type params = pty NM.t

type query_info = {
  scope : Scope.scope;
  inner_scope : Scope.scope;
  params : params;
  row : row;
  query : query pos;
}

type fieldset_info = {
  fs_scopes : Scope.scopes;
  fs_fields : (ty * name * expr) list;
}

type env = env_decl NT.t

and env_decl =
  | T of Scope.scope * Ddl.table
  | Q of query_info
  | F of fieldset_info

let env_find_table env name =
  let loc = fst name in
  match NT.find_opt env name with
  | Some (T (scope, table)) -> scope, table
  | Some _ -> Report.errorf ~loc "not a table: %s" (snd name)
  | None -> Report.errorf ~loc "no such table: %s" (snd name)

let lookup_field scope e =
  NT.to_seq scope.Scope.fields
  |> Seq.find_map (fun (n, f) -> if equal_expr e f.expr then Some f else None)

let subscopes scope =
  List.to_seq scope.Scope.scopes
  |> Seq.uniq (Equal.map ~f:fst equal_name)
  |> Seq.map (fun ((_, n), _) -> n)
  |> List.of_seq
  |> String.concat ~sep:", "

let build_field scope ~dependencies ~is_generated ~is_used name expr ty =
  let f = make_field ~dependencies ~is_generated ~is_used name expr ty in
  NT.replace scope.Scope.fields name f;
  f

let subsumes ~loc (ty : ty) ~(sup : ty) =
  let unexepected () =
    Report.errorf ~loc "expected %s but got %s" (ty_to_string sup)
      (ty_to_string ty)
  in
  match ty.nullable, sup.nullable with
  | `non_null, `non_null | `null, `null | `non_null, `null ->
      let subsumes =
        match ty.v, sup.v with
        | Ty t, Ty sup -> equal_name t sup
        | Ty t, Ty_one_of (_, sups) -> List.mem ~eq:equal_name t sups
        | Ty_one_of (_, ts), Ty sup -> false
        | Ty_one_of (_, ts), Ty_one_of (_, sups) ->
            List.for_all ~f:(fun t -> List.mem ~eq:equal_name t sups) ts
      in
      if not subsumes then unexepected ();
      ty
  | `null, `non_null -> unexepected ()

let rec sty_to_scope ?src env (sty : sty) =
  match sty with
  | Sty_name name -> fst (env_find_table env name)
  | Sty_struct elems ->
      let scopes, fields =
        List.fold_left elems ~init:([], [])
          ~f:(fun (scopes, fields) (name, sty) ->
            match sty with
            | Sty_elem_ty ty ->
                let f =
                  make_field ~is_generated:false ~is_used:false name
                    (expr_name name) ty
                in
                scopes, f :: fields
            | Sty_elem_scope sty ->
                let scope = sty_to_scope ?src env sty in
                (name, Scope.S (non_null scope)) :: scopes, fields)
      in
      Scope.scope_create ~is_open:true ~fields ~scopes ()

let rec scope_subsumes ~loc scope ~(sup : Scope.scope) =
  scopes_subsumes ~loc scope.Scope.scopes ~sup:sup.scopes;
  NT.iter
    (fun name f ->
      match NT.find_opt scope.Scope.fields name with
      | Some f' -> ignore (subsumes ~loc f.ty ~sup:f'.ty : ty)
      | None -> Report.errorf ~loc "no such column: %s" (snd name))
    sup.fields

and scopes_subsumes ~loc scopes ~sup =
  List.iter sup ~f:(fun (name, _) ->
      let scope =
        match Scope.Scopes.lookup' name scopes with
        | Some (_names, v) -> v.v
        | None -> failwithf "scopes_subsumes: no such scope"
      in
      let sup =
        match List.assoc_opt ~eq:equal_name name sup with
        | Some (S v') -> v'.v
        | Some (A _) | None -> failwithf "scopes_subsumes: alias found"
      in
      scope_subsumes ~loc scope ~sup)

let rec unify_pty ~loc ty' ty =
  match ty', ty with
  | ty, Pty_unknown -> ty
  | Pty_unknown, ty -> ty
  | Pty _, Pty_expr _
  | Pty _, Pty_variant _
  | Pty_variant _, Pty_expr _
  | Pty_variant _, Pty _
  | Pty_expr _, Pty _
  | Pty_expr _, Pty_variant _ ->
      Report.errorf ~loc "param type mismatch"
  | Pty ty', Pty ty -> Pty (ty_glb ~loc ty ty')
  | Pty_variant xs', Pty_variant xs ->
      let rec merge acc tag tys = function
        | [] -> List.rev ((tag, tys) :: acc)
        | (tag', tys') :: xs when equal_name tag tag' ->
            let tys =
              List.map2 tys' tys ~f:(fun ty' ty -> unify_pty ~loc ty' ty)
            in
            List.rev_append acc ((tag, tys) :: xs)
        | x :: xs -> merge (x :: acc) tag tys xs
      in
      let xs =
        List.fold_left xs ~init:xs' ~f:(fun xs' (tag, tys) ->
            List.rev (merge [] tag tys xs'))
      in
      Pty_variant xs
  | Pty_expr _, Pty_expr _ ->
      Report.errorf ~loc
        "param with the same name already defined (this is a todo)"

module Params = struct
  type t = { mutable params : params }

  let create () = { params = NM.empty }
  let find name t = NM.find_opt name t.params

  let add ~loc name ty t =
    t.params <-
      NM.update name
        (function None -> Some ty | Some ty' -> Some (unify_pty ~loc ty' ty))
        t.params;
    NM.find name t.params

  let remove_names names t =
    let params, stash =
      List.fold_left names ~init:(t.params, []) ~f:(fun (params, stash) name ->
          match NM.find_opt name params with
          | None -> params, (name, None) :: stash
          | Some ty -> NM.remove name params, (name, Some ty) :: stash)
    in
    t.params <- params;
    List.rev stash

  let add_names stash t =
    t.params <-
      List.fold_left stash ~init:t.params ~f:(fun params (name, ty) ->
          match ty with None -> params | Some ty -> NM.add name ty params)
end

let try_resolve_name ~f (env : Scope.scope) name =
  let loc, name' = name in
  let xs =
    List.to_seq env.scopes
    |> Seq.uniq (Equal.map ~f:fst equal_name)
    |> Seq.filter_map (function n, _ -> f n)
    |> Seq.to_list
  in
  match xs with
  | [ x ] -> x
  | [] -> Report.errorf ~loc "no such column: %s" name'
  | _ -> Report.errorf ~loc "ambigious column: %s" name'

type query_ctx = { params : Params.t; env : env }

let make_query_ctx ?params env =
  let params =
    match params with Some params -> params | None -> Params.create ()
  in
  { params; env }

module Expr_ctx : sig
  type t = private {
    scope : Scope.scope;
    is_used : bool;
    query_ctx : query_ctx;
    mutable dependencies : (name option * name) list;
  }

  val make : is_used:bool -> Scope.scope -> query_ctx -> t
  val add_dependency : t -> name option * name -> unit
end = struct
  type t = {
    scope : Scope.scope;
    is_used : bool;
    query_ctx : query_ctx;
    mutable dependencies : (name option * name) list;
  }

  let make ~is_used scope query_ctx =
    { scope; is_used; query_ctx; dependencies = [] }

  let add_dependency =
    let eq = CCEqual.(pair (option equal_name) equal_name) in
    fun ctx p -> ctx.dependencies <- List.add_nodup ~eq p ctx.dependencies
end

let get_select_row scope select =
  List.filter_map select.node.select_proj ~f:(function
    | Field_with_scope _ | Field_fieldset _ ->
        failwithf "get_select_row: not a field"
    | Field f when not f.is_used -> None
    | Field f -> (
        let name = Option.get_exn_or "impossible" f.name in
        match NT.find_opt scope.Scope.fields name with
        | Some f -> Some (name, f.ty)
        | None -> failwithf "get_select_row: no such field"))

let resolve_field_dependency scope (scope_name, name) =
  let scope' =
    match scope_name with
    | None -> scope
    | Some scope_name ->
        let rec lookup = function
          | [] -> failwithf "missing scope: %s" (snd scope_name)
          | (n, Scope.A v) :: scopes -> lookup scopes
          | (n, S v) :: scopes when equal_name n scope_name -> v.v
          | (n, S _) :: scopes -> lookup scopes
        in
        lookup scope.Scope.scopes
  in
  match NT.find_opt scope'.fields name with
  | Some f -> scope', f
  | None -> failwithf "resolve_field_dependency: no such field %s" (snd name)

let rec mark_field_as_used scope (f : field) =
  if not f.is_used then (
    f.is_used <- true;
    List.iter f.dependencies ~f:(fun p ->
        let scope', f = resolve_field_dependency scope p in
        mark_field_as_used scope' f))

let mark_field_dependencies_as_used (ctx : Expr_ctx.t) (f : field) =
  List.iter f.dependencies ~f:(fun p ->
      let scope', f = resolve_field_dependency ctx.scope p in
      Expr_ctx.add_dependency ctx p;
      mark_field_as_used scope' f)

let mark_field_as_used ctx scope n f =
  Expr_ctx.add_dependency ctx (n, f.name);
  if ctx.is_used then mark_field_as_used scope f

module Check_agg = struct
  type ctx = { group_by : expr list option; scope : Scope.scope }

  let is_agg_expr_app name _args =
    match snd name with "count" | "sum" | "max" | "min" -> true | _ -> false

  let fold =
    object (self)
      inherit [unit, ctx] Syntax.fold as super

      method! fold_select ctx select () =
        let group_by = select.node.select_group_by in
        super#fold_select { ctx with group_by } select ()

      method! fold_select_proj ctx fields () =
        super#fold_select_proj ctx fields ();
        NT.iter (fun _ f -> self#fold_expr ctx f.expr ()) ctx.scope.fields

      method! fold_From_select ctx select alias () =
        let scope =
          match Scope.scope_subscope ctx.scope alias with
          | Some scope -> scope
          | None -> failwithf "Check_agg.fold_From_select: no such scope"
        in
        super#fold_From_select { ctx with scope } select alias ()

      method! fold_expr ctx expr () =
        match ctx.group_by with
        | Some group_by when List.exists group_by ~f:(equal_expr expr) -> ()
        | _ -> super#fold_expr ctx expr ()

      method! fold_Expr_app ctx name args () =
        let ctx =
          match Option.is_some ctx.group_by, is_agg_expr_app name args with
          | false, true ->
              Report.errorf ~loc:(fst name)
                "aggregate function `%s(..)` is not allowed without GROUP BY"
                (snd name)
          | true, true -> { ctx with group_by = None }
          | _, false -> ctx
        in
        super#fold_Expr_app ctx name args ()

      method! fold_Expr_nav ctx name expr () =
        match expr.node with
        | Expr_name _ ->
            if Option.is_some ctx.group_by then
              Report.errorf ~loc:expr.loc
                "expression `%s` is not in GROUP BY clause and is not under \
                 aggregate function"
                (expr_to_string (expr_nav name expr))
        | _ -> super#fold_Expr_nav ctx name expr ()
    end

  let run scope f x = f { group_by = None; scope } x ()
end

let is_valid_date =
  let re = Re.Str.regexp {|^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$|} in
  fun x -> Re.Str.string_match re x 0

let is_valid_datetime =
  let re =
    Re.Str.regexp
      {|^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]$|}
  in
  fun x -> Re.Str.string_match re x 0

let rec infer_expr ~(ctx : Expr_ctx.t) (expr : expr) : ty * expr =
  let loc = expr.loc in
  match expr.node with
  | Expr_nav (name, expr) -> (
      match Scope.Scopes.lookup_step name ctx.scope.scopes with
      | `Alias (names, scopes) ->
          let names = List.rev names in
          let expr =
            List.fold_left names ~init:expr ~f:(fun expr name ->
                expr_nav name expr)
          in
          let ctx' =
            let scope = { ctx.scope with scopes } in
            Expr_ctx.make ~is_used:ctx.is_used scope ctx.query_ctx
          in
          let ty, expr = infer_expr ~ctx:ctx' expr in
          List.iter ctx'.dependencies ~f:(Expr_ctx.add_dependency ctx);
          ty, expr
      | `Scope scope' -> (
          let rec is_nav e =
            match e.node with
            | Expr_nav (_, e) -> is_nav e
            | Expr_name _ -> true
            | _ -> false
          in
          if (not scope'.v.is_open) && not (is_nav expr) then
            Report.errorf ~loc
              "subquery `%s` doesn't allow to build new expressions inside"
              (snd name);
          match lookup_field scope'.v expr with
          | Some f ->
              mark_field_as_used ctx scope'.v (Some name) f;
              let ty = nullable_lub scope' f.ty in
              ty, expr_nav name (expr_name f.name)
          | None ->
              let ctx' =
                Expr_ctx.make ~is_used:ctx.is_used scope'.v ctx.query_ctx
              in
              let ty, expr = infer_expr ~ctx:ctx' expr in
              let f =
                match lookup_field scope'.v expr with
                | Some f ->
                    mark_field_as_used ctx scope'.v (Some name) f;
                    f
                | None ->
                    let n' = loc, sprintf "_%i" (NT.length scope'.v.fields) in
                    let f =
                      build_field ~is_generated:true ~is_used:ctx'.is_used
                        ~dependencies:ctx'.dependencies scope'.v n' expr ty
                    in
                    mark_field_as_used ctx scope'.v (Some name) f;
                    f
              in
              nullable_lub scope' ty, expr_nav name (expr_name f.name))
      | `none ->
          Report.errorf ~loc "no such table/query `%s` (available %s)"
            (snd name) (subscopes ctx.scope))
  | Expr_name name -> (
      let loc = fst name in
      match NT.find_opt ctx.scope.fields name with
      | Some f ->
          assert (equal_name name f.name);
          mark_field_dependencies_as_used ctx f;
          f.ty, f.expr
      | None -> (
          let res =
            try_resolve_name ctx.scope name ~f:(fun n ->
                match Scope.Scopes.lookup' n ctx.scope.scopes with
                | Some ([], s) -> (
                    match NT.find_opt s.v.fields name with
                    | Some f -> Some (`field (n, s, f))
                    | None -> None)
                | Some (nav, s) ->
                    let expr =
                      List.fold_left nav ~init:(expr_name name)
                        ~f:(fun expr name -> expr_nav name expr)
                    in
                    Some (`expr expr)
                | None -> assert false)
          in
          match res with
          | `expr expr -> infer_expr ~ctx expr
          | `field (n, s, f) ->
              mark_field_as_used ctx s.v (Some n) f;
              nullable_lub s f.ty, expr_nav ~loc n (expr_name name)))
  | Expr_app (f, args) -> infer_expr_app ~ctx f args
  | Expr_in (es, select) ->
      let scope, select = infer_select ~ctx:ctx.query_ctx select in
      let row = get_select_row scope select in
      let es =
        let f e (_n, ty) = snd (check_expr ty ~ctx e) in
        match List.map2 es row ~f with
        | es -> es
        | exception Invalid_argument _ ->
            Report.errorf ~loc
              "number of expressions (%d) doesn't match number of columns (%d)"
              (List.length es) (List.length row)
      in
      non_null bool, expr_in es select
  | Expr_lit (Lit_int _) -> non_null int, expr
  | Expr_lit (Lit_string _) -> non_null string, expr
  | Expr_lit (Lit_bool _) -> non_null bool, expr
  | Expr_null -> Report.errorf ~loc "missing type annotation for NULL value"
  | Expr_param name ->
      (* first try to find if we've seen the param already *)
      let ty =
        match Params.find name ctx.query_ctx.params with
        | Some (Pty ty) -> ty
        | Some (Pty_variant _) ->
            Report.errorf ~loc
              "param ?%s is of a variant type, must be used with MATCH ... \
               WITH construct"
              (snd name)
        | Some (Pty_expr (ty, _)) -> ty
        | Some Pty_unknown | None ->
            Report.errorf ~loc "missing type annotation for a param"
      in
      ty, expr
  | Expr_match (param, xs) ->
      handle_param_match ~loc infer_expr ~ctx (param, xs)
  | Expr_ascribe (expr, Ty_val ty) -> check_expr ty ~ctx expr
  | Expr_ascribe ({ node = Expr_param name; _ }, Ty_expr ty) -> (
      let pty = Pty_expr (ty, ctx.scope) in
      match Params.add ~loc name pty ctx.query_ctx.params with
      | Pty ty -> ty, expr
      | Pty_expr (ty, _) -> ty, expr
      | Pty_unknown | Pty_variant _ -> assert false)
  | Expr_ascribe (expr, Ty_expr ty) ->
      Report.errorf ~loc:expr.loc "expected a param"

and check_expr ty ~ctx (expr : expr) : ty * expr =
  let loc = expr.loc in
  match expr.node with
  | Expr_null ->
      if Equal.poly ty.nullable `non_null then
        Report.errorf ~loc "expected value of type %s but got NULL"
          (ty_to_string ty);
      ty, expr
  | Expr_param name -> (
      let pty = Pty ty in
      match Params.add ~loc name pty ctx.query_ctx.params with
      | Pty ty -> ty, expr
      | Pty_expr (ty, _) -> ty, expr
      | Pty_unknown | Pty_variant _ -> assert false)
  | Expr_match (param, xs) ->
      let ty, expr =
        handle_param_match ~loc ~ctx (check_expr ty) (param, xs)
      in
      subsumes ~loc ~sup:ty ty, expr
  | _ ->
      let ty', expr = infer_expr ~ctx expr in
      subsumes ~loc ~sup:ty ty', expr

and handle_param_match ~loc ~ctx check_body (param, xs) =
  let f check_body (tag, args, body) =
    let ptys, ty, body =
      let prev = Params.remove_names args ctx.query_ctx.params in
      let ty, body = check_body ~ctx body in
      let ptys = Params.remove_names args ctx.query_ctx.params in
      Params.add_names prev ctx.query_ctx.params;
      ptys, ty, body
    in
    let pty =
      let args =
        List.map ptys ~f:(function _, None -> Pty_unknown | _, Some ty -> ty)
      in
      Pty_variant [ tag, args ]
    in
    ignore (Params.add ~loc param pty ctx.query_ctx.params : pty);
    ty, (tag, args, body)
  in
  match xs with
  | [] -> Report.errorf ~loc "missing MATCH ... WITH cases"
  | x :: xs ->
      let ty, x' = f check_body x in
      let ty, xs =
        List.fold_left xs ~init:(ty, [ x' ]) ~f:(fun (ty, xs) x ->
            let ty, x = f (check_expr (null ty.v)) x in
            ty, x :: xs)
      in
      ty, expr_match ~loc param (List.rev xs)

and handle_binop_prim ~ctx check_arg name args =
  let loc = fst name in
  match args with
  | [ ({ node = Expr_param _ | Expr_match _; _ } as x); y ] ->
      let ty, y = check_arg ~ctx y in
      let x = check_expr (null ty.v) ~ctx x in
      x, (ty, y)
  | [ x; ({ node = Expr_param _ | Expr_match _; _ } as y) ] ->
      let tx, x = check_arg ~ctx x in
      let y = check_expr (null tx.v) ~ctx y in
      (tx, x), y
  | [ x; y ] ->
      let tx, x = check_arg ~ctx x in
      let y = check_expr (null tx.v) ~ctx y in
      (tx, x), y
  | _ -> Report.errorf ~loc "expects two arguments" name

and infer_expr_app ~ctx name args =
  let loc = fst name in
  match snd name, args with
  | "count", [ x ] ->
      let _ty, x = infer_expr ~ctx x in
      non_null int, expr_app ~loc name [ x ]
  | "max", [ x ] | "min", [ x ] | "sum", [ x ] ->
      let ty, x = check_expr (null numeric) ~ctx x in
      ty, expr_app ~loc name [ x ]
  | "=", args | "<>", args | ">", args | ">=", args | "<", args | "<=", args ->
      let (tx, x), (ty, y) = handle_binop_prim ~ctx infer_expr name args in
      let t = ty_lub ~loc tx ty in
      { nullable = t.nullable; v = bool }, expr_app ~loc name [ x; y ]
  | "AND", args | "OR", args ->
      let (_, x), (_, y) = handle_binop_prim ~ctx infer_expr name args in
      non_null bool, expr_app ~loc name [ x; y ]
  | "-", [ x ] ->
      let ty, x = check_expr (null numeric) ~ctx x in
      ty, expr_app ~loc name [ x ]
  | "NOT", [ x ] ->
      let _, x = infer_expr ~ctx x in
      non_null bool, expr_app ~loc name [ x ]
  | "+", args | "-", args | "*", args | "/", args ->
      let (tx, x), (ty, y) =
        handle_binop_prim ~ctx (check_expr (null numeric)) name args
      in
      ty_lub ~loc tx ty, expr_app ~loc name [ x; y ]
  | "nullif", [ x; y ] ->
      let (tx, x), (ty, y) = handle_binop_prim ~ctx infer_expr name args in
      null tx.v, expr_app ~loc name [ x; y ]
  | "coalesce", [ x ] ->
      let ty, x = infer_expr ~ctx x in
      ty, expr_app ~loc name [ x ]
  | "coalesce", x :: xs ->
      let f (t, xs) x =
        let t', x =
          match x.node with
          | Expr_param _ | Expr_match _ -> check_expr (null t.v) ~ctx x
          | _ -> infer_expr ~ctx x
        in
        ty_glb ~loc t t', x :: xs
      in
      let t, x = infer_expr ~ctx x in
      let t, xs = List.fold_left xs ~init:(t, [ x ]) ~f in
      t, expr_app ~loc name (List.rev xs)
  | "IS NULL", [ x ] ->
      let _, x = infer_expr ~ctx x in
      non_null bool, expr_app ~loc name [ x ]
  | "IS NOT NULL", [ x ] ->
      let _, x = infer_expr ~ctx x in
      non_null bool, expr_app ~loc name [ x ]
  | "now", [] -> non_null datetime, expr_app ~loc name []
  | "toString", [ x ] ->
      let ty, x = infer_expr ~ctx x in
      nullable_lub ty (non_null string), expr_app ~loc name [ x ]
  | "datetime", [ x ] -> (
      match x.node with
      | Expr_lit (Lit_string d) ->
          if not (is_valid_datetime d) then
            Report.errorf ~loc:x.loc
              "invalid datetime %S, expected \"YYYY-MM-DDTHH:MM:SS\"" d;
          non_null datetime, x
      | _ ->
          let _, x = check_expr ~ctx (null string) x in
          null datetime, expr_app ~loc name [ x ])
  | "date", [ x ] -> (
      match x.node with
      | Expr_lit (Lit_string d) ->
          if not (is_valid_date d) then
            Report.errorf ~loc:x.loc "invalid date %S, expected \"YYYY-MM-DD\""
              d;
          non_null date, x
      | _ ->
          let _, x = check_expr ~ctx (null string) x in
          null date, expr_app ~loc name [ x ])
  | name, _ -> Report.errorf ~loc "no such function %s" name

and infer_select ~ctx (select : select) : Scope.scope * select =
  let {
    select_proj;
    select_from;
    select_where;
    select_group_by;
    select_having;
    select_order_by;
    select_is_open;
  } =
    select.node
  in
  let loc = select.loc in
  let scope, select_from =
    match select_from with
    | None -> Scope.scope_create ~is_open:select_is_open (), None
    | Some select_from ->
        let scopes, from = infer_from ~ctx select_from in
        let scope = Scope.scope_create ~is_open:select_is_open ~scopes () in
        scope, Some from
  in
  let select_where =
    Option.map
      (fun e ->
        snd
          (check_expr (null bool)
             ~ctx:(Expr_ctx.make ~is_used:true scope ctx)
             e))
      select_where
  in
  let select_group_by =
    Option.map
      (List.map ~f:(fun e ->
           snd (infer_expr ~ctx:(Expr_ctx.make ~is_used:true scope ctx) e)))
      select_group_by
  in
  let select_having =
    Option.map
      (fun e ->
        snd
          (check_expr (null bool)
             ~ctx:(Expr_ctx.make ~is_used:true scope ctx)
             e))
      select_having
  in
  let scopes = ref scope.scopes in
  let current_scope () = { scope with scopes = !scopes } in
  let select_proj =
    List.mapi select_proj ~f:(fun i f ->
        let scope = current_scope () in
        match f with
        | Field_fieldset { name; args; is_used } ->
            let loc = fst name in
            let fs =
              match NT.find_opt ctx.env name with
              | Some (F fs) -> fs
              | Some _ -> Report.errorf ~loc "not a fieldset: %s" (snd name)
              | None -> Report.errorf ~loc "no such fieldset: %s" (snd name)
            in
            let scope =
              match
                List.fold_left2 fs.fs_scopes args ~init:scope.scopes
                  ~f:(fun scopes (n, s) arg ->
                    let loc = fst (List.hd arg) in
                    let scope =
                      match Scope.Scopes.lookup_many arg scope.scopes with
                      | Some s -> s
                      | None ->
                          Report.errorf ~loc "no such table/query `%s`"
                            (List.map arg ~f:snd |> String.concat ~sep:".")
                    in
                    let scope' =
                      match s with Scope.S s -> s.v | A _ -> assert false
                    in
                    scope_subsumes ~loc scope ~sup:scope';
                    (n, Scope.A arg) :: scopes)
              with
              | exception Invalid_argument _ ->
                  Report.errorf ~loc
                    "fieldset ...%s expects %d arguments but %d provided"
                    (snd name) (List.length fs.fs_scopes) (List.length args)
              | scopes -> { scope with scopes }
            in
            List.map fs.fs_fields ~f:(fun (_ty, name, expr) ->
                let ctx = Expr_ctx.make ~is_used scope ctx in
                (* TODO: we can just rewrite prefix instead of doing a
                   typechecking *)
                let ty, expr = infer_expr ~ctx expr in
                ignore
                  (build_field ~is_generated:false ~is_used
                     ~dependencies:ctx.dependencies scope name expr ty
                    : field);
                Field { expr; name = Some name; is_used })
        | Field_with_scope (path, name) ->
            let next_scopes = (name, Scope.A path) :: scope.scopes in
            if Option.is_none (Scope.Scopes.lookup' name next_scopes) then
              Report.errorf ~loc "no such table/query `%s`"
                (List.map path ~f:snd |> String.concat ~sep:".");
            scopes := next_scopes;
            []
        | Field f ->
            let name =
              match f.name with
              | Some name when NT.mem scope.fields name ->
                  Report.errorf ~loc:(fst name)
                    "column `%s` is already defined" (snd name)
              | Some name -> Some name
              | None -> field_name f.expr
            in
            let name =
              match name with
              | Some name when not (NT.mem scope.fields name) -> name
              | Some _ | None -> loc, sprintf "_%i" (NT.length scope.fields)
            in
            let ctx = Expr_ctx.make ~is_used:f.is_used scope ctx in
            let ty, expr = infer_expr ~ctx f.expr in
            ignore
              (build_field ~is_generated:(not f.is_used) ~is_used:f.is_used
                 ~dependencies:ctx.dependencies scope name expr ty
                : field);
            if not f.is_used then []
            else [ Field { f with expr; name = Some name } ])
    |> List.flatten
  in
  let scope = current_scope () in
  ( scope,
    Syntax.select ~loc select_proj ?from:select_from ?where:select_where
      ?group_by:select_group_by ?having:select_having ?order_by:select_order_by
      ~is_open:select_is_open )

and infer_from ~ctx ((loc, from) : from pos) =
  match from with
  | From from ->
      let scope, name, from = infer_from_one ~ctx from in
      [ name, S scope ], (loc, From from)
  | From_join (from, right, kind, expr) ->
      let bindings, left = infer_from ~ctx from in
      let rscope, rname, right = infer_from_one ~ctx right in
      let _, expr =
        let scopes = (rname, Scope.S rscope) :: bindings in
        let scope = Scope.scope_create ~scopes () in
        check_expr (null bool)
          ~ctx:(Expr_ctx.make ~is_used:true scope ctx)
          expr
      in
      let bindings =
        match kind with
        | Join_inner -> (rname, Scope.S rscope) :: bindings
        | Join_left -> (rname, S (null rscope.v)) :: bindings
      in
      bindings, (loc, From_join (left, right, kind, expr))

and infer_from_one ~ctx (loc, from) =
  match from with
  | From_table (name, alias) -> (
      let alias = Option.value alias ~default:name in
      match NT.find_opt ctx.env name with
      | Some (T (scope, _)) -> non_null scope, alias, (loc, from)
      | Some (Q { scope; query = loc, Query_select select; _ }) ->
          let scope = Scope.fresh scope in
          non_null scope, alias, (loc, From_select (select, alias))
      | Some (Q q) ->
          Report.errorf ~loc:(fst q.query) "cannot select from %s" (snd name)
      | Some (F _) ->
          Report.errorf ~loc "cannot select from fieldset: %s" (snd name)
      | None -> Report.errorf ~loc "no such table %s" (snd name))
  | From_select (select, alias) ->
      let scope, select = infer_select ~ctx select in
      non_null scope, alias, (loc, From_select (select, alias))

and infer_insert ~ctx (insert : insert) =
  let loc = insert.loc in
  let { insert_table; insert_columns; insert_from; insert_on_conflict } =
    insert.node
  in
  let table_scope, table = env_find_table ctx.env insert_table in
  let insert_from, inner_scope =
    let find_field name =
      match NT.find_opt table_scope.fields name with
      | Some f -> f
      | None ->
          Report.errorf ~loc:(fst name) "no such column in table: %s"
            (snd name)
    in
    let arity_error ~loc len =
      Report.errorf ~loc
        "number of columns (%d) doesn't match number of expressions (%d)"
        (List.length insert_columns)
        len
    in
    match insert_from with
    | Insert_from_values rows ->
        let rows =
          List.map rows ~f:(fun row ->
              try
                List.map2 insert_columns row ~f:(fun col expr ->
                    let f = find_field col in
                    let ctx =
                      Expr_ctx.make ~is_used:true (Scope.scope_create ()) ctx
                    in
                    let ty, expr = check_expr ~ctx f.ty expr in
                    ignore (subsumes ~loc:expr.loc ty ~sup:f.ty : ty);
                    expr)
              with Invalid_argument _ -> arity_error ~loc (List.length row))
        in
        Insert_from_values rows, Scope.scope_create ()
    | Insert_from_select select ->
        let scope, select = infer_select ~ctx select in
        Check_agg.run scope Check_agg.fold#fold_select select;
        let row = get_select_row scope select in
        (match
           List.iter2 insert_columns row ~f:(fun col ((loc, _), ty) ->
               let f = find_field col in
               ignore (subsumes ~loc ty ~sup:f.ty : ty))
         with
        | exception Invalid_argument _ ->
            arity_error ~loc:select.loc (List.length row)
        | () -> ());
        Insert_from_select select, scope
  in
  let missing_columns =
    List.filter_map table.columns ~f:(fun (col : Ddl.column) ->
        if
          Equal.poly `non_null col.ty.nullable
          && (not col.autoincrement)
          && (not (Option.is_some col.default))
          && not (List.mem ~eq:equal_name (dummy_loc, col.name) insert_columns)
        then Some col.name
        else None)
  in
  if not (List.is_empty missing_columns) then
    Report.errorf ~loc "missing required columns: %s"
      (String.concat ~sep:", " missing_columns);
  {
    scope = Scope.scope_create ();
    inner_scope;
    params = ctx.params.params;
    row = [];
    query =
      ( loc,
        Query_insert
          (Syntax.insert ~loc ?on_conflict:insert_on_conflict insert_table
             insert_columns insert_from) );
  }

and infer_delete ~ctx (delete : delete) =
  let loc = delete.loc in
  let { delete_table; delete_where } = delete.node in
  let table_scope = fst (env_find_table ctx.env delete_table) in
  let delete_where =
    match delete_where with
    | None -> None
    | Some e ->
        let scope =
          Scope.scope_create
            ~scopes:[ delete_table, Scope.S (non_null table_scope) ]
            ()
        in
        let ctx = Expr_ctx.make ~is_used:true scope ctx in
        let _, e = check_expr (null bool) ~ctx e in
        Some e
  in
  {
    scope = Scope.scope_create ();
    inner_scope = Scope.scope_create ();
    params = ctx.params.params;
    row = [];
    query =
      loc, Query_delete (Syntax.delete ~loc ?where:delete_where delete_table);
  }

and infer_update ~ctx (update : update) =
  let loc = update.loc in
  let { update_table; update_set; update_where; update_from } = update.node in
  let table_scope = fst (env_find_table ctx.env update_table) in
  let scopes, update_from =
    match update_from with
    | None -> [], None
    | Some from ->
        let scopes, from = infer_from ~ctx from in
        scopes, Some from
  in
  let scopes = (update_table, Scope.S (non_null table_scope)) :: scopes in
  let scope = Scope.scope_create ~scopes () in
  let update_where =
    update_where
    |> Option.map
         (check_expr (null bool) ~ctx:(Expr_ctx.make ~is_used:true scope ctx))
    |> Option.map snd
  in
  let update_set =
    List.map update_set ~f:(fun (name, expr) ->
        let f =
          match NT.find_opt table_scope.fields name with
          | Some f -> f
          | None ->
              Report.errorf ~loc:(fst name) "no such column: %s" (snd name)
        in
        let ctx = Expr_ctx.make ~is_used:true scope ctx in
        let ty, expr = check_expr ~ctx f.ty expr in
        ignore (subsumes ~loc:expr.loc ty ~sup:f.ty : ty);
        name, expr)
  in
  Option.iter
    (Check_agg.run (Scope.scope_create ~scopes ()) Check_agg.fold#fold_from)
    update_from;
  {
    scope = Scope.scope_create ();
    inner_scope = scope;
    params = ctx.params.params;
    row = [];
    query =
      ( loc,
        Query_update
          (Syntax.update ~loc update_table update_set ?from:update_from
             ?where:update_where) );
  }

and infer_query ~ctx (loc, query) =
  match query with
  | Query_select select ->
      let scope, select = infer_select ~ctx select in
      Check_agg.run scope Check_agg.fold#fold_select select;
      let row = get_select_row scope select in
      {
        scope;
        inner_scope = scope;
        params = ctx.params.params;
        row;
        query = loc, Query_select select;
      }
  | Query_insert insert -> infer_insert ~ctx insert
  | Query_update update -> infer_update ~ctx update
  | Query_delete delete -> infer_delete ~ctx delete

let analyze_query ?src (db : env) query =
  Report.with_src ?src @@ fun () ->
  let ctx = make_query_ctx db in
  infer_query ~ctx query

let analyze_fieldset ?src (db : env) (_loc, fieldset) =
  Report.with_src ?src @@ fun () ->
  let fs_scopes =
    List.fold_left fieldset.fieldset_args ~init:[]
      ~f:(fun bindings (name, sty) ->
        let scope = sty_to_scope ?src db sty in
        (name, Scope.S (non_null scope)) :: bindings)
  in
  let scope = Scope.scope_create ~scopes:fs_scopes () in
  let ctx = make_query_ctx db in
  let ctx = Expr_ctx.make ~is_used:true scope ctx in
  let fs_fields =
    List.map fieldset.fieldset_exprs ~f:(fun (name, expr) ->
        let ty, _expr = infer_expr ~ctx expr in
        ty, name, expr)
  in
  { fs_scopes; fs_fields }

let analyze_expr ?ty ?scope ?src (db : env) expr =
  Report.with_src ?src @@ fun () ->
  let ctx = make_query_ctx db in
  let scope = Option.get_lazy Scope.scope_create scope in
  let ctx = Expr_ctx.make ~is_used:true scope ctx in
  let _ty, expr =
    match ty with
    | None -> infer_expr ~ctx expr
    | Some ty -> check_expr ~ctx ty expr
  in
  expr
