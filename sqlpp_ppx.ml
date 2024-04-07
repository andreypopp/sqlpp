open Sqlpp
open Syntax
open Analyze
open Ppxlib
open Ppxlib.Ast_builder.Default

let enullable gen_v ~loc { nullable; v } =
  let nullable =
    match nullable with
    | `non_null -> [%expr `non_null]
    | `null -> [%expr `null]
  in
  [%expr { Sqlpp.Syntax.nullable = [%e nullable]; v = [%e gen_v ~loc v] }]

let eoption gen ~loc = function
  | None -> [%expr None]
  | Some e -> [%expr Some [%e gen ~loc e]]

let ename ~loc:_ (loc, name) = [%expr Sqlpp.Syntax.name [%e estring ~loc name]]

let epair gen1 gen2 ~loc (e1, e2) =
  [%expr [%e gen1 ~loc e1], [%e gen2 ~loc e2]]

let elist gen ~loc l = elist ~loc (List.map ~f:(fun e -> gen ~loc e) l)

let etysyn ~loc = function
  | Ty name -> [%expr Sqlpp.Syntax.Ty [%e ename ~loc name]]
  | Ty_one_of (name, names) ->
      [%expr
        Sqlpp.Syntax.Ty_one_of
          ([%e eoption estring ~loc name], [%e elist ename ~loc names])]

let elit ~loc (lit : lit) =
  match lit with
  | Lit_int v -> [%expr Sqlpp.Syntax.Lit_int [%e eint ~loc v]]
  | Lit_string v -> [%expr Sqlpp.Syntax.Lit_string [%e estring ~loc v]]
  | Lit_bool v -> [%expr Sqlpp.Syntax.Lit_bool [%e ebool ~loc v]]

exception Not_supported of string

let rec eexpr ~loc (expr : expr) =
  match expr.node with
  | Expr_app (name, args) ->
      [%expr
        Sqlpp.Syntax.expr_app [%e ename ~loc name] [%e elist eexpr ~loc args]]
  | Expr_name name -> [%expr Sqlpp.Syntax.expr_name [%e ename ~loc name]]
  | Expr_lit lit -> [%expr Sqlpp.Syntax.expr_lit [%e elit ~loc lit]]
  | Expr_nav (name, expr) ->
      [%expr Sqlpp.Syntax.expr_nav [%e ename ~loc name] [%e eexpr ~loc expr]]
  | Expr_in (_, _) -> raise (Not_supported "Expr_in")
  | Expr_ascribe (expr, _ty) -> eexpr ~loc expr
  | Expr_param _ -> raise (Not_supported "Expr_param")
  | Expr_match (_, _) -> raise (Not_supported "Expr_match")
  | Expr_null -> [%expr Sqlpp.Syntax.expr_null ()]

let efield ~loc
    ({ name; expr; ty; is_generated; is_used; dependencies } : field) =
  [%expr
    {
      Sqlpp.Syntax.name = [%e ename ~loc name];
      expr = [%e eexpr ~loc expr];
      ty = [%e enullable etysyn ~loc ty];
      is_generated = [%e ebool ~loc is_generated];
      is_used = [%e ebool ~loc is_used];
      dependencies = [%e elist (epair (eoption ename) ename) ~loc dependencies];
    }]

let rec escope ~loc (scope : Scope.scope) =
  let gen_scope_elem ~loc = function
    | Scope.S scope -> enullable escope ~loc scope
    | A names -> elist ename ~loc names
  in
  let fields =
    let fields =
      NT.fold
        (fun k v next ->
          match efield ~loc v with
          | exception Not_supported _ -> next
          | v ->
              [%expr
                Sqlpp.Syntax.NT.add fields [%e ename ~loc k] [%e v];
                [%e next]])
        scope.fields [%expr fields]
    in
    [%expr
      let fields = Syntax.NT.create () in
      [%e fields]]
  in
  [%expr
    {
      Sqlpp.Analyze.scopes =
        [%e elist (epair ename gen_scope_elem) ~loc scope.scopes];
      fields = [%e fields];
      is_open = [%e ebool ~loc scope.is_open];
    }]

module Printer_ctx = struct
  type t = { mutable ops : op list }

  and op =
    | S of Buffer.t  (** raw sql string *)
    | E of Parsetree.expression
    | B of
        Parsetree.expression * (Parsetree.pattern * Parsetree.expression) list

  let create () = { ops = [] }
  let emit printer op = printer.ops <- op :: printer.ops

  let emit_string printer s =
    match printer.ops with
    | S buf :: xs ->
        Buffer.add_string buf s;
        printer.ops <- S buf :: xs
    | _ ->
        let buf = Buffer.create 100 in
        Buffer.add_string buf s;
        emit printer (S buf)

  let emit_seq emit printer exprs =
    Seq.iteri
      (fun i expr ->
        if i > 0 then emit_string printer ", ";
        emit printer expr)
      exprs

  let emit_list emit printer exprs = emit_seq emit printer (List.to_seq exprs)
  let emit_stringf printer fmt = Printf.ksprintf (emit_string printer) fmt

  open Ppxlib.Ast_builder.Default

  let to_expression ~loc p =
    let rec loop = function
      | [] -> [%expr ()]
      | S s :: xs ->
          [%expr
            Buffer.add_string buf [%e estring ~loc (Buffer.contents s)];
            [%e loop xs]]
      | E e :: xs ->
          [%expr
            Buffer.add_string buf [%e e];
            [%e loop xs]]
      | B (e, cs) :: xs ->
          let e =
            pexp_match ~loc e
              (List.map cs ~f:(fun (pc_lhs, pc_rhs) ->
                   { Parsetree.pc_lhs; pc_guard = None; pc_rhs }))
          in
          [%expr
            [%e e];
            [%e loop xs]]
    in
    loop (List.rev p.ops)
end

let name_to_lident name =
  pexp_ident ~loc:(fst name) { txt = Lident (snd name); loc = fst name }

let name_to_label (loc, txt) = { Location.loc; txt }

module Make (Db : DB) : sig
  val env : Sqlpp.Env.t
  val register : unit -> unit
end = struct
  let env = Sqlpp.Env.create ()

  let sqlpp_types ~loc name =
    let txt = Longident.parse (sprintf "Sqlpp_types.%s" name) in
    pexp_ident ~loc { loc; txt }

  let rec print_to_expression =
    let printer =
      object (self)
        inherit [Printer_ctx.t * pty NM.t] Db.printer
        method emit { ctx = p, ps; _ } s = Printer_ctx.emit_string p s

        method emit_Expr_param ({ ctx = p, ps; _ } as ctx) name =
          match NM.find_opt name ps with
          | None -> failwith (sprintf "no type for %s" (snd name))
          | Some Pty_unknown -> self#emit ctx "NULL"
          | Some (Pty_variant _) -> assert false
          | Some (Pty_expr (ty, scope)) ->
              let loc = fst name in
              Printer_ctx.emit p
                (E
                   [%expr
                     let scope = [%e escope ~loc scope] in
                     let expr = [%e ename ~loc name] () in
                     let env = Sqlpp.Env.create () in
                     let expr = Sqlpp.Analyze.analyze_expr ~scope expr env in
                     Sqlpp_db.To_sql.expr_to_sql expr])
          | Some (Pty ty) ->
              let id = name_to_lident name in
              let encode =
                match ty.nullable, ty.v with
                | _, Ty_one_of _ ->
                    Report.errorf ~loc:(fst name)
                      "no concrete type was not inferred %s for param %s, add \
                       type annotation"
                      (ty_to_string ty) (snd name)
                | `non_null, Ty t ->
                    sqlpp_types ~loc:(fst t) (sprintf "encode_%s" (snd t))
                | `null, Ty t ->
                    sqlpp_types ~loc:(fst t) (sprintf "encode_%s_NULL" (snd t))
              in
              let loc = fst name in
              Printer_ctx.emit p (E [%expr [%e encode] [%e id]])

        method emit_Expr_match ({ ctx = p, ps; _ } as ctx) name cases =
          let id = name_to_lident name in
          let ty_cases =
            match NM.find_opt name ps with
            | Some (Pty_variant ty_cases) -> ty_cases
            | None | Some Pty_unknown | Some (Pty _) | Some (Pty_expr _) ->
                assert false
          in
          let cases =
            List.map cases ~f:(fun (tag, args, expr) ->
                let loc = fst tag in
                let ps =
                  let tys =
                    List.Assoc.get ~eq:equal_name tag ty_cases
                    |> Option.get_exn_or "analyze should have checked this"
                  in
                  List.fold_left2 args tys ~init:ps ~f:(fun ps arg ty ->
                      NM.add arg ty ps)
                in
                let pat =
                  ppat_variant ~loc (snd tag)
                    (match args with
                    | [] -> None
                    | args ->
                        let args =
                          List.map args ~f:(fun arg ->
                              ppat_var ~loc:(fst arg) (name_to_label arg))
                        in
                        Some (ppat_tuple ~loc args))
                in
                let pp = Printer_ctx.create () in
                self#emit_expr { ctx with ctx = pp, ps } expr;
                pat, Printer_ctx.to_expression ~loc pp)
          in
          Printer_ctx.emit p (B (id, cases))
      end
    in
    fun (q : query_info) ->
      let p = Printer_ctx.create () in
      printer#emit_query { ctx = p, q.params; scope = q.scope } q.query;
      Printer_ctx.to_expression ~loc:(fst q.query) p

  let sort_params params =
    NM.to_seq params
    |> Seq.sort ~cmp:(Ord.map fst compare_name)
    |> List.of_seq_rev

  let with_dummy_loc (expr : Parsetree.expression) =
    let loc = expr.pexp_loc in
    [%expr Sqlpp.Syntax.dummy_loc, [%e expr]]

  let gent_ty ~loc ({ nullable; v } as ty : ty) =
    match v with
    | Ty_one_of _ ->
        Report.errorf ~loc
          "no concrete type %s was inferred, add type annotation"
          (ty_to_string ty)
    | Ty (loc, ty) -> (
        let ty = String.lowercase_ascii ty in
        let ty = ptyp_constr ~loc { txt = Longident.Lident ty; loc } [] in
        match nullable with
        | `null -> [%type: [%t ty] option]
        | `non_null -> ty)

  let rec gen_pty ~loc = function
    | Pty ty -> gent_ty ~loc ty
    | Pty_variant cases ->
        let cases =
          List.map cases ~f:(fun (tag, ptys) ->
              let tys = List.map ~f:(gen_pty ~loc) ptys in
              rtag ~loc (name_to_label tag) false tys)
        in
        ptyp_variant ~loc cases Closed None
    | Pty_expr (ty, expr) ->
        let ty = gent_ty ~loc ty in
        [%type: unit -> [%t ty] Sqlpp.expr]
    | Pty_unknown ->
        let loc = dummy_loc in
        [%type: unit]

  let gen_decode ~loc (ty : ty) =
    match ty.nullable, ty.v with
    | _, Ty_one_of _ ->
        Report.errorf ~loc "no concrete type %s was inferred" (ty_to_string ty)
    | `non_null, Ty t -> sqlpp_types ~loc (sprintf "decode_%s" (snd t))
    | `null, Ty t -> sqlpp_types ~loc (sprintf "decode_%s_NULL" (snd t))

  let gen_fold' (ret_ty : Analyze.row) init =
    let loc = dummy_loc in
    let ret_ty_len = List.length ret_ty in
    let rev_ret_ty = List.rev ret_ty in
    let body =
      List.foldi rev_ret_ty ~init ~f:(fun next i (n, ty) ->
          let loc = fst n in
          let pat = ppat_var ~loc (name_to_label n) in
          [%expr
            let [%p pat] =
              [%e gen_decode ~loc:(fst n) ty]
                row
                [%e eint ~loc (ret_ty_len - i - 1)]
            in
            [%e next]])
    in
    [%expr fun f row acc -> [%e body]]

  let gen_fold (row : Analyze.row) =
    let loc = dummy_loc in
    gen_fold' row
      (let args =
         let init = [ Asttypes.Nolabel, [%expr acc] ] in
         List.fold_left (List.rev row) ~init ~f:(fun args (n, ty) ->
             let loc = fst n in
             ( Asttypes.Labelled (snd n),
               pexp_ident ~loc { loc; txt = Lident (snd n) } )
             :: args)
       in
       pexp_apply ~loc [%expr f] args)

  let gen_fold_ty (ret_ty : Analyze.row) =
    let loc = dummy_loc in
    let fty = [%type: 'acc -> 'acc] in
    List.fold_left (List.rev ret_ty) ~init:fty ~f:(fun ty (p, rty) ->
        let loc = fst p in
        ptyp_arrow ~loc (Labelled (snd p)) (gent_ty ~loc rty) ty)

  let gen_query_to_sql_ty params k =
    List.fold_left (sort_params params) ~init:k ~f:(fun ty (p, pty) ->
        let loc = fst p in
        ptyp_arrow ~loc (Labelled (snd p)) (gen_pty ~loc pty) ty)

  let gen_query_to_sql q k =
    let loc = fst q.query in
    let to_string =
      let e = print_to_expression q in
      [%expr
        let sql =
          let buf = Buffer.create 100 in
          [%e e];
          Buffer.contents buf
        in
        [%e k [%expr sql]]]
    in
    List.fold_left (sort_params q.params) ~init:to_string ~f:(fun e (p, _) ->
        let loc = fst p in
        pexp_fun ~loc
          (Labelled (snd p))
          None
          (ppat_var ~loc (name_to_label p))
          e)

  let mode : [ `regular | `test ] ref = ref `regular
  let requires = ref []
  let print_query = ref false

  let query_expression ~ctxt src =
    let loc = src.loc in
    let src = src.txt in
    try
      let query = Sqlpp.parse_query ~pos:loc.loc_start src in
      let query = Sqlpp.Analyze.analyze_query ~src env query in
      let () =
        match !print_query with
        | true ->
            let buf = Buffer.create 100 in
            PPrint.ToBuffer.pretty 0.8 60 buf
              (Sqlpp.print_query ~scope:query.scope query.query);
            prerr_endline (Buffer.contents buf)
        | false -> ()
      in
      let e =
        match !mode with
        | `regular ->
            gen_query_to_sql query (fun sql ->
                match query.row with
                | [] -> [%expr fun () -> { Sqlpp_db.sql = [%e sql] }]
                | row ->
                    [%expr
                      fun () ->
                        let decode = [%e gen_fold row] in
                        { Sqlpp_db.sql = [%e sql]; decode }])
        | `test -> [%expr assert false]
      in
      let t =
        match query.row with
        | [] -> gen_query_to_sql_ty query.params [%type: unit -> Sqlpp_db.stmt]
        | row ->
            let t =
              [%type: unit -> ([%t gen_fold_ty row], 'acc) Sqlpp_db.query]
            in
            gen_query_to_sql_ty query.params [%type: [%t t]]
      in
      [%expr ([%e e] : [%t t])]
    with Sqlpp.Report.Error report ->
      let error = estring ~loc report.msg in
      let loc = report.loc in
      [%expr [%ocaml.error [%e error]]]

  let fetch_list_expression' ~k_acc ~k_typ ~ctxt (src, typ) =
    let loc = src.loc in
    let src = src.txt in
    try
      let query = Sqlpp.parse_query ~pos:loc.loc_start src in
      let query = Sqlpp.Analyze.analyze_query ~src env query in
      let e =
        let fold =
          let loc = dummy_loc in
          gen_fold' query.row
            (let e =
               match typ with
               | None ->
                   pexp_tuple ~loc
                     (List.map query.row ~f:(fun (n, _) -> name_to_lident n))
               | Some t ->
                   let e =
                     pexp_record ~loc:t.loc
                       (List.map query.row ~f:(fun (n, _) ->
                            ( { txt = Lident (snd n); loc = fst n },
                              name_to_lident n )))
                       None
                   in
                   let t = ptyp_constr ~loc t [] in
                   [%expr ([%e e] : [%t t])]
             in
             [%expr [%e e] :: acc])
        in
        gen_query_to_sql query (fun sql ->
            [%expr
              fun db ->
                let decode = [%e fold] in
                let q = { Sqlpp_db.sql = [%e sql]; decode } in
                Sqlpp_db.IO.( >>= )
                  (Sqlpp_db.fold db q ~init:[] ~f:Stdlib.Fun.id) (fun e ->
                    [%e k_acc [%expr e]])])
      in
      let t =
        let t =
          match typ with
          | None ->
              ptyp_tuple ~loc
                (List.map query.row ~f:(fun (n, ty) -> gent_ty ~loc:(fst n) ty))
          | Some t -> ptyp_constr ~loc t []
        in
        let t = [%type: Sqlpp_db.db -> [%t k_typ t]] in
        gen_query_to_sql_ty query.params [%type: [%t t]]
      in
      [%expr ([%e e] : [%t t])]
    with Sqlpp.Report.Error report ->
      let error = estring ~loc report.msg in
      let loc = report.loc in
      [%expr [%ocaml.error [%e error]]]

  let fetch_list_expression ~ctxt ((src, _) as x) =
    let loc = src.loc in
    fetch_list_expression' ~ctxt x
      ~k_typ:(fun t -> [%type: [%t t] list Sqlpp_db.IO.t])
      ~k_acc:(fun e -> [%expr Sqlpp_db.IO.return (List.rev [%e e])])

  let fetch_option_expression ~ctxt ((src, _) as x) =
    let loc = src.loc in
    (* TODO: need to make it throw on 2nd element and not wait till the whole
       list is computed *)
    fetch_list_expression' ~ctxt x
      ~k_typ:(fun t -> [%type: [%t t] option Sqlpp_db.IO.t])
      ~k_acc:(fun acc ->
        [%expr
          match [%e acc] with
          | [] -> Sqlpp_db.IO.return None
          | [ x ] -> Sqlpp_db.IO.return (Some x)
          | _ -> failwith "expected one row"])

  let exec_expression ~ctxt src =
    let loc = src.loc in
    let src = src.txt in
    try
      let query = Sqlpp.parse_query ~pos:loc.loc_start src in
      let query = Sqlpp.Analyze.analyze_query ~src env query in
      let e =
        gen_query_to_sql query (fun sql ->
            [%expr fun db -> Sqlpp_db.exec db { Sqlpp_db.sql = [%e sql] }])
      in
      let t =
        gen_query_to_sql_ty query.params
          [%type: Sqlpp_db.db -> unit Sqlpp_db.IO.t]
      in
      [%expr ([%e e] : [%t t])]
    with Sqlpp.Report.Error report ->
      let error = estring ~loc report.msg in
      let loc = report.loc in
      [%expr [%ocaml.error [%e error]]]

  let as_expression name expand_expression =
    let pattern =
      let open Ast_pattern in
      single_expr_payload (estring __')
    in
    Context_free.Rule.extension
      (Extension.V3.declare name Extension.Context.expression pattern
         expand_expression)

  let as_expression_with_record_arg name expand_expression =
    let pattern =
      let open Ast_pattern in
      let for_record =
        single_expr_payload
          (pexp_apply (estring __')
             (pair (labelled (string "record")) (pexp_ident __') ^:: nil))
        |> map2 ~f:(fun src typ -> src, Some typ)
      in
      let for_tuple =
        single_expr_payload (estring __') |> map1 ~f:(fun src -> src, None)
      in
      for_record ||| for_tuple
    in
    Context_free.Rule.extension
      (Extension.V3.declare name Extension.Context.expression pattern
         expand_expression)

  let as_structure_item name expand_expression =
    let pattern =
      let open Ast_pattern in
      let extractor_in_let =
        pstr_value drop
          (value_binding ~pat:(ppat_var __') ~expr:(estring __') ^:: nil)
      in
      pstr @@ extractor_in_let ^:: nil
    in
    let expand ~ctxt name src =
      let loc = name.loc in
      let p = ppat_var ~loc:name.loc name in
      let e = expand_expression ~ctxt src in
      [%stri let [%p p] = [%e e]]
    in
    Context_free.Rule.extension
      (Extension.V3.declare name Extension.Context.structure_item pattern
         expand)

  let query_structure_item () =
    as_structure_item "sqlpp.query" query_expression

  let exec_structure_item () = as_structure_item "sqlpp.exec" exec_expression
  let query_expression () = as_expression "sqlpp.query" query_expression
  let exec_expression () = as_expression "sqlpp.exec" exec_expression

  let fetch_list_expression () =
    as_expression_with_record_arg "sqlpp.fetch_list" fetch_list_expression

  let fetch_option_expression () =
    as_expression_with_record_arg "sqlpp.fetch_option" fetch_option_expression

  let env_structure_item ~ctxt src y =
    let loc = src.loc in
    try
      let loc' =
        { loc.loc_start with pos_cnum = loc.loc_start.pos_cnum + 1 }
      in
      Sqlpp.Env.add ~loc:loc' env src.txt;
      [%stri let () = ()]
    with Sqlpp.Report.Error report ->
      let error = estring ~loc report.msg in
      let loc = report.loc in
      [%stri [%%ocaml.error [%e error]]]

  let env_structure_item () =
    let pattern =
      let open Ast_pattern in
      pstr @@ pstr_eval (estring __') __ ^:: nil
    in
    Context_free.Rule.extension
      (Extension.V3.declare "sqlpp.env" Extension.Context.structure_item
         pattern env_structure_item)

  let register () =
    Driver.add_arg "-test" (Unit (fun () -> mode := `test)) ~doc:"test mode";
    Driver.add_arg "-require"
      (String (fun require -> requires := require :: !requires))
      ~doc:"require definitions";
    Driver.add_arg "-print"
      (Unit (fun () -> print_query := true))
      ~doc:"print query";
    Driver.register_transformation
      ~rules:
        [
          query_structure_item ();
          query_expression ();
          exec_expression ();
          exec_structure_item ();
          fetch_list_expression ();
          fetch_option_expression ();
          env_structure_item ();
        ]
      "sqlpp"
end
