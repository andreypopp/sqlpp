open Sqlpp
open Syntax
open Analyze
open Ppxlib
open Ppxlib.Ast_builder.Default

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

module type CONFIG = sig
  module Sqlpp_db : Sqlpp.BACKEND
end

module Make (C : CONFIG) : sig
  val env : Sqlpp.Env.t
  val register : unit -> unit
end = struct
  let env = Sqlpp.Env.create ()

  module Sqlpp_db = C.Sqlpp_db

  let sqlpp_backend ~loc name =
    let txt = Longident.parse (sprintf "Sqlpp_db.Db.%s" name) in
    pexp_ident ~loc { loc; txt }

  let rec print_to_expression =
    let printer =
      object (self)
        inherit [Printer_ctx.t * pty NM.t] Sqlpp_db.Db.printer
        method emit { ctx = p, ps; _ } s = Printer_ctx.emit_string p s

        method emit_Expr_param ({ ctx = p, ps; _ } as ctx) name =
          match NM.find_opt name ps with
          | None -> failwith (sprintf "no type for %s" (snd name))
          | Some Pty_unknown -> self#emit ctx "NULL"
          | Some (Pty_variant _) -> assert false
          | Some (Pty_expr _) -> assert false
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
                    sqlpp_backend ~loc:(fst t) (sprintf "encode_%s" (snd t))
                | `null, Ty t ->
                    sqlpp_backend ~loc:(fst t)
                      (sprintf "encode_%s_NULL" (snd t))
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

  let gen_ty ~loc ({ nullable; v } as ty : ty) =
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
    | Pty ty -> gen_ty ~loc ty
    | Pty_variant cases ->
        let cases =
          List.map cases ~f:(fun (tag, ptys) ->
              let tys = List.map ~f:(gen_pty ~loc) ptys in
              rtag ~loc (name_to_label tag) false tys)
        in
        ptyp_variant ~loc cases Closed None
    | Pty_expr (ty, expr) ->
        let ty = gen_ty ~loc ty in
        [%type: unit -> [%t ty] Sqlpp.expr]
    | Pty_unknown ->
        let loc = dummy_loc in
        [%type: unit]

  let gen_decode ~loc (ty : ty) =
    match ty.nullable, ty.v with
    | _, Ty_one_of _ ->
        Report.errorf ~loc "no concrete type %s was inferred" (ty_to_string ty)
    | `non_null, Ty t -> sqlpp_backend ~loc (sprintf "decode_%s" (snd t))
    | `null, Ty t -> sqlpp_backend ~loc (sprintf "decode_%s_NULL" (snd t))

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
        ptyp_arrow ~loc (Labelled (snd p)) (gen_ty ~loc rty) ty)

  let gen_query_to_sql_ty params query k =
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
                match query.query with
                | _, Query_select _ ->
                    [%expr
                      fun () ->
                        let decode = [%e gen_fold query.row] in
                        { Sqlpp_db.sql = [%e sql]; decode }]
                | _ -> [%expr fun () -> { Sqlpp_db.sql = [%e sql] }])
        | `test -> [%expr assert false]
      in
      let t =
        match query.query with
        | _, Query_select _ ->
            let t =
              [%type:
                unit -> ([%t gen_fold_ty query.row], 'acc) Sqlpp_db.query]
            in
            gen_query_to_sql_ty query.params query.query [%type: [%t t]]
        | _ ->
            gen_query_to_sql_ty query.params query.query
              [%type: unit -> Sqlpp_db.stmt]
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
                [%e k_acc [%expr Sqlpp_db.fold db q ~init:[] ~f:Stdlib.Fun.id]]])
      in
      let t =
        let t =
          match typ with
          | None ->
              ptyp_tuple ~loc
                (List.map query.row ~f:(fun (n, ty) -> gen_ty ~loc:(fst n) ty))
          | Some t -> ptyp_constr ~loc t []
        in
        let t = [%type: Sqlpp_db.Db.db -> [%t k_typ t]] in
        gen_query_to_sql_ty query.params query.query [%type: [%t t]]
      in
      [%expr ([%e e] : [%t t])]
    with Sqlpp.Report.Error report ->
      let error = estring ~loc report.msg in
      let loc = report.loc in
      [%expr [%ocaml.error [%e error]]]

  let fetch_list_expression ~ctxt ((src, _) as x) =
    let loc = src.loc in
    fetch_list_expression' ~ctxt x
      ~k_typ:(fun t -> [%type: [%t t] list])
      ~k_acc:(fun e -> [%expr List.rev [%e e]])

  let fetch_option_expression ~ctxt ((src, _) as x) =
    let loc = src.loc in
    (* TODO: need to make it throw on 2nd element and not wait till the whole
       list is computed *)
    fetch_list_expression' ~ctxt x
      ~k_typ:(fun t -> [%type: [%t t] option])
      ~k_acc:(fun acc ->
        [%expr
          match [%e acc] with
          | [] -> None
          | [ x ] -> Some x
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
        gen_query_to_sql_ty query.params query.query
          [%type: Sqlpp_db.Db.db -> unit]
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
