module Report = Report
module Syntax = Syntax
module Analyze = Analyze
module Printer = Printer
module Ddl = Ddl

let parse_with parse ?pos ?(fname = Report.no_fname) src =
  let lexbuf = Sedlexing.Utf8.from_string src in
  Sedlexing.set_filename lexbuf fname;
  Sedlexing.set_position lexbuf
    (match pos with
    | None -> { pos_lnum = 1; pos_cnum = 0; pos_bol = 0; pos_fname = fname }
    | Some pos -> pos);
  let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised parse in
  try parser lexer with
  | Parser.Error ->
      let loc_start, loc_end = Sedlexing.lexing_positions lexbuf in
      Report.reraise
        (Report.errorf
           ~loc:{ Warnings.loc_start; loc_end; loc_ghost = false }
           ~src "unexpected token")
  | Lexer.Error (p, msg) ->
      let loc = { Warnings.loc_start = p; loc_end = p; loc_ghost = false } in
      Report.reraise (Report.errorf ~loc ~src "lexer error: %s" msg)

let parse_decls = parse_with Parser.decl_many
let parse_query = parse_with Parser.query_one
let parse_expr = parse_with Parser.expr_one

let format_with_scope =
  let open Syntax in
  object
    inherit [Analyze.scope] format as super

    method! format_From_select scope select name =
      let scope =
        Analyze.scope_subscope scope name |> Option.get_exn_or "missing scope"
      in
      super#format_From_select scope select name

    method! format_fields scope fields =
      let fields =
        Seq.append fields
          (Analyze.scope_fields scope
          |> Seq.filter_map (fun f ->
                 if f.is_generated then
                   Some
                     (Field
                        {
                          name = Some f.name;
                          expr = f.expr;
                          is_used = f.is_used;
                        })
                 else None))
        |> Seq.uniq equal_select_field
      in
      super#format_fields scope fields
  end

let format =
  object
    inherit [unit] Syntax.format
  end

let print_query ?scope q =
  match scope with
  | None -> format#format_query () q
  | Some scope -> format_with_scope#format_query scope q

let print_expr e = format#format_expr e

module Env = struct
  open Syntax

  type t = Analyze.env

  let create () = NT.create 10

  let add' ?ddl ?src (env : t) (loc, decl) =
    let raise_duplicate name =
      Report.(
        reraise (errorf ~loc:(fst name) "name %s already defined" (snd name)))
    in
    match decl with
    | Syntax.Decl_table (name, cols) ->
        if NT.mem env name then raise_duplicate name;
        let fields =
          List.map cols ~f:(fun (n, ty) ->
              let e = expr_name n in
              make_field ~is_used:true ~is_generated:false n e ty)
        in
        let ddl =
          match ddl with
          | Some ddl -> ddl
          | None ->
              let columns =
                List.map cols ~f:(fun (n, ty) -> Ddl.column (snd n) ty)
              in
              Ddl.table (snd name) columns
        in
        NT.replace env name (T (Analyze.scope_create ~fields (), ddl))
    | Syntax.Decl_query (name, query) ->
        let q = Analyze.analyze_query ?src env query in
        if NT.mem env name then raise_duplicate name;
        NT.replace env name (Q q)
    | Syntax.Decl_fieldset (name, fs) ->
        let fs = Analyze.analyze_fieldset ?src env fs in
        if NT.mem env name then raise_duplicate name;
        NT.replace env name (F fs)

  let add_table env t cols = add' env (dummy_loc, Syntax.Decl_table (t, cols))

  let add ?loc env src =
    let decls = parse_decls ?pos:loc src in
    List.iter decls ~f:(add' ~src env)

  let add_file env file =
    let src = In_channel.(with_open_bin file input_all) in
    add env src

  let add_ddl env =
    let replace_table table f =
      match Syntax.NT.find_opt env (Syntax.name table) with
      | Some (Analyze.T (t, ddl)) -> (
          Syntax.NT.remove env (Syntax.name table);
          match f t ddl with
          | None -> ()
          | Some (name, t, ddl) ->
              Syntax.NT.replace env (Syntax.name name) (T (t, ddl)))
      | Some _ -> Report.errorf "not a table: %s" table
      | None -> Report.errorf "no such table: %s" table
    in
    function
    | Ddl.CREATE_TABLE table ->
        let columns =
          List.map table.columns ~f:(fun (col : Ddl.column) ->
              Syntax.name col.name, col.ty)
        in
        add' ~ddl:table env
          (dummy_loc, Syntax.Decl_table (Syntax.name table.name, columns))
    | DROP_TABLE { table } -> replace_table table (fun _ _ -> None)
    | ALTER_TABLE_RENAME { table; new_table } ->
        replace_table table (fun t ddl -> Some (new_table, t, ddl))
    | ALTER_TABLE_RENAME_COLUMN { table; column; new_column } ->
        replace_table table (fun t (ddl : Ddl.table) ->
            let columns =
              List.map ddl.columns ~f:(fun (col : Ddl.column) ->
                  if String.equal col.name column then
                    { col with name = new_column }
                  else col)
            in
            let ddl = { ddl with Ddl.columns } in
            let () =
              match Syntax.NT.find_opt t.fields (Syntax.name column) with
              | None -> Report.errorf "no such column: %s" column
              | Some f ->
                  let f =
                    make_field ~is_used:true ~is_generated:false
                      (Syntax.name new_column) f.expr f.ty
                  in
                  Syntax.NT.remove t.fields (Syntax.name column);
                  Syntax.NT.replace t.fields (Syntax.name new_column) f
            in
            Some (table, t, ddl))
    | ALTER_TABLE_DROP_COLUMN { table; column } ->
        replace_table table (fun t ddl ->
            let columns =
              List.filter ddl.columns ~f:(fun (col : Ddl.column) ->
                  not (String.equal col.name column))
            in
            let ddl = { ddl with Ddl.columns } in
            let () =
              match Syntax.NT.find_opt t.fields (Syntax.name column) with
              | None -> Report.errorf "no such column: %s" column
              | Some _ -> Syntax.NT.remove t.fields (Syntax.name column)
            in
            Some (table, t, ddl))
    | ALTER_TABLE_ADD_COLUMN { table; column } ->
        replace_table table (fun t ddl ->
            let columns = ddl.columns @ [ column ] in
            let ddl = { ddl with Ddl.columns } in
            let () =
              match Syntax.NT.find_opt t.fields (Syntax.name column.name) with
              | Some _ -> Report.errorf "column already exists: %s" column.name
              | None ->
                  let n = Syntax.name column.name in
                  let ty = column.ty in
                  let e = Syntax.expr_name n in
                  let f =
                    Syntax.make_field ~is_used:true ~is_generated:false n e ty
                  in
                  Syntax.NT.replace t.fields n f
            in
            Some (table, t, ddl))
end

let () =
  Stdlib.Printexc.register_printer (function
    | Report.Error report -> Some (Format.asprintf "%a" Report.pp report)
    | _ -> None)

type json = Yojson.Basic.t

module type DB = sig
  type db
  type row
  type query

  val fold : init:'a -> f:(row -> 'a -> 'a) -> db -> string -> 'a
  (** execute a an SQL query and fold over the result *)

  (* encoding *)
  type 'a encode := 'a -> string

  val encode_BOOL : bool encode
  val encode_BOOL_NULL : bool option encode
  val encode_INT : int encode
  val encode_INT_NULL : int option encode
  val encode_FLOAT : float encode
  val encode_FLOAT_NULL : float option encode
  val encode_STRING : string encode
  val encode_STRING_NULL : string option encode
  val encode_DATE : float encode
  val encode_DATE_NULL : float option encode
  val encode_DATETIME : float encode
  val encode_DATETIME_NULL : float option encode

  (* decoding *)
  type 'a decode := row -> int -> 'a

  val decode : json decode
  val decode_BOOL : bool decode
  val decode_BOOL_NULL : bool option decode
  val decode_INT : int decode
  val decode_INT_NULL : int option decode
  val decode_FLOAT : float decode
  val decode_FLOAT_NULL : float option decode
  val decode_STRING : string decode
  val decode_STRING_NULL : string option decode
  val decode_DATE : float decode
  val decode_DATE_NULL : float option decode
  val decode_DATETIME : float decode
  val decode_DATETIME_NULL : float option decode

  open Syntax

  (** printer for SQL queries *)
  class virtual ['ctx] printer : object
    method virtual emit : 'ctx Printer.ctx -> string -> unit

    method virtual emit_Expr_match :
      'ctx Printer.ctx -> name -> (name * name list * expr) list -> unit

    method virtual emit_Expr_param : 'ctx Printer.ctx -> name -> unit
    method emit_query : 'ctx Printer.ctx -> query pos -> unit
    method emit_expr : 'ctx Printer.ctx -> expr -> unit
  end
end

type params = string Syntax.NM.t

module type BACKEND = sig
  module Db : DB

  type stmt = { sql : string }
  type ('f, 'a) query = { sql : string; decode : 'f -> Db.row -> 'a -> 'a }

  val fold : init:'a -> f:'f -> Db.db -> ('f, 'a) query -> 'a
  val exec : Db.db -> stmt -> unit

  (** dynamic query API *)
  module Dynamic : sig
    val to_sql : ?params:params -> Env.t -> string -> string
    (** [to_sql schema q] returns the SQL query string for the given query [q] *)

    val exec : ?params:params -> Env.t -> Db.db -> string -> json list
    (** [exec schema db q] executes the query [q] against the database [db] and
        returns the result as a list of JSON values *)
  end

  module To_sql : sig
    val expr_to_sql : ?params:params -> Syntax.expr -> string
  end
end

module Make (Db : DB) : BACKEND with module Db = Db = struct
  module Db = Db

  type stmt = { sql : string }
  type ('f, 'a) query = { sql : string; decode : 'f -> Db.row -> 'a -> 'a }

  let fold ~init ~f db q = Db.fold ~init ~f:(q.decode f) db q.sql
  let exec db (q : stmt) = Db.fold ~init:() ~f:(fun _row () -> ()) db q.sql

  type printer_ctx = {
    env : Env.t;
    buf : Buffer.t;
    params : params;
    params_types : Analyze.params;
  }

  let printer =
    object (self)
      inherit [printer_ctx] Db.printer
      method emit { ctx; _ } s = Buffer.add_string ctx.buf s

      method emit_Expr_param
          ({ ctx = { params; params_types; env; _ }; _ } as ctx) name =
        let value =
          match Syntax.NM.find_opt name params with
          | None ->
              Report.errorf ~loc:(fst name) "missing parameter: %s" (snd name)
          | Some v -> v
        in
        match Syntax.NM.find_opt name params_types with
        | None -> failwith (sprintf "unexpected param: %s" (snd name))
        | Some Analyze.Pty_unknown -> failwith "unknown parameter type"
        | Some (Pty_variant _) -> failwith "unexpected variant parameter type"
        | Some (Pty_expr (ty, scope)) ->
            let expr = parse_expr value in
            let expr = Analyze.analyze_expr ~src:value ~scope ~ty env expr in
            self#emit_expr ctx expr
        | Some (Pty ty) ->
            let expr = parse_expr value in
            let expr = Analyze.analyze_expr ~src:value ~ty env expr in
            self#emit_expr ctx expr

      method emit_Expr_match _ _ = failwith "support variant params"
    end

  module Dynamic = struct
    let to_sql' params env src =
      Report.with_src ~src @@ fun () ->
      let q = parse_query src in
      let q = Analyze.analyze_query ~src env q in
      let buf = Buffer.create 100 in
      printer#emit_query
        {
          ctx = { buf; params; params_types = q.params; env };
          scope = q.scope;
        }
        q.query;
      q, Buffer.contents buf

    let to_sql ?(params = Syntax.NM.empty) env q = snd (to_sql' params env q)

    let exec ?(params = Syntax.NM.empty) env db q : json list =
      let info, sql = to_sql' params env q in
      let len = List.length info.row in
      let names =
        List.map info.row ~f:(fun ((_, n), _) -> n) |> Array.of_list
      in
      List.rev
        (Db.fold db sql ~init:[] ~f:(fun row acc ->
             let row =
               List.init len ~f:(fun idx ->
                   Array.get names idx, Db.decode row idx)
             in
             `Assoc row :: acc))
  end

  module To_sql = struct
    let expr_to_sql ?(params = Syntax.NM.empty) expr =
      let buf = Buffer.create 100 in
      let env = Env.create () in
      printer#emit_expr
        {
          ctx = { buf; params; params_types = Syntax.NM.empty; env };
          scope = Analyze.scope_create ();
        }
        expr;
      Buffer.contents buf
  end
end
