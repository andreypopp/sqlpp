open Sqlpp

module Sqlpp_db = Make (struct
  type db = Sqlite3.db
  type row = Sqlite3.Data.t array
  type query = Sqlite3.stmt

  let fold ~init ~f db sql =
    try
      let stmt = Sqlite3.prepare db sql in
      let rec loop acc =
        match Sqlite3.step stmt with
        | ROW -> loop (f (Sqlite3.row_data stmt) acc)
        | rc ->
            Sqlite3.Rc.check rc;
            acc
      in
      let acc = loop init in
      Sqlite3.Rc.check (Sqlite3.finalize stmt);
      acc
    with Sqlite3.SqliteError err -> failwith (Sqlite3.errmsg db)

  let or_NULL f = function None -> "NULL" | Some v -> f v
  let encode_BOOL = function true -> "TRUE" | false -> "FALSE"
  let encode_BOOL_NULL = or_NULL encode_BOOL
  let encode_INT v = string_of_int v
  let encode_FLOAT v = string_of_float v
  let encode_STRING = Printer.quote_string

  let encode_DATE v =
    let v = Unix.gmtime v in
    sprintf "%04d-%02d-%02d" (v.tm_year + 1900) (v.tm_mon + 1) v.tm_mday

  let encode_DATETIME v =
    let v = Unix.gmtime v in
    sprintf "%04d-%02d-%02d %02d:%02d:%02d" (v.tm_year + 1900) (v.tm_mon + 1)
      v.tm_mday v.tm_hour v.tm_min v.tm_sec

  let encode_INT_NULL = or_NULL encode_INT
  let encode_FLOAT_NULL = or_NULL encode_FLOAT
  let encode_STRING_NULL = or_NULL encode_STRING
  let encode_DATE_NULL = or_NULL encode_DATE
  let encode_DATETIME_NULL = or_NULL encode_DATETIME

  type 'a decode = row -> int -> 'a

  let or_NULL : 'a decode -> 'a option decode =
   fun f row idx -> match row.(idx) with NULL -> None | _ -> Some (f row idx)

  let decode_BOOL : bool decode =
   fun row idx ->
    match row.(idx) with INT 1L -> true | INT 0L -> false | _ -> assert false

  let decode_INT : int decode =
   fun row idx ->
    match row.(idx) with INT n -> Int64.to_int n | _ -> assert false

  let decode_FLOAT : float decode =
   fun row idx -> match row.(idx) with FLOAT n -> n | _ -> assert false

  let decode_STRING : string decode =
   fun row idx -> match row.(idx) with TEXT s -> s | _ -> assert false

  let decode_DATE : float decode =
   fun row idx ->
    match row.(idx) with
    | TEXT s ->
        Scanf.sscanf s "%d-%d-%d" (fun y m d -> y, m, d) |> fun (y, m, d) ->
        let t, _ =
          Unix.mktime
            {
              Unix.tm_year = y - 1900;
              tm_mon = m - 1;
              tm_mday = d;
              tm_hour = 0;
              tm_min = 0;
              tm_sec = 0;
              tm_wday = 0;
              tm_yday = 0;
              tm_isdst = false;
            }
        in
        t
    | _ -> failwith "expected date"

  let decode_DATETIME : float decode =
   fun row idx ->
    match row.(idx) with
    | TEXT s ->
        Scanf.sscanf s "%d-%d-%d %d:%d:%d" (fun y m d h mi s ->
            y, m, d, h, mi, s)
        |> fun (y, m, d, h, mi, s) ->
        let t, _ =
          Unix.mktime
            {
              Unix.tm_year = y - 1900;
              tm_mon = m - 1;
              tm_mday = d;
              tm_hour = h;
              tm_min = mi;
              tm_sec = s;
              tm_wday = 0;
              tm_yday = 0;
              tm_isdst = false;
            }
        in
        t
    | _ -> failwith "expected datetime"

  let decode_BOOL_NULL = or_NULL decode_BOOL
  let decode_INT_NULL = or_NULL decode_INT
  let decode_FLOAT_NULL = or_NULL decode_FLOAT
  let decode_STRING_NULL = or_NULL decode_STRING
  let decode_DATE_NULL = or_NULL decode_DATE
  let decode_DATETIME_NULL = or_NULL decode_DATETIME

  let decode row idx =
    match row.(idx) with
    | Sqlite3.Data.NONE -> `Null
    | Sqlite3.Data.NULL -> `Null
    | Sqlite3.Data.INT n -> `Int (Int64.to_int n)
    | Sqlite3.Data.FLOAT n -> `Float n
    | Sqlite3.Data.TEXT s -> `String s
    | Sqlite3.Data.BLOB s -> `String s

  class virtual ['ctx] printer =
    object (self)
      inherit ['ctx] Printer.printer as super
      method! private emit_Lit_string ctx s = self#emit ctx (encode_STRING s)
      method! private emit_Lit_bool ctx s = self#emit ctx (encode_BOOL s)
      method! private emit_Lit_int ctx s = self#emit ctx (encode_INT s)

      method! private emit_Expr_app ctx f args =
        match snd f, args with
        | "now", [] ->
            (* sqlite doesn't have a now() function, so we use the
               CURRENT_TIMESTAMP keyword *)
            self#emit ctx "CURRENT_TIMESTAMP"
        | "toString", [ x ] ->
            (* sqlite doesn't have a now() function, so we use the
               CURRENT_TIMESTAMP keyword *)
            self#emit ctx "cast(";
            self#emit_expr ctx x;
            self#emit ctx " as text)"
        | _ -> super#emit_Expr_app ctx f args

      method! private emit_name ctx (_, name) =
        self#emit ctx (Printer.quote_ident name)
    end
end)

module Ppx = Sqlpp_ppx.Make (struct
  module Sqlpp_db = Sqlpp_db
end)

let env = Ppx.env
let () = Ppx.register ()

module Ddl = struct
  open PPrint
  open Sqlpp.Ddl

  let rec print_create_table env table =
    let decls = List.map table.columns ~f:(print_column env) in
    let decls =
      match table.primary_key with
      | [] -> decls
      | columns ->
          let columns =
            List.map columns ~f:(fun column ->
                match
                  List.exists table.columns ~f:(fun (c : column) ->
                      String.equal c.name column)
                with
                | false ->
                    Report.errorf "no such column in PRIMARY KEY: %s" column
                | true -> string column)
          in
          decls
          @ [
              string "PRIMARY KEY"
              ^^ parens (separate (comma ^^ space) columns);
            ]
    in
    string "CREATE TABLE "
    ^^ string table.name
    ^^ parens (separate (comma ^^ space) decls)
    ^^ string " STRICT"

  and print_column env { name; ty; extra; default; primary_key; autoincrement }
      =
    let extra =
      List.filter_map
        [
          Option.return_if primary_key "PRIMARY KEY";
          Option.return_if autoincrement "AUTOINCREMENT";
          Option.map
            (fun src ->
              let e = parse_expr src in
              let e = Analyze.analyze_expr ~src ~ty env e in
              let sql = Sqlpp_db.To_sql.expr_to_sql e in
              sprintf "DEFAULT (%s)" sql)
            default;
          extra;
        ]
        ~f:Fun.id
    in
    let extra =
      match extra with
      | [] -> empty
      | extra -> space ^^ string (String.concat ~sep:" " extra)
    in
    print_ident name ^^ space ^^ print_ty ty ^^ extra

  and print_ty { Syntax.v; nullable } =
    let v =
      match v with
      | Syntax.Ty (_, "INT") -> string "INTEGER"
      | Ty (_, "FLOAT") -> string "REAL"
      | Ty (_, "STRING") -> string "TEXT"
      | Ty (_, "BOOL") -> string "INTEGER"
      | Ty (_, "DATE") -> string "TEXT"
      | Ty (_, "DATETIME") -> string "TEXT"
      | Ty (_, "TIME") -> string "TEXT"
      | Ty (_, "INTERVAL") -> string "TEXT"
      | Ty (_, _) -> assert false
      | Ty_one_of _ -> assert false
    in
    match nullable with
    | `null -> v ^^ string " NULL"
    | `non_null -> v ^^ string " NOT NULL"

  and print_ident name = string (Printer.quote_ident name)

  and print env = function
    | CREATE_TABLE table -> print_create_table env table
    | DROP_TABLE { table } -> string "DROP TABLE " ^^ print_ident table
    | ALTER_TABLE_RENAME { table; new_table } ->
        string "ALTER TABLE "
        ^^ print_ident table
        ^^ string " RENAME TO "
        ^^ print_ident new_table
    | ALTER_TABLE_RENAME_COLUMN { table; column; new_column } ->
        string "ALTER TABLE "
        ^^ print_ident table
        ^^ string " RENAME COLUMN "
        ^^ print_ident column
        ^^ string " TO "
        ^^ print_ident new_column
    | ALTER_TABLE_DROP_COLUMN { table; column } ->
        string "ALTER TABLE "
        ^^ print_ident table
        ^^ string " DROP COLUMN "
        ^^ print_ident column
    | ALTER_TABLE_ADD_COLUMN { table; column; default } ->
        string "ALTER TABLE "
        ^^ print_ident table
        ^^ string " ADD COLUMN "
        ^^ print_column env column
        ^^ Option.map_or ~default:empty
             (fun src ->
               let e = parse_expr src in
               let e = Analyze.analyze_expr ~src ~ty:column.ty env e in
               let sql = Sqlpp_db.To_sql.expr_to_sql e in
               string (sprintf " DEFAULT (%s)" sql))
             default

  and print_many env = function
    | [] -> empty
    | x :: xs -> print env x ^^ semi ^^ hardline ^^ print_many env xs

  let to_string env t =
    let buf = Buffer.create 100 in
    PPrint.ToBuffer.pretty 0.8 60 buf (print env t ^^ semi);
    Buffer.contents buf
end

let ddl_to_string = Ddl.to_string
