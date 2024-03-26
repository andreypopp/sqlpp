open Syntax

let quote_string =
  let buf = Buffer.create 16 in
  fun v ->
    let len = String.length v in
    let rec loop s i =
      if i = len then Buffer.add_substring buf v s (i - s)
      else
        match String.unsafe_get v i with
        | '\'' ->
            if s > i then Buffer.add_substring buf v s (i - s);
            Buffer.add_string buf "''";
            loop (i + 1) (i + 1)
        | _ -> loop s (i + 1)
    in
    Buffer.add_char buf '\'';
    loop 0 0;
    Buffer.add_char buf '\'';
    let s = Buffer.contents buf in
    Buffer.clear buf;
    s

let quote_ident =
  let buf = Buffer.create 16 in
  fun v ->
    let len = String.length v in
    let rec loop s i =
      if i = len then Buffer.add_substring buf v s (i - s)
      else
        match String.unsafe_get v i with
        | '"' ->
            if s > i then Buffer.add_substring buf v s (i - s);
            Buffer.add_string buf "\"\"";
            loop (i + 1) (i + 1)
        | _ -> loop s (i + 1)
    in
    Buffer.add_char buf '"';
    loop 0 0;
    Buffer.add_char buf '"';
    let s = Buffer.contents buf in
    Buffer.clear buf;
    s

type 'ctx ctx = { ctx : 'ctx; scope : Scope.scope }

class virtual ['ctx] printer =
  object (self)
    method virtual emit : 'ctx ctx -> string -> unit

    method private emitf
        : 'a. 'ctx ctx -> ('a, unit, string, unit) format4 -> 'a =
      fun ctx fmt -> Printf.ksprintf (self#emit ctx) fmt

    method private emit_seq
        : 'a. ('ctx ctx -> 'a -> unit) -> 'ctx ctx -> 'a Seq.t -> unit =
      fun emit ctx ->
        Seq.iteri (fun i expr ->
            if i > 0 then self#emit ctx ", ";
            emit ctx expr)

    method private emit_list
        : 'a. ('ctx ctx -> 'a -> unit) -> 'ctx ctx -> 'a list -> unit =
      fun emit ctx exprs -> self#emit_seq emit ctx (List.to_seq exprs)

    method private emit_option
        : 'a. ('ctx ctx -> 'a -> unit) -> 'ctx ctx -> 'a option -> unit =
      fun emit ctx -> function None -> () | Some expr -> emit ctx expr

    method private emit_Lit_int ctx v = self#emitf ctx "%d" v
    method private emit_Lit_string ctx v = self#emit ctx (quote_string v)

    method private emit_Lit_bool ctx =
      function true -> self#emit ctx "TRUE" | false -> self#emit ctx "FALSE"

    method private emit_name ctx ((loc, name) : name) =
      self#emit ctx (quote_ident name)

    method private emit_lit : 'ctx ctx -> lit -> unit =
      fun ctx -> function
        | Lit_int v -> self#emit_Lit_int ctx v
        | Lit_string v -> self#emit_Lit_string ctx v
        | Lit_bool v -> self#emit_Lit_bool ctx v

    method private emit_Expr_app ctx f args =
      match classify_app f args with
      | `Binop (op, x, y) ->
          self#emit ctx "(";
          self#emit_expr ctx x;
          self#emitf ctx " %s " (snd op);
          self#emit_expr ctx y;
          self#emit ctx ")"
      | `Prefixop ((_, "-"), x) ->
          (* special case - to print it without space *)
          self#emit ctx "-";
          self#emit_expr ctx x
      | `Prefixop (op, x) ->
          self#emitf ctx "%s " (snd op);
          self#emit_expr ctx x
      | `Suffixop (op, x) ->
          self#emit_expr ctx x;
          self#emitf ctx " %s" (snd op)
      | `Function (f, args) ->
          self#emitf ctx "%s(" (snd f);
          self#emit_list self#emit_expr ctx args;
          self#emitf ctx ")"

    method virtual emit_Expr_param : 'ctx ctx -> name -> unit

    method virtual emit_Expr_match
        : 'ctx ctx -> name -> (name * name list * expr) list -> unit

    method emit_expr ctx (expr : expr) =
      match expr.node with
      | Expr_name _ ->
          failwith "Expr_name should be lowered by analyze to Expr_nav"
      | Expr_nav (table, { node = Expr_name name; _ }) ->
          self#emit_name ctx table;
          self#emit ctx ".";
          self#emit_name ctx name
      | Expr_nav _ -> failwith "invalid SQL syntax (analyze should lower this)"
      | Expr_in (es, select) ->
          let () =
            match es with
            | [ e ] -> self#emit_expr ctx e
            | es ->
                self#emit ctx "(";
                self#emit_list self#emit_expr ctx es;
                self#emit ctx ")"
          in
          self#emit ctx " IN (";
          self#emit_select ctx select;
          self#emit ctx ")"
      | Expr_app (f, args) -> self#emit_Expr_app ctx f args
      | Expr_lit lit -> self#emit_lit ctx lit
      | Expr_ascribe (e, _) -> self#emit_expr ctx e
      | Expr_param name -> self#emit_Expr_param ctx name
      | Expr_match (name, cases) -> self#emit_Expr_match ctx name cases
      | Expr_null -> self#emit ctx "NULL"

    method private emit_select ctx (select : select) =
      let select = select.node in
      self#emit ctx "SELECT ";
      self#emit_fields ctx select.select_proj;
      self#emit_option self#emit_from ctx select.select_from;
      self#emit_option self#emit_where ctx select.select_where;
      self#emit_option self#emit_group_by ctx select.select_group_by;
      self#emit_option self#emit_having ctx select.select_having;
      ()

    method private emit_fields ctx fields =
      let select_fields =
        Seq.append
          (List.to_seq fields
          |> Seq.filter (function Field f -> f.is_used | _ -> assert false))
          (Scope.scope_fields ctx.scope
          |> Seq.filter_map (fun f ->
                 if f.is_generated && f.is_used then
                   Some
                     (Field
                        { expr = f.expr; name = Some f.name; is_used = true })
                 else None))
        |> Seq.uniq equal_select_field
      in
      self#emit_seq self#emit_field ctx select_fields

    method private emit_field ctx =
      function
      | Field_with_scope _ -> assert false
      | Field_fieldset _ -> assert false
      | Field { expr; name; is_used = _ } -> (
          self#emit_expr ctx expr;
          match name with
          | None -> ()
          | Some name ->
              self#emit ctx " AS ";
              self#emit_name ctx name)

    method private emit_from ctx ((loc, from) : from pos) =
      match from with
      | From from_one ->
          self#emit ctx " FROM ";
          self#emit_from_one ctx from_one
      | From_join (from, rigth, kind, expr) ->
          self#emit_from ctx from;
          (match kind with
          | Join_left -> self#emit ctx " LEFT JOIN "
          | Join_inner -> self#emit ctx " INNER JOIN ");
          self#emit_from_one ctx rigth;
          self#emit ctx " ON ";
          self#emit_expr ctx expr

    method private emit_from_one ctx ((loc, from) : from_one pos) =
      match from with
      | From_table (table, None) -> self#emit_name ctx table
      | From_table (table, Some alias) ->
          self#emit_name ctx table;
          self#emit ctx " AS ";
          self#emit_name ctx alias
      | From_select (select, alias) ->
          let scope =
            Scope.scope_subscope ctx.scope alias
            |> Option.get_exn_or "missing scope"
          in
          let ctx = { ctx with scope } in
          self#emit ctx "(";
          self#emit_select ctx select;
          self#emitf ctx ") AS ";
          self#emit_name ctx alias

    method private emit_where ctx (expr : expr) =
      self#emit ctx " WHERE ";
      self#emit_expr ctx expr

    method private emit_group_by ctx (exprs : expr list) =
      self#emit ctx " GROUP BY ";
      match exprs with
      | [] -> self#emit ctx "()"
      | exprs -> self#emit_list self#emit_expr ctx exprs

    method private emit_having ctx (expr : expr) =
      self#emit ctx " HAVING ";
      self#emit_expr ctx expr

    method private emit_insert ctx (insert : insert) =
      let insert = insert.node in
      self#emit ctx "INSERT INTO ";
      self#emit_name ctx insert.insert_table;
      self#emit ctx " (";
      self#emit_list self#emit_name ctx insert.insert_columns;
      self#emit ctx ") ";
      (match insert.insert_from with
      | Insert_from_values rows ->
          self#emit ctx "VALUES ";
          self#emit_list
            (fun ctx row ->
              self#emit ctx "(";
              self#emit_list self#emit_expr ctx row;
              self#emit ctx ")")
            ctx rows
      | Insert_from_select select -> self#emit_select ctx select);
      (match insert.insert_on_conflict with
      | Some On_conflict_ignore -> self#emit ctx " ON CONFLICT DO NOTHING"
      | Some On_conflict_replace -> self#emit ctx " ON CONFLICT DO UPDATE"
      | None -> ());
      self#emit_returning ctx insert.insert_returning

    method private emit_returning ctx fields =
      match fields with
      | [] -> ()
      | fields ->
          self#emit ctx " RETURNING ";
          self#emit_fields ctx fields

    method private emit_delete ctx (delete : delete) =
      let delete = delete.node in
      self#emit ctx "DELETE FROM ";
      self#emit_name ctx delete.delete_table;
      self#emit_option
        (fun ctx expr ->
          self#emit ctx " WHERE ";
          self#emit_expr ctx expr)
        ctx delete.delete_where;
      self#emit_returning ctx delete.delete_returning

    method private emit_update ctx (update : update) =
      let update = update.node in
      self#emit ctx "UPDATE ";
      self#emit_name ctx update.update_table;
      self#emit ctx " SET ";
      self#emit_list
        (fun ctx (name, expr) ->
          self#emit_name ctx name;
          self#emit ctx " = ";
          self#emit_expr ctx expr)
        ctx update.update_set;
      self#emit_option self#emit_from ctx update.update_from;
      self#emit_option
        (fun ctx expr ->
          self#emit ctx " WHERE ";
          self#emit_expr ctx expr)
        ctx update.update_where;
      self#emit_returning ctx update.update_returning

    method emit_query ctx ((_loc, query) : query pos) =
      match query with
      | Query_select select -> self#emit_select ctx select
      | Query_insert insert -> self#emit_insert ctx insert
      | Query_update update -> self#emit_update ctx update
      | Query_delete delete -> self#emit_delete ctx delete
  end
