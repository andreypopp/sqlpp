open Ppx_hash_lib.Std.Hash.Builtin
open Ppx_compare_lib.Builtin

type loc = Warnings.loc
(** start/end locations *)

(** dummy location *)
let dummy_loc : loc =
  {
    Warnings.loc_start = Lexing.dummy_pos;
    loc_end = Lexing.dummy_pos;
    loc_ghost = true;
  }

type 'a node = { node : 'a; eq : 'a Eq_class.t; loc : loc }
(** a syntax node with location and equivalence class *)

let hash_fold_node _ s node = Eq_class.hash_fold_t s node.eq
let hash_node _ node = Eq_class.hash node.eq
let equal_node _ a b = Eq_class.equal a.eq b.eq

type 'a pos = loc * 'a
(** data annotated with location *)

let equal_pos equal (_, a) (_, b) = equal a b
let hash_fold_pos hash s (_, x) = hash s x

type name = string pos
(** names/identifiers *)

let name ?(loc = dummy_loc) n = loc, n
let hash_name (_, n) = Hash.string n
let hash_fold_name s (_, n) = hash_fold_string s n
let compare_name = Ord.(map snd string)
let equal_name a b = compare_name a b = 0
let name_to_string = snd

module Name = struct
  type t = name

  let equal = equal_name
  let compare = compare_name
  let hash = hash_name
  let hash_fold_t = hash_fold_name
end

module NM = Map.Make (Name)
module NT = Hashtbl.Make (Name)

type 'a nullable = { v : 'a; nullable : [ `null | `non_null ] }
[@@deriving hash, equal]
(** nullable *)

let non_null v = { v; nullable = `non_null }
let null v = { v; nullable = `null }
let nullable_map { nullable; v } f = { nullable; v = f v }

let nullable_lub a b =
  let nullable =
    match a.nullable, b.nullable with
    | `null, _ | _, `null -> `null
    | `non_null, `non_null -> `non_null
  in
  { b with nullable }

let nullable_glb a b =
  let nullable =
    match a.nullable, b.nullable with
    | `non_null, _ | _, `non_null -> `non_null
    | `null, `null -> `null
  in
  { b with nullable }

type ty = tysyn nullable

and tysyn = Ty of name | Ty_one_of of (string option * name list)
[@@deriving hash, equal]

type ty_or_expr = Ty_val of ty | Ty_expr of ty [@@deriving hash, equal]

let bool = Ty (name "BOOL")
let string = Ty (name "STRING")
let int = Ty (name "INT")
let float = Ty (name "FLOAT")
let datetime = Ty (name "DATETIME")
let date = Ty (name "DATE")
let time = Ty (name "TIME")
let interval = Ty (name "INTERVAL")
let numeric = Ty_one_of (Some "NUMERIC", [ name "INT"; name "FLOAT" ])

let ty_to_string : ty -> string =
  let with_null nullable s =
    match nullable with `null -> s | `non_null -> sprintf "%s NOT NULL" s
  in
  fun { v; nullable } ->
    match v with
    | Ty_one_of (Some name, _) -> with_null nullable name
    | Ty_one_of (None, ts) ->
        let s = List.map ~f:name_to_string ts |> String.concat ~sep:" | " in
        with_null nullable s
    | Ty name -> with_null nullable (String.uppercase_ascii (snd name))

let ty_lub ?src ?loc a b =
  match a.v, b.v with
  | Ty x, Ty y when equal_name x y -> nullable_lub a b
  | Ty x, Ty y -> nullable_lub a { b with v = Ty_one_of (None, [ x; y ]) }
  | Ty x, Ty_one_of (_, ys) ->
      nullable_lub a
        { b with v = Ty_one_of (None, List.add_nodup ~eq:equal_name x ys) }
  | Ty_one_of (_, xs), Ty y ->
      nullable_lub a
        { b with v = Ty_one_of (None, List.add_nodup ~eq:equal_name y xs) }
  | Ty_one_of (_, xs), Ty_one_of (_, ys) ->
      nullable_lub a
        { b with v = Ty_one_of (None, List.union ~eq:equal_name xs ys) }

let ty_glb ?src ?loc a b =
  let no_match_error () =
    Report.errorf ?src ?loc "types do not match %s and %s" (ty_to_string a)
      (ty_to_string b)
  in
  match a.v, b.v with
  | Ty x, Ty y when equal_name x y -> nullable_glb a b
  | Ty x, Ty y -> no_match_error ()
  | Ty x, Ty_one_of (_, ys) ->
      if not (List.mem ~eq:equal_name x ys) then no_match_error ();
      nullable_glb b a
  | Ty_one_of (_, xs), Ty y ->
      if not (List.mem ~eq:equal_name y xs) then no_match_error ();
      nullable_glb a b
  | Ty_one_of (_, xs), Ty_one_of (_, ys) -> (
      match List.inter ~eq:equal_name xs ys with
      | [] -> no_match_error ()
      | [ t ] -> nullable_glb a { b with v = Ty t }
      | ts -> nullable_glb a { b with v = Ty_one_of (None, ts) })

type sty = Sty_name of name | Sty_struct of (name * sty_elem) list
and sty_elem = Sty_elem_ty of ty | Sty_elem_scope of sty

type lit = Lit_int of int | Lit_string of string | Lit_bool of bool
[@@deriving hash, equal]

type query =
  | Query_select of select
  | Query_insert of insert
  | Query_update of update
  | Query_delete of delete

and selectsyn = {
  select_proj : select_field list;
  select_from : from pos option;
  select_where : expr option;
  select_group_by : expr list option;
  select_having : expr option;
  select_order_by : (expr * dir) list option;
  select_is_open : bool;
}
(** SELECT ... FROM ... *)

and select = selectsyn node

and updatesyn = {
  update_set : (name * expr) list;
  update_table : name;
  update_where : expr option;
  update_from : from pos option;
}
(** UPDATE ... SET ... *)

and update = updatesyn node

and insertsyn = {
  insert_table : name;
  insert_columns : name list;
  insert_from : insert_from;
  insert_on_conflict : on_conflict option;
}
(** INSERT INTO ... *)

and on_conflict = On_conflict_replace | On_conflict_ignore
and insert = insertsyn node

and deletesyn = { delete_table : name; delete_where : expr option }
(** DELETE FROM ... WHERE ... *)

and delete = deletesyn node

and from_one =
  | From_table of name * name option
  | From_select of select * name

and from =
  | From of from_one pos
  | From_join of from pos * from_one pos * join_kind * expr

and insert_from =
  | Insert_from_values of expr list list
  | Insert_from_select of select

and dir = Dir_asc | Dir_desc
and join_kind = Join_left | Join_inner

and select_field =
  | Field of { name : name option; expr : expr; is_used : bool }
  | Field_with_scope of name list * name
  | Field_fieldset of { name : name; args : name list list; is_used : bool }
[@@deriving equal, hash]

and expr = exprsyn node

and exprsyn =
  | Expr_app of name * expr list
  | Expr_lit of lit
  | Expr_name of name
  | Expr_nav of name * expr
  | Expr_in of expr list * select
  | Expr_ascribe of expr * ty_or_expr
  | Expr_param of name
  | Expr_match of name * expr_match_case list
  | Expr_null
[@@deriving hash, equal]

and expr_match_case = name * name list * expr

type field = {
  name : name;
  expr : expr;
  ty : ty;
  is_generated : bool;
  mutable is_used : bool;
  dependencies : (name option * name) list;
}

type decl =
  | Decl_table of name * (name * ty) list
  | Decl_query of name * query pos
  | Decl_fieldset of name * fieldset pos

and fieldset = {
  fieldset_args : (name * sty) list;
  fieldset_exprs : (name * expr) list;
}

(* field aux *)

let equal_field a b = equal_name a.name b.name && equal_expr a.expr b.expr

let make_field ?(dependencies = []) ~is_generated ~is_used name expr ty : field
    =
  { name; expr; ty; is_generated; is_used; dependencies }

let fresh_field f =
  { f with is_used = f.is_used; dependencies = f.dependencies }

(* select aux *)

module Eq_select = Eq_class.Make (struct
  type t = selectsyn

  let hash = hash_selectsyn
  let equal = equal_selectsyn
end)

let select ?(loc = dummy_loc) ?(is_open = false) ?group_by ?having ?order_by
    ?where ?from proj =
  let node =
    {
      select_proj = proj;
      select_from = from;
      select_where = where;
      select_group_by = group_by;
      select_having = having;
      select_order_by = order_by;
      select_is_open = is_open;
    }
  in
  { node; loc; eq = Eq_select.v node }

module Eq_update = Eq_class.Make (struct
  type t = updatesyn

  let hash = hash_updatesyn
  let equal = equal_updatesyn
end)

let update ?(loc = dummy_loc) ?from ?where table set =
  let node =
    {
      update_table = table;
      update_set = set;
      update_where = where;
      update_from = from;
    }
  in
  { node; loc; eq = Eq_update.v node }

module Eq_insert = Eq_class.Make (struct
  type t = insertsyn

  let hash = hash_insertsyn
  let equal = equal_insertsyn
end)

let insert ?(loc = dummy_loc) ?on_conflict table columns from =
  let node =
    {
      insert_table = table;
      insert_columns = columns;
      insert_from = from;
      insert_on_conflict = on_conflict;
    }
  in
  { node; loc; eq = Eq_insert.v node }

module Eq_delete = Eq_class.Make (struct
  type t = deletesyn

  let hash = hash_deletesyn
  let equal = equal_deletesyn
end)

let delete ?(loc = dummy_loc) ?where table =
  let node = { delete_table = table; delete_where = where } in
  { node; loc; eq = Eq_delete.v node }

(* expr aux *)

module Eq_expr = Eq_class.Make (struct
  type t = exprsyn

  let hash = hash_exprsyn
  let equal = equal_exprsyn
end)

let expr ?(loc = dummy_loc) node = { node; loc; eq = Eq_expr.v node }
let expr_app ?loc f es = expr ?loc (Expr_app (f, es))
let expr_lit ?loc lit = expr ?loc (Expr_lit lit)
let expr_name ?loc n = expr ?loc (Expr_name n)
let expr_in ?loc es select = expr ?loc (Expr_in (es, select))
let expr_ascribe ?loc e ty = expr ?loc (Expr_ascribe (e, ty))
let expr_param ?loc name = expr ?loc (Expr_param name)
let expr_match ?loc n vs = expr ?loc (Expr_match (n, vs))
let expr_null ?loc () = expr ?loc Expr_null
let expr_nav ?loc n e = expr ?loc (Expr_nav (n, e))

let rec field_name e =
  match e.node with
  | Expr_name n -> Some n
  | Expr_nav (_, e) -> field_name e
  | _ -> None

let classify_app' f args =
  match snd f, args with
  | "-", [ x ] -> Some 100, `Prefixop (f, x)
  | ("*" | "/"), [ x; y ] -> Some 90, `Binop (f, x, y)
  | ("+" | "-"), [ x; y ] -> Some 80, `Binop (f, x, y)
  | (">" | "<" | "<=" | ">="), [ x; y ] -> Some 70, `Binop (f, x, y)
  | ("=" | "!="), [ x; y ] -> Some 60, `Binop (f, x, y)
  | ("IS NULL" | "IS NOT NULL"), [ x ] -> Some 60, `Suffixop (f, x)
  | "NOT", [ x ] -> Some 50, `Prefixop (f, x)
  | "AND", [ x; y ] -> Some 40, `Binop (f, x, y)
  | "OR", [ x; y ] -> Some 30, `Binop (f, x, y)
  | _ -> None, `Function (f, args)

let classify_app f args = snd (classify_app' f args)

class ['ctx] format =
  let open PPrint in
  let ( ^^> ) a b = a ^^ space ^^ b in
  let parens_if_needed prec prec1 f =
    match prec, prec1 with
    | prec, None | None, prec -> f prec
    | Some prec0, Some prec1 ->
        if Int.(prec0 > prec1) then parens (f None) else f (Some prec1)
  in
  object (self)
    method format_name (_, name) = string name
    method format_kw = string

    method format_lit =
      function
      | Lit_bool true -> string "TRUE"
      | Lit_bool false -> string "FALSE"
      | Lit_int v -> string (Int.to_string v)
      | Lit_string v ->
          string
            (sprintf "'%s'" (String.replace v ~which:`All ~sub:"'" ~by:"''"))

    method format_param (_, name) = string (sprintf "?%s" name)

    method format_expr prec ctx expr =
      match expr.node with
      | Expr_lit lit -> self#format_lit lit
      | Expr_app (f, args) -> self#format_expr_app prec ctx (f, args)
      | Expr_null -> string "NULL"
      | Expr_in ([ e ], select) ->
          self#format_expr prec ctx e
          ^^> string "IN"
          ^^> parens (self#format_select ctx select)
      | Expr_in (es, select) ->
          parens
            (separate (comma ^^ space)
               (List.map es ~f:(self#format_expr None ctx)))
          ^^> string "IN"
          ^^> parens (self#format_select ctx select)
      | Expr_ascribe (expr, _) -> self#format_expr prec ctx expr
      | Expr_param name -> self#format_param name
      | Expr_match (name, vs) ->
          let format_case (tag, args, expr) =
            let args =
              match args with
              | [] -> empty
              | [ x ] -> space ^^ self#format_param x
              | xs ->
                  space
                  ^^ parens
                       (separate (comma ^^ space)
                          (List.map xs ~f:self#format_param))
            in
            break 1
            ^^ char '|'
            ^^> self#format_name tag
            ^^ args
            ^^> string "->"
            ^^> self#format_expr None ctx expr
          in
          group
            (self#format_kw "MATCH"
            ^^> self#format_param name
            ^^> self#format_kw "WITH"
            ^^ group (nest 2 (separate empty (List.map vs ~f:format_case)))
            ^^ break 1
            ^^ self#format_kw "END")
      | Expr_nav
          (name, ({ node = Expr_nav _ | Expr_app _ | Expr_name _; _ } as e)) ->
          self#format_name name ^^ dot ^^ self#format_expr prec ctx e
      | Expr_nav (name, e) ->
          self#format_name name ^^ dot ^^ parens (self#format_expr None ctx e)
      | Expr_name name -> self#format_name name

    method format_expr_app prec ctx (f, args) =
      match classify_app' f args with
      | prec1, `Binop (op, x, y) ->
          group
          @@ parens_if_needed prec prec1 (fun prec1 ->
                 self#format_expr prec1 ctx x
                 ^^ nest 2
                      (break 1
                      ^^ self#format_name op
                      ^^> self#format_expr prec1 ctx y))
      | prec1, `Prefixop (((_, "-") as op), x) ->
          (* special case - to print it without space *)
          parens_if_needed prec prec1 (fun prec1 ->
              self#format_name op ^^ self#format_expr prec1 ctx x)
      | prec1, `Prefixop (op, x) ->
          parens_if_needed prec prec1 (fun prec1 ->
              self#format_name op ^^> self#format_expr prec1 ctx x)
      | prec1, `Suffixop (op, x) ->
          parens_if_needed prec prec1 (fun prec1 ->
              self#format_expr prec1 ctx x ^^ string " " ^^ self#format_name op)
      | _, `Function (f, args) ->
          let args =
            lparen
            ^^ nest 2
                 (break 0
                 ^^ separate
                      (comma ^^ break 1)
                      (List.map args ~f:(self#format_expr None ctx))
                 ^^ rparen)
          in
          group (self#format_name f ^^ args)

    method format_query (ctx : 'ctx) ((_, q) : query pos) =
      match q with
      | Query_select select -> self#format_select ctx select
      | Query_insert insert -> self#format_insert ctx insert
      | Query_update update -> self#format_update ctx update
      | Query_delete delete -> self#format_delete ctx delete

    method format_select ctx select =
      let {
        select_proj;
        select_from;
        select_where;
        select_group_by;
        select_having;
        select_order_by;
      } =
        select.node
      in
      group
        (self#format_kw "SELECT"
        ^^ nest 2 (break 1 ^^ self#format_fields ctx (List.to_seq select_proj))
        ^^ break 1
        ^^ (match select_from with
           | None -> empty
           | Some select_from ->
               group
                 (self#format_kw "FROM" ^^> self#format_from ctx select_from))
        ^^ self#format_where ctx select_where
        ^^ self#format_group_by ctx select_group_by
        ^^ self#format_having ctx select_having
        ^^ self#format_order_by ctx select_order_by)

    method format_insert ctx insert =
      let { insert_table; insert_columns; insert_from } = insert.node in
      let from =
        match insert_from with
        | Insert_from_values values ->
            self#format_kw "VALUES"
            ^^> group
                  (nest 2
                     (break 0
                     ^^ separate
                          (comma ^^ break 1)
                          (List.map values ~f:(self#format_values ctx))))
        | Insert_from_select select -> self#format_select ctx select
      in
      let on_conflict =
        match insert.node.insert_on_conflict with
        | None -> empty
        | Some On_conflict_replace ->
            break 1 ^^ self#format_kw "ON CONFLICT REPLACE"
        | Some On_conflict_ignore ->
            break 1 ^^ self#format_kw "ON CONFLICT IGNORE"
      in
      group
        (group
           (self#format_kw "INSERT INTO"
           ^^ break 1
           ^^ self#format_name insert_table
           ^^ lparen
           ^^ group
                (nest 2
                   (break 0
                   ^^ separate
                        (comma ^^ break 1)
                        (List.map insert_columns ~f:self#format_name)
                   ^^ rparen)))
        ^^ break 1
        ^^ from
        ^^ on_conflict)

    method format_values ctx es =
      group
        (lparen
        ^^ nest 2
             (break 0
             ^^ separate
                  (comma ^^ break 1)
                  (List.map es ~f:(self#format_expr None ctx))
             ^^ rparen))

    method format_update ctx update =
      let { update_table; update_set; update_from; update_where } =
        update.node
      in
      group
        (group
           (self#format_kw "UPDATE"
           ^^ break 1
           ^^ self#format_name update_table
           ^^ break 1
           ^^ self#format_kw "SET")
        ^^ group
             (nest 2
                (break 1
                ^^ separate
                     (comma ^^ break 1)
                     (List.map update_set ~f:(self#format_update_set ctx))))
        ^^ (match update_from with
           | None -> empty
           | Some from ->
               break 1 ^^ self#format_kw "FROM" ^^> self#format_from ctx from)
        ^^ self#format_where ctx update_where)

    method format_update_set ctx (n, e) =
      group (self#format_name n ^^ string " = " ^^ self#format_expr None ctx e)

    method format_delete ctx delete =
      let { delete_table; delete_where } = delete.node in
      group
        (group
           (self#format_kw "DELETE FROM"
           ^^ break 1
           ^^ self#format_name delete_table)
        ^^ self#format_where ctx delete_where)

    method format_field ctx =
      function
      | Field { expr; name; is_used } -> (
          let prefix =
            if not is_used then self#format_kw "WITH" ^^ space else empty
          in
          match name with
          | None -> group (prefix ^^ self#format_expr None ctx expr)
          | Some name ->
              group
                (prefix
                ^^ self#format_expr None ctx expr
                ^^ nest 2
                     (break 1 ^^ self#format_kw "AS" ^^> self#format_name name)
                ))
      | Field_with_scope (names, name) ->
          group
            (self#format_kw "WITH"
            ^^> group (separate dot (List.map names ~f:self#format_name))
            ^^ nest 2 (break 1 ^^ self#format_kw "AS" ^^> self#format_name name)
            )
      | Field_fieldset { name; args } ->
          string (sprintf "...%s" (snd name))
          ^^ parens
               (separate comma
                  (List.map args ~f:(fun names ->
                       group
                         (separate dot (List.map names ~f:self#format_name)))))

    method format_order ctx (expr, dir) =
      group (self#format_expr None ctx expr ^^> self#format_dir dir)

    method format_dir =
      function
      | Dir_asc -> self#format_kw "ASC" | Dir_desc -> self#format_kw "DESC"

    method format_fields ctx fields =
      separate
        (comma ^^ break 1)
        (Seq.map (self#format_field ctx) fields |> Seq.to_list)

    method format_from ctx (_, from) =
      match from with
      | From from_one -> self#format_from_one ctx from_one
      | From_join (left, right, kind, cond) ->
          self#format_from ctx left
          ^^ break 1
          ^^ self#format_join_kind kind
          ^^> self#format_from_one ctx right
          ^^ break 1
          ^^ self#format_join_cond ctx cond

    method format_From_select ctx (select : select) name =
      group
        (parens (nest 2 (break 0 ^^ self#format_select ctx select))
        ^^> self#format_kw "AS"
        ^^> self#format_name name)

    method format_From_table _ctx table alias =
      match alias with
      | None -> self#format_name table
      | Some name ->
          group
            (self#format_name table
            ^^> self#format_kw "AS"
            ^^> self#format_name name)

    method format_from_one ctx (_, from) =
      match from with
      | From_table (table, name) -> self#format_From_table ctx table name
      | From_select (select, name) -> self#format_From_select ctx select name

    method format_join_kind =
      function
      | Join_inner -> self#format_kw "JOIN"
      | Join_left -> self#format_kw "LEFT JOIN"

    method format_join_cond ctx expr =
      group (self#format_kw "ON" ^^> self#format_expr None ctx expr)

    method format_where ctx =
      function
      | None -> empty
      | Some expr ->
          group
            (break 1
            ^^ self#format_kw "WHERE"
            ^^ nest 2 (break 1 ^^ self#format_expr None ctx expr))

    method format_having ctx =
      function
      | None -> empty
      | Some expr ->
          break 1
          ^^ group
               (self#format_kw "HAVING"
               ^^ nest 2 (break 1 ^^ self#format_expr None ctx expr))

    method format_order_by ctx =
      function
      | None | Some [] -> empty
      | Some fs ->
          break 1
          ^^ self#format_kw "ORDER BY"
          ^^ group
               (nest 2
                  (break 1
                  ^^ separate
                       (comma ^^ break 1)
                       (List.map fs ~f:(self#format_order ctx))))

    method format_group_by ctx =
      function
      | None -> empty
      | Some [] -> self#format_kw "GROUP BY ()"
      | Some fs ->
          break 1
          ^^ self#format_kw "GROUP BY"
          ^^ group
               (nest 2
                  (break 1
                  ^^ separate
                       (comma ^^ break 1)
                       (List.map fs ~f:(self#format_expr None ctx))))
  end

let print_expr =
  let format = new format in
  fun expr ->
    let ctx = () in
    format#format_expr None ctx expr

let expr_to_string expr =
  let buf = Buffer.create 32 in
  PPrint.ToBuffer.compact buf (print_expr expr);
  Buffer.contents buf

class virtual ['a, 'ctx] fold =
  let flist f ctx xs acc =
    List.fold_left xs ~init:acc ~f:(fun acc x -> f ctx x acc)
  in
  let fopt f ctx x acc = Option.fold (fun acc x -> f ctx x acc) acc x in
  object (self)
    method fold_select_field ctx field acc =
      match field with
      | Field field -> self#fold_expr ctx field.expr acc
      | Field_with_scope _ -> acc
      | Field_fieldset _ -> acc

    method fold_From_table _ _ _ acc = acc
    method fold_From_select ctx select _ acc = self#fold_select ctx select acc

    method fold_from_one ctx (_, from_one) acc =
      match from_one with
      | From_table (table, alias) -> self#fold_From_table ctx table alias acc
      | From_select (select, alias) ->
          self#fold_From_select ctx select alias acc

    method fold_From_join ctx left right cond acc =
      let acc = self#fold_from ctx left acc in
      let acc = self#fold_from_one ctx right acc in
      self#fold_expr ctx cond acc

    method fold_from ctx (_, from) acc =
      match from with
      | From from -> self#fold_from_one ctx from acc
      | From_join (left, right, _, cond) ->
          self#fold_From_join ctx left right cond acc

    method fold_Expr_app ctx _name args acc = flist self#fold_expr ctx args acc
    method fold_Expr_lit _ _ acc = acc
    method fold_Expr_name _ _ acc = acc
    method fold_Expr_nav ctx _name expr acc = self#fold_expr ctx expr acc
    method fold_Expr_ascribe ctx expr _ty acc = self#fold_expr ctx expr acc
    method fold_Expr_param _ _ acc = acc

    method fold_Expr_match ctx _name cases acc =
      flist (fun ctx (_, _, e) acc -> self#fold_expr ctx e acc) ctx cases acc

    method fold_Expr_null _ acc = acc

    method fold_expr ctx e acc =
      match e.node with
      | Expr_app (name, args) -> self#fold_Expr_app ctx name args acc
      | Expr_lit lit -> self#fold_Expr_lit ctx lit acc
      | Expr_name name -> self#fold_Expr_name ctx name acc
      | Expr_in (es, select) ->
          let acc = flist self#fold_expr ctx es acc in
          self#fold_select ctx select acc
      | Expr_nav (name, expr) -> self#fold_Expr_nav ctx name expr acc
      | Expr_ascribe (name, ty) -> self#fold_Expr_ascribe ctx name ty acc
      | Expr_param param -> self#fold_Expr_param ctx param acc
      | Expr_match (e, cases) -> self#fold_Expr_match ctx e cases acc
      | Expr_null -> self#fold_Expr_null ctx acc

    method fold_order ctx (e, _) acc = self#fold_expr ctx e acc

    method fold_select_group_by ctx exprs acc =
      flist self#fold_expr ctx exprs acc

    method fold_select_where ctx expr acc = self#fold_expr ctx expr acc
    method fold_select_having ctx expr acc = self#fold_expr ctx expr acc

    method fold_select_order_by ctx exprs acc =
      flist self#fold_order ctx exprs acc

    method fold_select_proj ctx fields acc =
      flist self#fold_select_field ctx fields acc

    method fold_select ctx select acc =
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
      let acc = self#fold_select_proj ctx select_proj acc in
      let acc = fopt self#fold_from ctx select_from acc in
      let acc = fopt self#fold_select_where ctx select_where acc in
      let acc = fopt self#fold_select_group_by ctx select_group_by acc in
      let acc = fopt self#fold_select_having ctx select_having acc in
      let acc = fopt self#fold_select_order_by ctx select_order_by acc in
      acc

    method fold_insert : 'ctx -> insert -> 'a -> 'a =
      fun ctx insert acc ->
        let { insert_table; insert_columns; insert_from } = insert.node in
        let acc =
          match insert_from with
          | Insert_from_values es -> flist (flist self#fold_expr) ctx [] acc
          | Insert_from_select select -> self#fold_select ctx select acc
        in
        acc

    method fold_delete : 'ctx -> delete -> 'a -> 'a =
      fun ctx delete acc ->
        let { delete_table; delete_where } = delete.node in
        let acc = fopt self#fold_expr ctx delete_where acc in
        acc

    method fold_update : 'ctx -> update -> 'a -> 'a =
      fun ctx update acc ->
        let { update_table; update_set; update_from; update_where } =
          update.node
        in
        let acc =
          flist
            (fun ctx (_, e) acc -> self#fold_expr ctx e acc)
            ctx update_set acc
        in
        let acc = fopt self#fold_from ctx update_from acc in
        let acc = fopt self#fold_expr ctx update_where acc in
        acc

    method fold_query : 'ctx -> query pos -> 'a -> 'a =
      fun ctx (loc, q) acc ->
        match q with
        | Query_select select -> self#fold_select ctx select acc
        | Query_insert insert -> self#fold_insert ctx insert acc
        | Query_update update -> self#fold_update ctx update acc
        | Query_delete delete -> self#fold_delete ctx delete acc
  end
