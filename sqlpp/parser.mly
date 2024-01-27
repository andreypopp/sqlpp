%{

open Syntax

let to_loc (loc_start, loc_end) =
  { Location.loc_start; loc_end; loc_ghost = false }

%}

%token <Syntax.lit> LIT
%token <string> IDENT
%token AND OR
%token AS
%token ASC
%token BY
%token COLON
%token COMMA
%token SEMI
%token DESC
%token EOF
%token FROM
%token GROUP
%token OP_IS NULL NOT
%token JOIN
%token LEFT
%token LPAREN RPAREN
%token ON
%token OP_ADD OP_MIN OP_MUL OP_DIV OP_EQ OP_NEQ
%token OP_GT OP_LT OP_GTE OP_LTE
%token ORDER
%token SELECT
%token INSERT
%token UPDATE
%token DELETE
%token IGNORE
%token REPLACE
%token CONFLICT
%token SET
%token INTO
%token VALUES
%token WHERE
%token HAVING
%token DOT
%token MATCH
%token WITH
%token WITHSCOPE
%token BAR
%token RARROW
%token END
%token IN
%token ELLIPSIS
%token CREATE
%token QUERY
%token TABLE
%token FIELDSET
%token EXPR
%token <string> FIELDSET_SPLICE
%token <string> PARAM

%left OR
%left AND
%right NOT
%left OP_EQ OP_NEQ OP_IS
%left OP_GT OP_LT OP_GTE OP_LTE
%left IN
%left OP_ADD OP_MIN
%left OP_MUL OP_DIV
%nonassoc COLON
%nonassoc UMINUS 

%start <Syntax.expr> expr_one
%start <Syntax.query Syntax.pos> query_one
%start <Syntax.decl Syntax.pos> decl_one
%start <Syntax.decl Syntax.pos list> decl_many
%%

expr_one:
  expr = expr; EOF { expr }

query_one:
  query = query; EOF { query }

query:
  q = select; { to_loc $loc, Query_select q }
| q = insert; { to_loc $loc, Query_insert q }
| q = update; { to_loc $loc, Query_update q }
| q = delete; { to_loc $loc, Query_delete q }

decl_many:
  EOF { [] }
| x = decl; SEMI?; xs = decl_many; { x::xs }

decl_one:
  decl = decl; SEMI?; EOF { decl }

decl:
  CREATE; QUERY; name = name; AS; q = query; { to_loc $loc, Decl_query (name, q) }
| CREATE; TABLE; name = name; LPAREN; cols = nonempty_flex_list(COMMA, col); RPAREN
  { to_loc $loc, Decl_table (name, cols) }
| CREATE; FIELDSET; name = name;  
  AS; SELECT; exprs = nonempty_flex_list(COMMA, fieldset_field)
  { to_loc $loc, Decl_fieldset (name, (to_loc $loc, {fieldset_args = []; fieldset_exprs = exprs})) }
| CREATE; FIELDSET; name = name; LPAREN; args = flex_list(COMMA, fieldset_arg); RPAREN; 
  AS; SELECT; exprs = nonempty_flex_list(COMMA, fieldset_field)
  { to_loc $loc, Decl_fieldset (name, (to_loc $loc, {fieldset_args = args; fieldset_exprs = exprs})) }

fieldset_field:
  e = expr; AS; n = name; { n, e }

fieldset_arg:
  FROM; sty = sty; AS; n = name { n, sty }
| FROM; n = name { n, Sty_name n }

sty:
  name = name { Sty_name name }
| LPAREN; xs = nonempty_flex_list(COMMA, sty_elem); RPAREN { Sty_struct xs }

sty_elem:
  n = name; ty = ty; { n, Sty_elem_ty ty }
| FROM; sty = sty; AS; n = name  { n, Sty_elem_scope sty }
| FROM; n = name  { n, Sty_elem_scope (Sty_name n) }

col:
  name = name; ty = ty; { name, ty }

select:
  SELECT; fields = fields; t = option(select_tail);
  {
    let proj, is_open =
      List.fold_left fields ~init:([], false) ~f:(fun (fs, is_open) f ->
        match f with
        | `E -> fs, is_open || true
        | `F f -> f::fs, is_open
      )
    in
    let proj = List.rev proj in
    match t with
    | None ->
      select 
        proj
        ~loc:(to_loc $loc)
        ?from:None
        ?where:None
        ?group_by:None
        ?having:None
        ?order_by:None
        ~is_open
    | Some (from, where, group_by, having, order_by) ->
      select 
        proj
        ~loc:(to_loc $loc)
        ~from
        ?where
        ?group_by
        ?having
        ?order_by
        ~is_open
  }

select_tail:
  select_from = select_from;
  select_where = option(where);
  select_group_by = option(group_by);
  select_having = option(having);
  select_order_by = option(order_by);
  {
    select_from,
    select_where,
    select_group_by,
    select_having,
    select_order_by
  }

select_from:
  FROM; from = from { from }

insert:
  INSERT; INTO; table = name; t = insert_tail; on_conflict = option(insert_on_conflict);
  {
    let columns, from = t in
    insert ~loc:(to_loc $loc) ?on_conflict table columns from
  }

insert_on_conflict:
  ON; CONFLICT; IGNORE { On_conflict_ignore }
| ON; CONFLICT; REPLACE { On_conflict_replace }

insert_tail:
  LPAREN; names = nonempty_flex_list(COMMA, name); RPAREN; from = insert_from;
  { names, from }
| SET; set = nonempty_flex_list(COMMA, set); 
  {
    let ns, es = List.split set in
    ns, Insert_from_values [es]
  }

insert_from:
  VALUES; vs = nonempty_flex_list(COMMA, tuple); { Insert_from_values vs }
| s = select; { Insert_from_select s }

tuple:
  LPAREN; es = nonempty_flex_list(COMMA, expr); RPAREN; { es }

update:
  UPDATE; table = name;
  SET; set = nonempty_flex_list(COMMA, set);
  from = option(update_from);
  where = option(where);
  { update ~loc:(to_loc $loc) ?from ?where table set }

set:
  n = name; OP_EQ; e = expr; { n, e }

delete:
  DELETE; FROM; table = name; where = option(where);
  { delete ~loc:(to_loc $loc) ?where table }

update_from:
  FROM; f = from; { f }

where:
  WHERE; expr = expr; { expr }

having:
  HAVING; expr = expr; { expr }

order_by:
  ORDER; BY; fields = nonempty_flex_list(COMMA, order) { fields }

group_by:
  GROUP; BY; fields = nonempty_flex_list(COMMA, expr) { fields }
| GROUP; BY; LPAREN; RPAREN { [] }

order:
  expr = expr; dir = option(dir); { expr, Option.value dir ~default:Dir_asc }

dir:
  ASC { Dir_asc }
| DESC { Dir_desc }

fields:
  fields = nonempty_flex_list(COMMA, field) { fields }

field:
  expr = expr; name = option(alias) { `F (Field { expr; name; is_used = true }) }
| WITH; expr = expr; name = option(alias) { `F (Field { expr; name; is_used = false }) }
| WITHSCOPE; names = scopename; name = alias; { `F (Field_with_scope (names, name)) }
| name = FIELDSET_SPLICE;
  { `F (Field_fieldset {name=(to_loc $loc, name); args= []; is_used=true}) }
| name = FIELDSET_SPLICE; LPAREN; args = flex_list(COMMA, scopename) RPAREN
  { `F (Field_fieldset {name=(to_loc $loc, name); args; is_used=true}) }
| WITH; name = FIELDSET_SPLICE;
  { `F (Field_fieldset {name=(to_loc $loc, name); args= []; is_used=false}) }
| WITH; name = FIELDSET_SPLICE; LPAREN; args = flex_list(COMMA, scopename) RPAREN
  { `F (Field_fieldset {name=(to_loc $loc, name); args; is_used=false}) }
| ELLIPSIS { `E }

from_one:
  table = name; name = option(alias) 
  { to_loc $loc, From_table (table, name) }
| LPAREN; select = select; RPAREN; name = alias;
  { to_loc $loc, From_select (select, name) }

from:
  from_one = from_one
  { fst from_one, From from_one }
| left = from; kind = join_kind; right = from_one; cond = join_cond
  { to_loc $loc, From_join (left, right, kind, cond) }

alias:
  AS; name = name; { name }

join_kind:
  JOIN { Join_inner }
| LEFT; JOIN { Join_left }

join_cond:
  ON; e = expr { e }

expr:
  e = expr_simple { e }
| lhs = expr; op = binop; rhs = expr { expr_app ~loc:(to_loc $loc) op [lhs; rhs] }
| lit = LIT { expr_lit ~loc:(to_loc $loc) lit }
| NULL { expr_null ~loc:(to_loc $loc) () }
| e = expr; op = suffixop { expr_app ~loc:(to_loc $loc) op [e] }
| op = minisprefixop; e = expr %prec UMINUS { expr_app ~loc:(to_loc $loc) op [e] }
| op = prefixop; e = expr { expr_app ~loc:(to_loc $loc) op [e] }
| e = expr; COLON; ty = ty_or_expr { expr_ascribe ~loc:(to_loc $loc) e ty }
| e = expr; IN; LPAREN; select = select; RPAREN { expr_in ~loc:(to_loc $loc) [e] select }
| LPAREN; e = expr; COMMA; es = nonempty_flex_list(COMMA, expr); RPAREN; IN; LPAREN; select = select; RPAREN
  { expr_in ~loc:(to_loc $loc) (e::es) select }

expr_simple:
  name = name { expr_name ~loc:(to_loc $loc) name }
| n = name; DOT; e = expr_simple; { expr_nav ~loc:(to_loc $loc) n e }
| param = param { expr_param ~loc:(fst param) param }
| LPAREN; expr = expr; RPAREN { expr }
| e = expr_app { e }
| MATCH; param = param; WITH; option(BAR); vs = nonempty_flex_list(BAR, variant); END
  { expr_match ~loc:(to_loc $loc) param vs }

param:
  param = PARAM { to_loc $loc, param }

variant:
  tag = name; args = variant_args; RARROW; expr = expr
  { tag, args, expr }

variant_args:
  { [] }
| x = param { [x] }
| LPAREN; xs = flex_list(COMMA, param); RPAREN { xs }

%inline expr_app:
  func = name; LPAREN; args = flex_list(COMMA, expr); RPAREN {
    expr_app ~loc:(to_loc $loc) func args }

%inline binop:
  OP_ADD { to_loc $loc, "+" }
| OP_MIN { to_loc $loc, "-" }
| OP_MUL { to_loc $loc, "-" }
| OP_DIV { to_loc $loc, "/" }
| OP_EQ  { to_loc $loc, "=" }
| OP_NEQ { to_loc $loc, "!=" }
| OP_GT  { to_loc $loc, ">" }
| OP_LT  { to_loc $loc, "<" }
| OP_GTE { to_loc $loc, ">=" }
| OP_LTE { to_loc $loc, "<=" }
| AND    { to_loc $loc, "AND" }
| OR     { to_loc $loc, "OR" }

%inline prefixop:
  NOT { to_loc $loc, "NOT" }
%inline minisprefixop:
  OP_MIN { to_loc $loc, "-" }

%inline suffixop:
  OP_IS; NULL { to_loc $loc, "IS NULL" }
| OP_IS; NOT; NULL; { to_loc $loc, "IS NOT NULL" }

name:
  name = IDENT { to_loc $loc, name }

uname:
  name = IDENT { to_loc $loc, String.uppercase_ascii name }

scopename:
  name = separated_nonempty_list(DOT, name) { name }

ty:
  name = uname; { null (Ty name) }
| name = uname; NULL { null (Ty name) }
| name = uname; NOT; NULL { non_null (Ty name) }

ty_or_expr:
  ty = ty { Ty_val ty }
| ty = ty; EXPR { (Ty_expr ty) }

(* Utilities for flexible lists (and its non-empty version).

   A flexible list [flex_list(delim, X)] is the delimited with [delim] list of
   it [X] items where it is allowed to have a trailing [delim].

   A non-empty [nonempty_flex_list(delim, X)] version of flexible list is
   provided as well.

   From http://gallium.inria.fr/blog/lr-lists/

 *)

flex_list(delim, X):
    { [] }
  | x = X { [x] }
  | x = X; delim; xs = flex_list(delim, X) { x::xs }

nonempty_flex_list(delim, X):
    x = X { [x] }
  | x = X; delim; xs = flex_list(delim, X) { x::xs }
