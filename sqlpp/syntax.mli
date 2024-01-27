(** PRELUDE *)

type loc = Warnings.loc

val dummy_loc : loc

type 'a pos = loc * 'a
(** data with location *)

type 'a node = private { node : 'a; eq : 'a Eq_class.t; loc : loc }
(** data with location and fast equality *)

(** NAME *)

type name = string pos

val name : ?loc:loc -> string -> name
val compare_name : name ord
val equal_name : name eq
val name_to_string : name -> string

module NM : Map.S with type key = name
module NT : Hashtbl.S with type key = name

(** NULLABLE *)

type 'a nullable = { v : 'a; nullable : [ `null | `non_null ] }
(** value that may be null *)

val non_null : 'a -> 'a nullable
val null : 'a -> 'a nullable
val nullable_map : 'a nullable -> ('a -> 'b) -> 'b nullable
val nullable_lub : 'a nullable -> 'b nullable -> 'b nullable
val nullable_glb : 'a nullable -> 'b nullable -> 'b nullable

(** TYPES *)

type ty = tysyn nullable
and tysyn = Ty of name | Ty_one_of of (string option * name list)

type ty_or_expr = Ty_val of ty | Ty_expr of ty

val hash_ty : ty hash
val equal_ty : ty eq
val ty_to_string : ty -> string
val ty_lub : ?src:'a -> ?loc:'b -> ty -> ty -> ty
val ty_glb : ?src:string -> ?loc:Location.t -> ty -> ty -> ty

(** SCOPE TYPES *)

type sty = Sty_name of name | Sty_struct of (name * sty_elem) list
and sty_elem = Sty_elem_ty of ty | Sty_elem_scope of sty

(** TYPE CONSTRUCTORS *)

val bool : tysyn
val string : tysyn
val int : tysyn
val float : tysyn
val datetime : tysyn
val date : tysyn
val time : tysyn
val interval : tysyn
val numeric : tysyn

(** LITERALS *)

type lit = Lit_int of int | Lit_string of string | Lit_bool of bool

(** SYNTAX *)

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

and select = selectsyn node
(** SELECT ... FROM ... *)

and updatesyn = {
  update_set : (name * expr) list;
  update_table : name;
  update_where : expr option;
  update_from : from pos option;
}

and update = updatesyn node
(** UPDATE ... SET ... *)

and insertsyn = {
  insert_table : name;
  insert_columns : name list;
  insert_from : insert_from;
  insert_on_conflict : on_conflict option;
}

and on_conflict = On_conflict_replace | On_conflict_ignore

and insert = insertsyn node
(** INSERT INTO ... *)

and deletesyn = { delete_table : name; delete_where : expr option }

and delete = deletesyn node
(** DELETE FROM ... WHERE ... *)

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

and expr_match_case = name * name list * expr

and expr = exprsyn node
(** expressions *)

(** select fields *)
and select_field =
  | Field of { name : name option; expr : expr; is_used : bool }
      (** field within the SELECT clause *)
  | Field_with_scope of name list * name
  | Field_fieldset of { name : name; args : name list list; is_used : bool }

(** FIELDS *)

val equal_select_field : select_field eq

(** EXPRESSIONS *)

val equal_expr : expr eq
val hash_expr : expr hash
val expr_to_string : expr -> string
val print_expr : expr -> PPrint.document

(** EXPRESSION CONSTRUCTORS *)

val expr_app : ?loc:loc -> name -> expr list -> expr
val expr_lit : ?loc:loc -> lit -> expr
val expr_name : ?loc:loc -> name -> expr
val expr_in : ?loc:loc -> expr list -> select -> expr
val expr_ascribe : ?loc:loc -> expr -> ty_or_expr -> expr
val expr_param : ?loc:loc -> name -> expr
val expr_nav : ?loc:loc -> name -> expr -> expr
val expr_match : ?loc:loc -> name -> (name * name list * expr) list -> expr
val expr_null : ?loc:loc -> unit -> exprsyn node

(** QUERY CONSTRUCTORS *)

val select :
  ?loc:loc ->
  ?is_open:bool ->
  ?group_by:expr list ->
  ?having:expr ->
  ?order_by:(expr * dir) list ->
  ?where:expr ->
  ?from:from pos ->
  select_field list ->
  select

val update :
  ?loc:loc ->
  ?from:from pos ->
  ?where:expr ->
  name ->
  (name * expr) list ->
  update

val insert :
  ?loc:loc ->
  ?on_conflict:on_conflict ->
  name ->
  name list ->
  insert_from ->
  insert

val delete : ?loc:loc -> ?where:expr -> name -> delete

(** OUT OF BAND FIELDS *)

type field = {
  name : name;
  expr : expr;
  ty : ty;
  is_generated : bool;
  mutable is_used : bool;
  dependencies : (name option * name) list;
}
(** field generated by the propagating nested Expr_nav *)

val make_field :
  ?dependencies:(name option * name) list ->
  is_generated:bool ->
  is_used:bool ->
  name ->
  expr ->
  ty ->
  field

val fresh_field : field -> field
val equal_field : field eq

(** DECLARATIONS *)

type decl =
  | Decl_table of name * (name * ty) list
  | Decl_query of name * query pos
  | Decl_fieldset of name * fieldset pos

and fieldset = {
  fieldset_args : (name * sty) list;
  fieldset_exprs : (name * expr) list;
}

(** DECLARATIONS *)

(** UTILITIES *)

val field_name : expr -> name option

val classify_app :
  name ->
  expr list ->
  [ `Binop of name * expr * expr
  | `Prefixop of name * expr
  | `Suffixop of name * expr
  | `Function of name * expr list ]

(** fold over the syntax *)
class virtual ['a, 'ctx] fold : object
  method fold_query : 'ctx -> query pos -> 'a -> 'a
  method fold_select : 'ctx -> select -> 'a -> 'a
  method fold_insert : 'ctx -> insert -> 'a -> 'a
  method fold_delete : 'ctx -> delete -> 'a -> 'a
  method fold_update : 'ctx -> update -> 'a -> 'a
  method fold_select_proj : 'ctx -> select_field list -> 'a -> 'a
  method fold_select_where : 'ctx -> expr -> 'a -> 'a
  method fold_select_field : 'ctx -> select_field -> 'a -> 'a
  method fold_select_group_by : 'ctx -> expr list -> 'a -> 'a
  method fold_select_having : 'ctx -> expr -> 'a -> 'a
  method fold_select_order_by : 'ctx -> (expr * dir) list -> 'a -> 'a
  method fold_order : 'ctx -> expr * dir -> 'a -> 'a
  method fold_from : 'ctx -> from pos -> 'a -> 'a
  method fold_from_one : 'ctx -> from_one pos -> 'a -> 'a
  method fold_From_select : 'ctx -> select -> name -> 'a -> 'a
  method fold_From_table : 'ctx -> name -> name option -> 'a -> 'a
  method fold_From_join : 'ctx -> from pos -> from_one pos -> expr -> 'a -> 'a
  method fold_expr : 'ctx -> expr -> 'a -> 'a
  method fold_Expr_app : 'ctx -> name -> expr list -> 'a -> 'a
  method fold_Expr_ascribe : 'ctx -> expr -> ty_or_expr -> 'a -> 'a
  method fold_Expr_lit : 'ctx -> lit -> 'a -> 'a
  method fold_Expr_match : 'ctx -> name -> expr_match_case list -> 'a -> 'a
  method fold_Expr_name : 'ctx -> name -> 'a -> 'a
  method fold_Expr_nav : 'ctx -> name -> expr -> 'a -> 'a
  method fold_Expr_null : 'ctx -> 'a -> 'a
  method fold_Expr_param : 'ctx -> name -> 'a -> 'a
end

open PPrint

(** pretty-printer *)
class ['ctx] format : object
  method format_query : 'ctx -> query pos -> document
  method format_select : 'ctx -> select -> document
  method format_insert : 'ctx -> insert -> document
  method format_update : 'ctx -> update -> document
  method format_delete : 'ctx -> delete -> document
  method format_dir : dir -> document
  method format_expr : int option -> 'ctx -> expr -> document
  method format_expr_app : int option -> 'ctx -> name * expr list -> document
  method format_field : 'ctx -> select_field -> document
  method format_fields : 'ctx -> select_field Seq.t -> document
  method format_from : 'ctx -> from pos -> document
  method format_from_one : 'ctx -> from_one pos -> document
  method format_From_select : 'ctx -> select -> name -> document
  method format_From_table : 'ctx -> name -> name option -> document
  method format_group_by : 'ctx -> expr list option -> document
  method format_having : 'ctx -> expr option -> document
  method format_join_cond : 'ctx -> expr -> document
  method format_join_kind : join_kind -> document
  method format_kw : string -> document
  method format_lit : lit -> document
  method format_name : name -> document
  method format_order : 'ctx -> expr * dir -> document
  method format_order_by : 'ctx -> (expr * dir) list option -> document
  method format_param : name -> document
  method format_update_set : 'ctx -> name * expr -> document
  method format_values : 'ctx -> expr list -> document
  method format_where : 'ctx -> expr option -> document
end
