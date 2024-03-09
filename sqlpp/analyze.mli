open Syntax
open Scope

type params = pty NM.t
(** query params *)

(** type for query params... *)
and pty =
  | Pty of ty  (** ... is either a primitive type *)
  | Pty_variant of (name * pty list) list  (** ... or a tagged variant *)
  | Pty_expr of ty * scope
      (** or an expression of a given type within a given scope *)
  | Pty_unknown  (** ... or an unknown type *)

type row = (name * ty) list
(** this represents a query result *)

type query_info = {
  scope : scope;
  inner_scope : scope;
  params : params;
  row : row;
  query : query pos;
}

type fieldset_info = {
  fs_scopes : scopes;
  fs_fields : (ty * name * expr) list;
}

type env = env_decl NT.t
and env_decl = T of scope * Ddl.table | Q of query_info | F of fieldset_info

val analyze_query : ?src:string -> env -> query pos -> query_info
val analyze_fieldset : ?src:string -> env -> fieldset pos -> fieldset_info

val analyze_expr :
  ?ty:ty -> ?scope:scope -> ?src:string -> env -> exprsyn node -> expr
