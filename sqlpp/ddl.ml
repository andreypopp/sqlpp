type t =
  | CREATE_TABLE of table
  | DROP_TABLE of { table : string }
  | ALTER_TABLE_RENAME of { table : string; new_table : string }
  | ALTER_TABLE_RENAME_COLUMN of {
      table : string;
      column : string;
      new_column : string;
    }
  | ALTER_TABLE_DROP_COLUMN of { table : string; column : string }
  | ALTER_TABLE_ADD_COLUMN of {
      table : string;
      column : column;
      default : string option;
    }

and table = { name : string; columns : column list; primary_key : string list }

and column = {
  name : string;
  ty : Syntax.ty;
  primary_key : bool;
  autoincrement : bool;
  default : string option;
  extra : string option;
}

let table name columns = { name; columns; primary_key = [] }

let column ?extra ?default ?(primary_key = false) ?(autoincrement = false) name
    ty =
  { name; ty; extra; autoincrement; primary_key; default }
