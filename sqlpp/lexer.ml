open Parser

let string_of_token = function
  | EXPR -> "EXPR"
  | CONFLICT -> "CONFLICT"
  | IGNORE -> "IGNORE"
  | REPLACE -> "REPLACE"
  | IN -> "IN"
  | AND -> "AND"
  | AS -> "AS"
  | ASC -> "ASC"
  | BY -> "BY"
  | COLON -> "COLON"
  | COMMA -> ","
  | SEMI -> ";"
  | DESC -> "DESC"
  | DOT -> "DOT"
  | EOF -> "<EOF>"
  | FROM -> "FROM"
  | GROUP -> "GROUP"
  | IDENT string -> string
  | OP_IS -> "IS"
  | JOIN -> "JOIN"
  | LEFT -> "LEFT"
  | LIT (Lit_int v) -> sprintf "LIT %i" v
  | LIT (Lit_string v) -> sprintf "LIT %s" v
  | LIT (Lit_bool v) -> sprintf "LIT %b" v
  | LPAREN -> "("
  | NOT -> "NOT"
  | NULL -> "NULL"
  | ON -> "ON"
  | OP_ADD -> "+"
  | OP_DIV -> "/"
  | OP_EQ -> "="
  | OP_MIN -> "-"
  | OP_MUL -> "*"
  | OP_NEQ -> "!="
  | OP_GT -> ">"
  | OP_LT -> "<"
  | OP_GTE -> ">="
  | OP_LTE -> "<="
  | OR -> "OR"
  | ORDER -> "ORDER"
  | RPAREN -> ")"
  | SELECT -> "SELECT"
  | INSERT -> "INSERT"
  | UPDATE -> "UPDATE"
  | DELETE -> "DELETE"
  | SET -> "SET"
  | INTO -> "INTO"
  | VALUES -> "VALUES"
  | WHERE -> "WHERE"
  | HAVING -> "HAVING"
  | PARAM p -> sprintf "?%s" p
  | MATCH -> "MATCH"
  | WITH -> "WITH"
  | WITHSCOPE -> "WITHSCOPE"
  | BAR -> "|"
  | RARROW -> "->"
  | END -> "END"
  | ELLIPSIS -> "..."
  | CREATE -> "CREATE"
  | QUERY -> "QUERY"
  | TABLE -> "TABLE"
  | FIELDSET -> "FIELDSET"
  | FIELDSET_SPLICE name -> sprintf "...%s" name

exception Error of Lexing.position * string

let ident_first = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z' | '_']
let ident_rest = [%sedlex.regexp? ident_first | '0' .. '9']
let integer_first = [%sedlex.regexp? '0' .. '9']
let integer_rest = [%sedlex.regexp? '0' .. '9']

let binop =
  [%sedlex.regexp?
    Plus
      ( '+' | '-' | '*' | '/' | '<' | '>' | '=' | '~' | '!' | '@' | '#' | '%'
      | '^' | '&' | '|' | '`' | '?' )]

let a = [%sedlex.regexp? 'a' | 'A']
let b = [%sedlex.regexp? 'b' | 'B']
let c = [%sedlex.regexp? 'c' | 'C']
let d = [%sedlex.regexp? 'd' | 'D']
let e = [%sedlex.regexp? 'e' | 'E']
let f = [%sedlex.regexp? 'f' | 'F']
let g = [%sedlex.regexp? 'g' | 'G']
let h = [%sedlex.regexp? 'h' | 'H']
let i = [%sedlex.regexp? 'i' | 'I']
let j = [%sedlex.regexp? 'j' | 'J']
let k = [%sedlex.regexp? 'k' | 'K']
let l = [%sedlex.regexp? 'l' | 'L']
let m = [%sedlex.regexp? 'm' | 'M']
let n = [%sedlex.regexp? 'n' | 'N']
let o = [%sedlex.regexp? 'o' | 'O']
let p = [%sedlex.regexp? 'p' | 'P']
let q = [%sedlex.regexp? 'q' | 'Q']
let r = [%sedlex.regexp? 'r' | 'R']
let s = [%sedlex.regexp? 's' | 'S']
let t = [%sedlex.regexp? 't' | 'T']
let u = [%sedlex.regexp? 'u' | 'U']
let v = [%sedlex.regexp? 'v' | 'V']
let w = [%sedlex.regexp? 'w' | 'W']
let x = [%sedlex.regexp? 'x' | 'X']
let y = [%sedlex.regexp? 'y' | 'Y']
let z = [%sedlex.regexp? 'z' | 'Z']

let rec token buf =
  match%sedlex buf with
  | eof -> EOF
  | '-', '-', Star (Compl '\n'), '\n' -> token buf
  | s, e, l, e, c, t -> SELECT
  | i, n, s, e, r, t -> INSERT
  | u, p, d, a, t, e -> UPDATE
  | d, e, l, e, t, e -> DELETE
  | s, e, t -> SET
  | i, n, t, o -> INTO
  | v, a, l, u, e, s -> VALUES
  | f, r, o, m -> FROM
  | l, e, f, t -> LEFT
  | j, o, i, n -> JOIN
  | a, s -> AS
  | w, h, e, r, e -> WHERE
  | h, a, v, i, n, g -> HAVING
  | o, r, d, e, r -> ORDER
  | g, r, o, u, p -> GROUP
  | b, y -> BY
  | a, s, c -> ASC
  | o, n -> ON
  | d, e, s, c -> DESC
  | n, o, t -> NOT
  | a, n, d -> AND
  | o, r -> OR
  | n, u, l, l -> NULL
  | i, n -> IN
  | t, r, u, e -> LIT (Syntax.Lit_bool true)
  | f, a, l, s, e -> LIT (Syntax.Lit_bool false)
  | c, r, e, a, t, e -> CREATE
  | q, u, e, r, y -> QUERY
  | t, a, b, l, e -> TABLE
  | f, i, e, l, d, s, e, t -> FIELDSET
  | c, o, n, f, l, i, c, t -> CONFLICT
  | r, e, p, l, a, c, e -> REPLACE
  | i, g, n, o, r, e -> IGNORE
  | e, x, p, r -> EXPR
  | '=' -> OP_EQ
  | '!', '=' -> OP_NEQ
  | '+' -> OP_ADD
  | '-' -> OP_MIN
  | '*' -> OP_MUL
  | '/' -> OP_DIV
  | '>' -> OP_GT
  | '<' -> OP_LT
  | '>', '=' -> OP_GTE
  | '<', '=' -> OP_LTE
  | i, s -> OP_IS
  | '(' -> LPAREN
  | ')' -> RPAREN
  | ',' -> COMMA
  | ';' -> SEMI
  | '.' -> DOT
  | '.', '.', '.', ident_first, Star ident_rest ->
      let len = Sedlexing.lexeme_length buf - 3 in
      FIELDSET_SPLICE (Sedlexing.Utf8.sub_lexeme buf 3 len)
  | '.', '.', '.' -> ELLIPSIS
  | ':' -> COLON
  | '|' -> BAR
  | e, n, d -> END
  | '-', '>' -> RARROW
  | m, a, t, c, h -> MATCH
  | w, i, t, h -> WITH
  | w, i, t, h, s, c, o, p, e -> WITHSCOPE
  | ' ' | '\t' -> token buf
  | '\n' -> token buf
  | integer_first, Star integer_rest ->
      LIT (Syntax.Lit_int (Int.of_string_exn (Sedlexing.Utf8.lexeme buf)))
  | ident_first, Star ident_rest -> IDENT (Sedlexing.Utf8.lexeme buf)
  | '?', ident_first, Star ident_rest ->
      let len = Sedlexing.lexeme_length buf - 1 in
      PARAM (Sedlexing.Utf8.sub_lexeme buf 1 len)
  | '"' ->
      let buffer = Buffer.create 1024 in
      let rec scan () =
        match%sedlex buf with
        | '\\' ->
            let skip () =
              match%sedlex buf with
              | any ->
                  Buffer.add_string buffer (Sedlexing.Utf8.lexeme buf);
                  scan ()
              | _ ->
                  let pos = fst @@ Sedlexing.lexing_positions buf in
                  raise (Error (pos, "identifier not closed"))
            in
            skip ()
        | '"' -> IDENT (Buffer.contents buffer)
        | any ->
            Buffer.add_string buffer (Sedlexing.Utf8.lexeme buf);
            scan ()
        | _ ->
            let pos = fst @@ Sedlexing.lexing_positions buf in
            raise (Error (pos, "identifier not closed"))
      in
      scan ()
  | '\'' ->
      let buffer = Buffer.create 1024 in
      let rec scan () =
        match%sedlex buf with
        | '\\' ->
            let skip () =
              match%sedlex buf with
              | any ->
                  Buffer.add_string buffer (Sedlexing.Utf8.lexeme buf);
                  scan ()
              | _ ->
                  let pos = fst @@ Sedlexing.lexing_positions buf in
                  raise (Error (pos, "string not closed"))
            in
            skip ()
        | '\'' -> LIT (Syntax.Lit_string (Buffer.contents buffer))
        | any ->
            Buffer.add_string buffer (Sedlexing.Utf8.lexeme buf);
            scan ()
        | _ ->
            let pos = fst @@ Sedlexing.lexing_positions buf in
            raise (Error (pos, "string not closed"))
      in
      scan ()
  | '`' ->
      let buffer = Buffer.create 1024 in
      let rec scan () =
        match%sedlex buf with
        | '\\' ->
            Buffer.add_string buffer (Sedlexing.Utf8.lexeme buf);
            let skip () =
              match%sedlex buf with
              | any ->
                  Buffer.add_string buffer (Sedlexing.Utf8.lexeme buf);
                  scan ()
              | _ ->
                  let pos = fst @@ Sedlexing.lexing_positions buf in
                  raise (Error (pos, "identifier not closed"))
            in
            skip ()
        | '`' -> IDENT (Buffer.contents buffer)
        | any ->
            Buffer.add_string buffer (Sedlexing.Utf8.lexeme buf);
            scan ()
        | _ ->
            let pos = fst @@ Sedlexing.lexing_positions buf in
            raise (Error (pos, "identifier not closed"))
      in
      scan ()
  | _ ->
      let pos = fst @@ Sedlexing.lexing_positions buf in
      raise (Error (pos, Printf.sprintf "Unexpected token"))

let lexer buf = Sedlexing.with_tokenizer token buf
