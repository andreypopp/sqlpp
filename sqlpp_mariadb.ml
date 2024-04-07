open Sqlpp
module M = Mariadb.Blocking

module Sqlpp_db = Make (struct
  type query = M.Stmt.t
  type row = M.Row.Array.t
  type db = M.t
  type 'a decode = row -> int -> 'a
  type date = M.Time.t
  type datetime = M.Time.t

  let fold' ~init ~f ~handle_res db sql =
    let open Result in
    let result =
      let* stmt = M.prepare db sql in
      let* res = M.Stmt.execute stmt Array.empty in
      let* s = handle_res res in
      let+ () = M.Stmt.close stmt in
      s
    in
    match result with
    | Ok r -> r
    | Error (i, e) -> failwith @@ sprintf "%s: (%d)" e i

  let fold ~init ~f db sql =
    let handle_res res =
      let rec loop acc =
        res
        |> M.Res.fetch (module M.Row.Array)
        |> Result.flat_map
             (Option.map_or (fun x -> loop (f x acc)) ~default:(Ok acc))
      in
      loop init
    in
    fold' ~init ~f ~handle_res db sql

  let exec db sql =
    let handle_res _res = Ok () in
    fold' ~init:() ~f:(Fun.const ()) ~handle_res db sql

  let or_NULL f = function None -> "NULL" | Some v -> f v
  let encode_BOOL = function true -> "TRUE" | false -> "FALSE"
  let encode_STRING = Printer.quote_string
  let encode_INT v = string_of_int v
  let encode_FLOAT v = string_of_float v
  let encode_BOOL_NULL = or_NULL encode_BOOL
  let encode_INT_NULL = or_NULL encode_INT
  let encode_FLOAT_NULL = or_NULL encode_FLOAT
  let encode_STRING_NULL = or_NULL encode_STRING

  let encode_DATE v =
    sprintf "%04d-%02d-%02d" (M.Time.year v) (M.Time.month v) (M.Time.day v)

  let encode_DATE_NULL = or_NULL encode_DATE

  let encode_DATETIME v =
    sprintf "%04d-%02d-%02d %02d:%02d:%02d" (M.Time.year v) (M.Time.month v)
      (M.Time.day v) (M.Time.hour v) (M.Time.minute v) (M.Time.second v)

  let encode_DATETIME_NULL = or_NULL encode_DATETIME

  let decode_BOOL : bool decode =
   fun row idx ->
    match M.Field.int row.(idx) with
    | 1 -> true
    | 0 -> false
    | _ -> assert false

  let or_NULL : 'a decode -> 'a option decode =
   fun f row idx ->
    if M.Field.null_value row.(idx) then None else Some (f row idx)

  let decode_INT : int decode = fun row idx -> M.Field.int row.(idx)
  let decode_FLOAT : float decode = fun row idx -> M.Field.float row.(idx)
  let decode_STRING : string decode = fun row idx -> M.Field.string row.(idx)
  let decode_DATE : M.Time.t decode = fun row idx -> M.Field.time row.(idx)
  let decode_DATETIME : M.Time.t decode = fun row idx -> M.Field.time row.(idx)
  let encode_STRING = Printer.quote_string
  let decode_INT_NULL = or_NULL decode_INT
  let decode_FLOAT_NULL = or_NULL decode_FLOAT
  let decode_STRING_NULL = or_NULL decode_STRING
  let decode_BOOL_NULL = or_NULL decode_BOOL
  let decode_DATE_NULL = or_NULL decode_DATE
  let decode_DATETIME_NULL = or_NULL decode_DATETIME

  let decode (row : row) idx : json =
    let field = row.(idx) in
    match M.Field.value field with
    | `Float f -> `Float f
    | `Int i -> `Int i
    | `String s -> `String s
    | `Bytes b -> `String (Bytes.to_string b)
    | `Time t -> `String (encode_DATETIME t)
    | `Null -> `Null

  class virtual ['ctx] printer =
    object (self)
      inherit ['ctx] Printer.printer as super
      method! private emit_Lit_string ctx s = self#emit ctx (encode_STRING s)
      method! private emit_Lit_bool ctx s = self#emit ctx (encode_BOOL s)
      method! private emit_Lit_int ctx s = self#emit ctx (encode_INT s)

      method! private emit_name ctx (_, name) =
        self#emit ctx (Printer.quote_ident_backticks name)
    end
end)

module Ppx = Sqlpp_ppx.Make (struct
  module Sqlpp_db = Sqlpp_db
end)

let env = Ppx.env
let () = Ppx.register ()
