open Sqlpp
module S = Mariadb.Nonblocking.Status

module LwtIO = struct
  type 'a t = 'a Lwt.t
  type 'a future = 'a t

  let ( >>= ) = Lwt.( >>= )
  let return = Lwt.return
end

module Mariadb_lwt = Mariadb.Nonblocking.Make (struct
  module IO = LwtIO

  let wait mariadb status =
    let open Lwt.Infix in
    let fd = Lwt_unix.of_unix_file_descr @@ Mariadb.Nonblocking.fd mariadb in
    assert (S.read status || S.write status || S.timeout status);
    let idle, _ = Lwt.task () in
    let rt = if S.read status then Lwt_unix.wait_read fd else idle in
    let wt = if S.write status then Lwt_unix.wait_write fd else idle in
    let tt =
      match S.timeout status, Mariadb.Nonblocking.timeout mariadb with
      | true, 0 -> Lwt.return ()
      | true, tmout -> Lwt_unix.timeout (float tmout)
      | false, _ -> idle
    in
    Lwt.catch
      (fun () ->
        Lwt.nchoose [ rt; wt; tt ] >>= fun _ ->
        Lwt.return
        @@ S.create ~read:(Lwt_unix.readable fd) ~write:(Lwt_unix.writable fd)
             ())
      (function
        | Lwt_unix.Timeout -> Lwt.return @@ S.create ~timeout:true ()
        | e -> Lwt.fail e)
end)

module IO = struct
  type 'a t = 'a Mariadb_lwt.result Lwt.t

  let ( >>= ) v f =
    Lwt.bind v (function Ok v -> f v | Error err -> Lwt.return (Error err))

  let return v = Lwt.return (Ok v)
end

module Sqlpp_db = Make (struct
  module IO = IO
  open IO

  type query = Mariadb_lwt.Stmt.t
  type row = Mariadb_lwt.Row.Array.t
  type db = Mariadb_lwt.t
  type 'a decode = row -> int -> 'a
  type date = Mariadb_lwt.Time.t
  type datetime = Mariadb_lwt.Time.t

  let fold' ~init ~f ~handle_res db sql =
    Mariadb_lwt.prepare db sql >>= fun stmt ->
    Mariadb_lwt.Stmt.execute stmt Array.empty >>= fun res ->
    handle_res res >>= fun res ->
    Mariadb_lwt.Stmt.close stmt >>= fun () -> return res

  let fold ~init ~f db sql =
    let handle_res res =
      let rec loop acc =
        Mariadb_lwt.Res.fetch (module Mariadb_lwt.Row.Array) res >>= function
        | None -> return acc
        | Some row -> loop (f row acc)
      in
      loop init
    in
    fold' ~init ~f ~handle_res db sql

  let exec db sql =
    let handle_res _res = Lwt.return_ok () in
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
    sprintf "%04d-%02d-%02d" (Mariadb_lwt.Time.year v)
      (Mariadb_lwt.Time.month v) (Mariadb_lwt.Time.day v)

  let encode_DATE_NULL = or_NULL encode_DATE

  let encode_DATETIME v =
    sprintf "%04d-%02d-%02d %02d:%02d:%02d" (Mariadb_lwt.Time.year v)
      (Mariadb_lwt.Time.month v) (Mariadb_lwt.Time.day v)
      (Mariadb_lwt.Time.hour v)
      (Mariadb_lwt.Time.minute v)
      (Mariadb_lwt.Time.second v)

  let encode_DATETIME_NULL = or_NULL encode_DATETIME

  let decode_BOOL : bool decode =
   fun row idx ->
    match Mariadb_lwt.Field.int row.(idx) with
    | 1 -> true
    | 0 -> false
    | _ -> assert false

  let or_NULL : 'a decode -> 'a option decode =
   fun f row idx ->
    if Mariadb_lwt.Field.null_value row.(idx) then None else Some (f row idx)

  let decode_INT : int decode = fun row idx -> Mariadb_lwt.Field.int row.(idx)

  let decode_FLOAT : float decode =
   fun row idx -> Mariadb_lwt.Field.float row.(idx)

  let decode_STRING : string decode =
   fun row idx -> Mariadb_lwt.Field.string row.(idx)

  let decode_DATE : Mariadb_lwt.Time.t decode =
   fun row idx -> Mariadb_lwt.Field.time row.(idx)

  let decode_DATETIME : Mariadb_lwt.Time.t decode =
   fun row idx -> Mariadb_lwt.Field.time row.(idx)

  let encode_STRING = Printer.quote_string
  let decode_INT_NULL = or_NULL decode_INT
  let decode_FLOAT_NULL = or_NULL decode_FLOAT
  let decode_STRING_NULL = or_NULL decode_STRING
  let decode_BOOL_NULL = or_NULL decode_BOOL
  let decode_DATE_NULL = or_NULL decode_DATE
  let decode_DATETIME_NULL = or_NULL decode_DATETIME

  let decode (row : row) idx : json =
    let field = row.(idx) in
    match Mariadb_lwt.Field.value field with
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
