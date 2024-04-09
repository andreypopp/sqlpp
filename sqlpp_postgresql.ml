open Sqlpp

let () =
  Stdlib.Printexc.register_printer (function
    | Postgresql.Error err -> Some (Postgresql.string_of_error err)
    | _ -> None)

module Row = struct
  type t = {
    mutable isnull : int -> bool;
    mutable tuple : string array;
    mutable ftype : int -> Postgresql.ftype;
  }

  let create () =
    {
      isnull = (fun _ -> false);
      tuple = [||];
      ftype = (fun _ -> Postgresql.INT2);
    }

  let set row res idx =
    row.isnull <- res#getisnull idx;
    row.tuple <- res#get_tuple idx;
    row.ftype <- res#ftype

  let isnull row idx = row.isnull idx
  let get row idx = row.tuple.(idx)
  let ftype row idx = row.ftype idx
end

module Sqlpp_types = struct
  type nonrec row = Row.t

  let or_NULL f = function None -> "NULL" | Some v -> f v
  let encode_BOOL = function true -> "TRUE" | false -> "FALSE"
  let encode_STRING = Printer.quote_string
  let encode_INT v = string_of_int v
  let encode_FLOAT v = string_of_float v
  let encode_BOOL_NULL = or_NULL encode_BOOL
  let encode_INT_NULL = or_NULL encode_INT
  let encode_FLOAT_NULL = or_NULL encode_FLOAT
  let encode_STRING_NULL = or_NULL encode_STRING

  type 'a decode = row -> int -> 'a

  let or_NULL : 'a decode -> 'a option decode =
   fun f row idx -> if row.isnull idx then None else Some (f row idx)

  let decode_BOOL : bool decode = fun row idx -> bool_of_string row.tuple.(idx)
  let decode_INT : int decode = fun row idx -> int_of_string row.tuple.(idx)

  let decode_FLOAT : float decode =
   fun row idx -> float_of_string row.tuple.(idx)

  let decode_STRING : string decode = fun row idx -> row.tuple.(idx)
  let encode_STRING = Printer.quote_string
  let decode_INT_NULL = or_NULL decode_INT
  let decode_FLOAT_NULL = or_NULL decode_FLOAT
  let decode_STRING_NULL = or_NULL decode_STRING
  let decode_BOOL_NULL = or_NULL decode_BOOL
end

module IO = struct
  type 'a t = ('a, string) result Lwt.t

  let ( >>= ) = Lwt_result.bind
  let return = Lwt_result.return
end

let db_socket db =
  (Obj.magic db#socket : Unix.file_descr)
  |> Lwt_unix.of_unix_file_descr ~blocking:false

module Postgresql_lwt = struct
  open Lwt.Infix

  let connect ~uri () =
    let db = new Postgresql.connection ~startonly:true ~conninfo:uri () in
    let rec finish_conn = function
      | Postgresql.Polling_failed ->
          Lwt.return_error (sprintf "Connection failed: %s" db#error_message)
      | Polling_reading ->
          Lwt_unix.wait_read (db_socket db) >>= fun () ->
          finish_conn db#connect_poll
      | Polling_writing ->
          Lwt_unix.wait_write (db_socket db) >>= fun () ->
          finish_conn db#connect_poll
      | Polling_ok -> Lwt.return_ok ()
    in
    finish_conn Polling_writing >>= function
    | Error msg -> Lwt.return_error msg
    | Ok () ->
        assert (Equal.poly db#status Ok);
        db#set_nonblocking true;
        Lwt.return_ok db
end

module Db = struct
  open Sqlpp_types
  open Lwt.Infix
  module IO = IO

  type query = unit
  type nonrec row = row
  type db = Postgresql.connection

  let rec wait_for_result (db : db) =
    let rec do_wait () =
      db#consume_input;
      if db#is_busy then Lwt_unix.wait_read (db_socket db) >>= do_wait
      else Lwt.return db#get_result
    in
    do_wait ()

  let fold' ~init ~f ~handle_res (db : db) sql =
    db#send_query sql;
    handle_res db

  let fold ~init ~f db sql =
    let handle_res db =
      let row = Row.create () in
      let rec loop acc =
        wait_for_result db >>= function
        | None -> Lwt.return_ok acc
        | Some res -> (
            match res#status with
            | Tuples_ok ->
                let len = res#ntuples in
                let rec loop_inner idx acc =
                  if idx = len then acc
                  else (
                    Row.set row res idx;
                    let acc = f row acc in
                    loop_inner (idx + 1) acc)
                in
                let acc = loop_inner 0 acc in
                loop acc
            | Empty_query | Bad_response | Nonfatal_error | Fatal_error ->
                let err = db#error_message in
                Lwt.return_error err
            | Command_ok -> failwith "unexpected: Command_ok"
            | Copy_out -> failwith "unexpected: Copy_out"
            | Copy_in -> failwith "unexpected: Copy_in"
            | Copy_both -> failwith "unexpected: Copy_both"
            | Single_tuple -> failwith "unexpected: Single_tuple")
      in
      loop init
    in
    fold' ~init ~f ~handle_res db sql

  let exec db sql =
    let handle_res res =
      let rec loop () =
        wait_for_result db >>= function
        | None -> Lwt.return_ok ()
        | Some res -> (
            match res#status with
            | Command_ok -> loop ()
            | Empty_query | Bad_response | Nonfatal_error | Fatal_error ->
                let err = db#error_message in
                Lwt.return_error err
            | Tuples_ok -> failwith "unexpected: Tuples_ok"
            | Copy_out -> failwith "unexpected: Copy_out"
            | Copy_in -> failwith "unexpected: Copy_in"
            | Copy_both -> failwith "unexpected: Copy_both"
            | Single_tuple -> failwith "unexpected: Single_tuple")
      in
      loop ()
    in
    fold' ~init:() ~f:(Fun.const ()) ~handle_res db sql

  let row_to_json (row : row) idx : json =
    if row.isnull idx then `Null
    else
      match row.ftype idx with
      | BOOL -> `Bool (decode_BOOL row idx)
      | INT8 | INT2 | INT4 -> `Int (decode_INT row idx)
      | FLOAT4 | FLOAT8 -> `Float (decode_FLOAT row idx)
      | BYTEA | CHAR | NAME | INT2VECTOR | REGPROC | TEXT | OID | TID | XID
      | CID | OIDVECTOR | POINT | LSEG | PATH | BOX | POLYGON | LINE | ABSTIME
      | RELTIME | TINTERVAL | UNKNOWN | CIRCLE | CASH | MACADDR | INET | CIDR
      | ACLITEM | BPCHAR | VARCHAR | DATE | TIME | TIMESTAMP | TIMESTAMPTZ
      | INTERVAL | TIMETZ | BIT | VARBIT | NUMERIC | REFCURSOR | REGPROCEDURE
      | REGOPER | REGOPERATOR | REGCLASS | REGTYPE | RECORD | CSTRING | ANY
      | ANYARRAY | VOID | TRIGGER | LANGUAGE_HANDLER | INTERNAL | OPAQUE
      | ANYELEMENT | JSONB | JSON ->
          `String (decode_STRING row idx)
      | exception Postgresql.Oid _ -> `String (decode_STRING row idx)

  class virtual ['ctx] printer =
    object (self)
      inherit ['ctx] Printer.printer as super
      method! private emit_Lit_string ctx s = self#emit ctx (encode_STRING s)
      method! private emit_Lit_bool ctx s = self#emit ctx (encode_BOOL s)
      method! private emit_Lit_int ctx s = self#emit ctx (encode_INT s)

      method! private emit_name ctx (_, name) =
        self#emit ctx (Printer.quote_ident name)
    end
end

module Sqlpp_db = Make (Db)
module Ppx = Sqlpp_ppx.Make (Db)

let env = Ppx.env
let () = Ppx.register ()
