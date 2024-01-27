type t = {
  loc : Warnings.loc;
  src : string option;
  msg : string;
  props : (string * PPrint.document) list;
}

let no_fname = "_none_"

let lines_around ~at src =
  let lines = String.split src ~by:"\n" in
  let rec aux n lines in_lines =
    if n = 0 then List.rev lines
    else
      match in_lines with
      | [] -> List.rev lines
      | line :: in_lines ->
          if n <= 3 then aux (n - 1) ((2 = n, line) :: lines) in_lines
          else aux (n - 1) lines in_lines
  in
  match lines with
  | [] -> []
  | [ line ] -> [ true, line ]
  | lines -> aux (at + 1) [] lines

let pp_props fmt props =
  let open PPrint in
  let doc =
    match props with
    | [] -> empty
    | props ->
        concat
        @@ List.map props ~f:(fun (name, doc) ->
               let key = string (sprintf "%s:" name) in
               key ^^ hardline ^^ group (nest 2 (break 1 ^^ doc)) ^^ hardline)
  in
  ToFormatter.pretty 1. 79 fmt doc

let pp_snippet fmt (src, msg, loc) =
  let fname = loc.Location.loc_start.pos_fname in
  if String.equal fname no_fname && Option.is_none src then
    Format.fprintf fmt "%s@." msg
  else
    let src =
      match src with
      | Some src -> Some src
      | None -> (
          try Some (In_channel.with_open_bin fname In_channel.input_all)
          with _ -> None)
    in
    match src with
    | None -> Format.fprintf fmt "%s@." msg
    | Some src ->
        let lines = lines_around ~at:loc.Location.loc_start.pos_lnum src in
        List.iter lines ~f:(fun (is_focus, line) ->
            Format.fprintf fmt "│ %s" line;
            Format.pp_print_newline fmt ();
            if is_focus then (
              let s = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
              let s = max 0 s in
              Format.fprintf fmt "│ %s⮬ %s" (String.make s ' ') msg;
              Format.pp_print_newline fmt ()))

let pp fmt { loc; msg; src; props } =
  match Stdlib.( = ) loc Location.none with
  | true ->
      Format.fprintf fmt "%s@." msg;
      pp_props fmt props
  | false ->
      Format.fprintf fmt "%a@." Location.print_loc loc;
      pp_snippet fmt (src, msg, loc);
      pp_props fmt props

let reraise exn =
  Stdlib.Printexc.(raise_with_backtrace exn (get_raw_backtrace ()))

exception Error of t

let errorf ?src ?loc ?(props = []) =
  let loc = Option.value loc ~default:Location.none in
  Format.kasprintf (fun msg -> raise (Error { loc; msg; src; props }))

let with_src ?src f =
  try f ()
  with Error { loc; msg; props; src = _ } ->
    raise (Error { loc; msg; src; props })
