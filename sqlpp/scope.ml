open Syntax

type scope = {
  scopes : scopes;
  fields : fields;
  is_open : bool;
  group_by : expr list option;
}

and scopes = (name * scope_elem) list
and fields = field NT.t
and scope_elem = S of scope nullable | A of name list

module Scopes = struct
  let rec lookup_step n = function
    | [] -> `none
    | (n', v) :: s when equal_name n n' -> (
        match v with S v -> `Scope v | A ns -> `Alias (ns, s))
    | _ :: xs -> lookup_step n xs

  let lookup' =
    let rec lookup nav n = function
      | [] -> None
      | (n', v) :: s when equal_name n n' -> (
          match v with S v -> Some (nav, v) | A ns -> traverse nav s ns)
      | _ :: xs -> lookup nav n xs
    and traverse nav s = function
      | [] -> failwithf "Scopes.lookup': no such scope"
      | n :: ns -> (
          match lookup (n :: nav) n s with
          | None -> None
          | Some (nav, s) -> (
              match ns with
              | [] -> Some (nav, s)
              | ns -> traverse nav s.v.scopes ns))
    in
    lookup []

  let lookup n s = Option.map (fun (_, s) -> s.v) (lookup' n s)

  let rec lookup_many ns s =
    match ns with
    | [] -> failwithf "Scopes.lookup_many: no such scope"
    | n :: ns ->
        Option.bind (lookup n s) (fun s ->
            match ns with [] -> Some s | ns -> lookup_many ns s.scopes)
end

let scope_subscope scope name =
  let rec aux name = function
    | (n, S scope) :: _ when equal_name n name -> Some scope.v
    | (_, S _) :: scopes -> aux name scopes
    | (_, A _) :: scopes -> aux name scopes
    | [] -> None
  in
  aux name scope.scopes

let scope_create ?group_by ?(is_open = false) ?fields:fields' ?(scopes = []) ()
    =
  let fields = NT.create 10 in
  Option.iter (List.iter ~f:(fun f -> NT.replace fields f.name f)) fields';
  { scopes; fields; is_open; group_by }

let scope_fields scope = NT.to_seq_values scope.fields

let fresh =
  let rec copy_scope { scopes; fields; is_open; group_by } =
    {
      scopes = copy_scopes scopes;
      fields = copy_fields fields;
      is_open;
      group_by;
    }
  and copy_scopes s = List.map ~f:(fun (n, v) -> n, copy_scopeb v) s
  and copy_scopeb = function
    | S s -> S { s with v = copy_scope s.v }
    | A n -> A n
  and copy_fields c =
    let c' = NT.create (NT.length c) in
    NT.iter
      (fun k v ->
        let v = fresh_field v in
        NT.replace c' k v)
      c;
    c'
  in
  copy_scope
