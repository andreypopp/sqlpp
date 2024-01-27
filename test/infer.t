
  $ alias p='./test_ppx'

  $ p 'select 1 as x'
  let q =
    (assert false
      : unit -> (x:int -> 'acc -> 'acc, 'acc) Sqlpp_db.query)

  $ p 'select 1'
  let q =
    (assert false
      : unit -> (_0:int -> 'acc -> 'acc, 'acc) Sqlpp_db.query)

match .. with syntax

  $ p '
  > select 
  >   match ?v with
  >   | Some ?x -> ?x + 1
  >   | None -> null:int
  >   end as v
  > '
  let q =
    (assert false
      : v:[ `None | `Some of int option ] ->
        unit ->
        (v:int option -> 'acc -> 'acc, 'acc) Sqlpp_db.query)

querying from tables

  $ p '
  > select id, name 
  > from users
  > '
  let q =
    (assert false
      : unit ->
        ( id:int -> name:string option -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)

  $ p '
  > select id, name, user_id
  > from users
  > join profiles on id = user_id
  > '
  let q =
    (assert false
      : unit ->
        ( id:int ->
          name:string option ->
          user_id:int ->
          'acc ->
          'acc,
          'acc )
        Sqlpp_db.query)

  $ p '
  > select id, name, user_id
  > from users
  > left join profiles on id = user_id
  > '
  let q =
    (assert false
      : unit ->
        ( id:int ->
          name:string option ->
          user_id:int option ->
          'acc ->
          'acc,
          'acc )
        Sqlpp_db.query)

paramter unification:

  $ p 'select ?x:int not null as x, ?x:int not null as y'
  let q =
    (assert false
      : x:int ->
        unit ->
        (x:int -> y:int -> 'acc -> 'acc, 'acc) Sqlpp_db.query)

  $ p 'select ?x:int null as x, ?x:int null as y'
  let q =
    (assert false
      : x:int option ->
        unit ->
        ( x:int option -> y:int option -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)

  $ p 'select ?x:int null as x, ?x:int not null as y'
  let q =
    (assert false
      : x:int ->
        unit ->
        ( x:int option -> y:int -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)

  $ p 'select ?x:int not null as x, ?x:int null as y'
  let q =
    (assert false
      : x:int ->
        unit ->
        (x:int -> y:int -> 'acc -> 'acc, 'acc) Sqlpp_db.query)

  $ p 'select ?x:int as x, ?x:int null as y'
  let q =
    (assert false
      : x:int option ->
        unit ->
        ( x:int option -> y:int option -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)

  $ p 'select ?x:int as x, ?x:int not null as y'
  let q =
    (assert false
      : x:int ->
        unit ->
        ( x:int option -> y:int -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)

  $ p 'select ?x:int as x, ?x:int null as y'
  let q =
    (assert false
      : x:int option ->
        unit ->
        ( x:int option -> y:int option -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)

  $ p 'select ?x:int not null as x, ?x:int as y'
  let q =
    (assert false
      : x:int ->
        unit ->
        (x:int -> y:int -> 'acc -> 'acc, 'acc) Sqlpp_db.query)

  $ p 'select ?x:int null as x, ?x:int as y'
  let q =
    (assert false
      : x:int option ->
        unit ->
        ( x:int option -> y:int option -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)

test that duplicate field names are not allowed:
  $ p 'select 1 as x, 2 as x'
  let q = [%ocaml.error "column `x` is already defined"]
