
  $ alias p='./test_ppx'

  $ p 'select id from users where id in (select 1)' -print
  SELECT
    users.id AS id
  FROM users WHERE users.id IN (SELECT 1 AS _0 )
  let q =
    (assert false
      : unit -> (id:int -> 'acc -> 'acc, 'acc) Sqlpp_db.query)

  $ p 'select id from users where id in (select true)' -print
  let q =
    [%ocaml.error
      "expected BOOL NOT NULL but got INT NOT NULL"]

  $ p 'select id from users where id in (select 1, 2)' -print
  let q =
    [%ocaml.error
      "number of expressions (1) doesn't match number of \
       columns (2)"]

  $ p 'select 1 in (select id from users)' -print
  SELECT
    1 IN (SELECT users.id AS id FROM users) AS _0
  
  let q =
    (assert false
      : unit -> (_0:bool -> 'acc -> 'acc, 'acc) Sqlpp_db.query)
