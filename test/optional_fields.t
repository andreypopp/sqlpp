
  $ alias p='./test_ppx'

  $ p '
  > select one,three,five
  > from (
  >   select
  >     with 1 as one,
  >     with 2 as two,
  >     with 3 as three,
  >     with 4 as four,
  >     with 5 as five,
  >     with 6 as six,
  > ) as q
  > ' -print
  SELECT
    q.one AS one,
    q.three AS three,
    q.five AS five
  FROM (
    SELECT
      WITH 2 AS two,
      WITH 4 AS four,
      WITH 6 AS six,
      3 AS three,
      1 AS one,
      5 AS five
    ) AS q
  let q =
    (assert false
      : unit ->
        ( one:int -> three:int -> five:int -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)

  $ p '
  > select q.id
  > from (
  >   select
  >     id,
  >     with x + x as x,
  >   from (
  >     select id, with id as x from users
  >   ) as u
  > ) as q
  > ' -print
  SELECT
    q.id AS id
  FROM (
    SELECT
      u.id AS id,
      WITH u.x + u.x AS x
    FROM (
      SELECT
        users.id AS id,
        WITH users.id AS x
      FROM users) AS u) AS q
  let q =
    (assert false
      : unit -> (id:int -> 'acc -> 'acc, 'acc) Sqlpp_db.query)

  $ p '
  > select q.id, x
  > from (
  >   select
  >     id,
  >     with x + x as x,
  >   from (
  >     select id, with id as x from users
  >   ) as u
  > ) as q
  > ' -print
  SELECT
    q.id AS id,
    q.x AS x
  FROM (
    SELECT
      u.id AS id,
      u.x + u.x AS x
    FROM (
      SELECT users.id AS id, users.id AS x FROM users) AS u) AS q
  let q =
    (assert false
      : unit ->
        (id:int -> x:int -> 'acc -> 'acc, 'acc) Sqlpp_db.query)

We can use optional fields in the same scope as they are defined, in this case
we just copy the field expression instead of referencing it:
  $ p '
  > select with 1 as x, x+x
  > from users
  > ' -print
  SELECT 1 + 1 AS _1, WITH 1 AS x FROM users
  let q =
    (assert false
      : unit -> (_1:int -> 'acc -> 'acc, 'acc) Sqlpp_db.query)

  $ p '
  > select with 1 as x, with x + 2 as y, x + y
  > from users
  > ' -print
  SELECT
    1 + 1 + 2 AS _2,
    WITH 1 + 2 AS y,
    WITH 1 AS x
  FROM users
  let q =
    (assert false
      : unit -> (_2:int -> 'acc -> 'acc, 'acc) Sqlpp_db.query)

  $ p '
  > select with 1 as x, with x + user_id as y, x + y as x_plus_y
  > from (select with id as user_id from users) as u
  > ' -print
  SELECT
    1 + 1 + u.user_id AS x_plus_y,
    WITH 1 + u.user_id AS y,
    WITH 1 AS x
  FROM (
    SELECT users.id AS user_id FROM users) AS u
  let q =
    (assert false
      : unit ->
        (x_plus_y:int -> 'acc -> 'acc, 'acc) Sqlpp_db.query)
