
  $ alias p='./test_ppx'

  $ p '
  > select
  >   x.users.id
  > from (select id from users) as x
  > ' -print
  SELECT
    x.id AS id
  FROM (SELECT users.id AS id FROM users) AS x
  let q =
    (assert false
      : unit -> (id:int -> 'acc -> 'acc, 'acc) Sqlpp_db.query)

  $ p '
  > select
  >   x.(id + id)
  > from (select id from users) as x
  > ' -print
  let q =
    [%ocaml.error
      "subquery `x` doesn't allow to build new expressions \
       inside"]

  $ p '
  > select
  >   id, x.id, x.users.id
  > from (select id, ... from users) as x
  > ' -print
  SELECT
    x.id AS id,
    x.id AS _1,
    x.id AS _2
  FROM (SELECT users.id AS id FROM users) AS x
  let q =
    (assert false
      : unit ->
        ( id:int -> _1:int -> _2:int -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)

  $ p '
  > select
  >   x.y.users.id,
  >   x.y.id,
  >   x.id,
  >   x.(id + y.id + y.users.id),
  >   x.y.users.created_at,
  >   stats.count(1),
  >   stats.max(id),
  > from (
  >   select y.id as id, ...
  >   from (
  >     select users.id as id, ... from users
  >   ) as y
  > ) as x
  > join (
  >   select id, ...
  >   from users
  >   group by id
  > ) as stats
  > on x.id = stats.id
  > ' -print
  SELECT
    x.id AS id,
    x.id AS _1,
    x.id AS _2,
    x._1 AS _3,
    x._2 AS created_at,
    stats._1 AS _5,
    stats._2 AS _6
  FROM (
    SELECT
      y.id AS id,
      y.id + y.id + y.id AS _1,
      y._1 AS _2
    FROM (
      SELECT
        users.id AS id,
        users.created_at AS _1
      FROM users) AS y) AS x
  JOIN (
    SELECT
      users.id AS id,
      count(1) AS _1,
      max(users.id) AS _2
    FROM users
    GROUP BY users.id) AS stats
  ON x.id = stats.id
  let q =
    (assert false
      : unit ->
        ( id:int ->
          _1:int ->
          _2:int ->
          _3:int ->
          created_at:float ->
          _5:int ->
          _6:int ->
          'acc ->
          'acc,
          'acc )
        Sqlpp_db.query)
