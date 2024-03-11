
  $ alias p='./test_ppx'

it is possible to use `withscope` to setup scope aliases
  $ p '
  > select 
  >   withscope x.z as x,
  >   withscope agg.y as agg,
  >   x.id,
  >   agg.count(1)
  > from (
  >   select withscope y.users as z, ...
  >   from (
  >     select ... 
  >     from users) as y) as x
  > join (
  >   select ... 
  >   from (
  >     select id, ...
  >     from users
  >     group by id) as y
  > ) as agg
  > on x.y.id = agg.y.id
  > ' -print
  SELECT
    x._0 AS id,
    agg._1 AS _1
  FROM (
    SELECT
      y._0 AS _0
    FROM (SELECT users.id AS _0 FROM users) AS y) AS x
  JOIN (
    SELECT
      y.id AS _0,
      y._1 AS _1
    FROM (
      SELECT
        users.id AS id,
        count(1) AS _1
      FROM users
      GROUP BY users.id) AS y) AS agg
  ON x._0 = agg._0
  let q =
    (assert false
      : unit ->
        ( id:int -> _1:int -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)

unqualified columns names will also be looked up in scope aliases:
  $ p '
  > select
  >   withscope x.y.z as u,
  >   name
  > from (select ... from (select ... from (select ... from users) as z) as y) as x
  > ' -print
  SELECT
    x._0 AS name
  FROM (
    SELECT
      y._0 AS _0
    FROM (
      SELECT
        z._0 AS _0
      FROM (SELECT users.name AS _0 FROM users) AS z) AS y) AS x
  let q =
    (assert false
      : unit ->
        ( name:string option -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)

one more example which showcases nested aliases:
  $ p '
  > select withscope x.users as users, users.name as name
  > from (
  >   select withscope y.users as users, ... from (
  >     select withscope z.users as users, ... from (select ... from users) as z) as y) as x
  > ' -print
  SELECT
    x._0 AS name
  FROM (
    SELECT
      y._0 AS _0
    FROM (
      SELECT
        z._0 AS _0
      FROM (SELECT users.name AS _0 FROM users) AS z) AS y) AS x
  let q =
    (assert false
      : unit ->
        ( name:string option -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)

shadowing is also supported:
  $ p '
  > select
  >   x.id as xid,
  >   withscope y as x,
  >   x.id as yid,
  > from users as x
  > join users as y on true
  > ' -print
  SELECT
    x.id AS xid,
    y.id AS yid
  FROM users AS x JOIN users AS y ON TRUE
  let q =
    (assert false
      : unit ->
        ( xid:int -> yid:int -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)
