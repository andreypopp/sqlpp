
  $ alias p='./test_ppx'

  $ p '
  > select users.id, stats.max(id), stats.count,
  > from users
  > join user_stats as stats
  > on users.id = stats.user_id
  > join user_stats as stats2
  > on users.id = stats2.user_id
  > ' -print
  SELECT
    users.id AS id,
    stats._2 AS _1,
    stats.count AS count
  FROM users
  JOIN (
    SELECT
      users.id AS user_id,
      max(users.id) AS _2,
      count(1) AS count
    FROM users
    GROUP BY users.id) AS stats
  ON users.id = stats.user_id
  JOIN (
    SELECT
      users.id AS user_id,
      WITH count(1) AS count
    FROM users
    GROUP BY users.id) AS stats2
  ON users.id = stats2.user_id
  let q =
    (assert false
      : unit ->
        ( id:int -> _1:int -> count:int -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)
