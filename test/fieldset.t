  $ alias p='./test_ppx'

it is possible to define a fieldset and then splice it into a query:
  $ p '
  > select with ...users_fields(users) from users
  > ' -print
  SELECT
    WITH users.id AS user_id,
    WITH users.name AS user_name
  FROM users
  let q =
    (assert false
      : unit -> ('acc -> 'acc, 'acc) Sqlpp_db.query)

without `with` the fieldset will actually select the fields:
  $ p '
  > select ...users_fields(users) from users
  > ' -print
  SELECT
    users.id AS user_id,
    users.name AS user_name
  FROM users
  let q =
    (assert false
      : unit ->
        ( user_id:int ->
          user_name:string option ->
          'acc ->
          'acc,
          'acc )
        Sqlpp_db.query)

one can later use fields from the fieldset:
  $ p '
  > select with ...users_fields(users), user_id from users
  > ' -print
  SELECT
    WITH users.id AS user_id,
    WITH users.name AS user_name,
    users.id AS _2
  FROM users
  let q =
    (assert false
      : unit -> (_2:int -> 'acc -> 'acc, 'acc) Sqlpp_db.query)

fieldset work well if passed a table alias:
  $ p '
  > select with ...users_fields(u), user_id from users as u
  > ' -print
  SELECT
    WITH u.id AS user_id,
    WITH u.name AS user_name,
    u.id AS _2
  FROM users AS u
  let q =
    (assert false
      : unit -> (_2:int -> 'acc -> 'acc, 'acc) Sqlpp_db.query)

you can pass nested scopes as well:
  $ p '
  > select with ...users_fields(x.y.users), user_id
  > from (select ... from (select ... from users) as y) as x
  > ' -print
  SELECT
    WITH x._0 AS user_id,
    WITH x._1 AS user_name,
    x._0 AS _2
  FROM (
    SELECT
      y._0 AS _0,
      WITH y._1 AS _1
    FROM (
      SELECT
        users.id AS _0,
        WITH users.name AS _1
      FROM users) AS y) AS x
  let q =
    (assert false
      : unit -> (_2:int -> 'acc -> 'acc, 'acc) Sqlpp_db.query)

if fieldset invocation has no enough arguments, it should raise an error:
  $ p '
  > select ...users_fields() from users
  > ' -print
  let q =
    [%ocaml.error
      "fieldset ...users_fields expects 1 arguments but 0 \
       provided"]

  $ alias sqlpp-analyze='./test.exe sqlpp-analyze --require ./schema.sql'

  $ sqlpp-analyze \
  > --require='create fieldset f as select 1 as x;' \
  > 'select with ...f, x'
  SELECT WITH 1 AS x, 1 AS _1

  $ sqlpp-analyze \
  > --require='create fieldset f as select x as x;' \
  > 'select ...f, x'
  ERROR: File "_none_", line 1, characters 28-29
  │ create fieldset f as select x as x;
  │                             ⮬ no such column: x
  
  [1]

fieldsets can accept arguments:
  $ sqlpp-analyze \
  > --require='create fieldset f(from users as u) as select u.id as x' \
  > 'select with ...f(users), x from users'
  SELECT
    WITH users.id AS x,
    users.id AS _1
  FROM users

it's an error to pass not enough arguments to a fieldset:
  $ sqlpp-analyze \
  > --require='create fieldset f(from users as u) as select u.id as x' \
  > 'select ...f(), x from users'
  ERROR: File "_none_", line 1, characters 7-13
  │ select ...f(), x from users
  │        ⮬ fieldset ...f expects 1 arguments but 0 provided
  
  [1]

it's an error to pass too many arguments to a fieldset:
  $ sqlpp-analyze \
  > --require='create fieldset f(from users as u) as select u.id as x' \
  > 'select ...f(users, users), x from users'
  ERROR: File "_none_", line 1, characters 7-25
  │ select ...f(users, users), x from users
  │        ⮬ fieldset ...f expects 1 arguments but 2 provided
  
  [1]

it's an error to pass argument of an invalid type to a fieldset:
  $ sqlpp-analyze \
  > --require='create fieldset f(from users as u) as select u.id as x' \
  > 'select ...f(profiles), x from profiles'
  ERROR: File "_none_", line 1, characters 12-20
  │ select ...f(profiles), x from profiles
  │             ⮬ no such column: id
  
  [1]
