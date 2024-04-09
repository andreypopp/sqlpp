
  $ export DATABASE="sqlite::memory:"
  $ alias p='./test.exe sqlpp-analyze --require ./schema.sql'
  $ alias s='./test.exe sqlpp-sql --require ./schema.sql'

  $ p "insert into profiles(user_id, settings, info)
  >    values (1, 's', 'i')"
  INSERT INTO profiles(user_id, settings, info)
  VALUES (1, 's', 'i')

  $ p "insert into profiles(user_id, settings, info)
  >    values (1, 's1', 'i1'), (2, 's2', 'i2')"
  INSERT INTO profiles(user_id, settings, info)
  VALUES (1, 's1', 'i1'), (2, 's2', 'i2')

  $ p "insert into profiles(user_id, settings, info)
  >    select 1, 's', 'i'"
  INSERT INTO profiles(user_id, settings, info)
  SELECT 1 AS _0, 's' AS _1, 'i' AS _2

  $ p "insert into profiles(user_id, settings, info)
  >    select id, 's', 'i' from users"
  INSERT INTO profiles(user_id, settings, info)
  SELECT
    users.id AS id,
    's' AS _1,
    'i' AS _2
  FROM users

test that params require no annotation:
  $ p "insert into profiles(user_id, settings, info)
  >    values (?id, 's', 'i')"
  INSERT INTO profiles(user_id, settings, info)
  VALUES (?id, 's', 'i')

test alternative syntax with `SET`:
  $ p "insert into profiles set user_id=?id, settings='s', info='i'"
  INSERT INTO profiles(user_id, settings, info)
  VALUES (?id, 's', 'i')

`RETURNING` clause is supported:

  $ s "insert into profiles set user_id=1, settings='s', info='i' returning user_id"
  INSERT INTO "profiles" ("user_id", "settings", "info") VALUES (1, 's', 'i') RETURNING "profiles"."user_id" AS "user_id"

though cannot reference anything else but the columns of the table being inserted into:
  $ s "insert into profiles(user_id, settings, info)
  >    select id, 's', 'i' from users
  >    returning users.id"
  ERROR: File "_none_", line 3, characters 13-21
  │    select id, 's', 'i' from users
  │    returning users.id
  │              ⮬ no such table/query `users` (available profiles)
  
  [1]

TODO: error message is off
  $ p "insert into profiles(user_id, settings, info)
  >    select id, 's', from users"
  ERROR: File "_none_", line 2, characters 3-29
  │ insert into profiles(user_id, settings, info)
  │    select id, 's', from users
  │    ⮬ number of columns (3) doesn't match number of expressions (2)
  
  [1]

TODO: error message is off
  $ p "insert into profiles(user_id, settings, info)
  >    values (1, 's')"
  ERROR: File "_none_", lines 1-2, characters 0-18
  │ insert into profiles(user_id, settings, info)
  │ ⮬ number of columns (3) doesn't match number of expressions (2)
  │    values (1, 's')
  
  [1]

  $ p "insert into profiles(user_id, settings, x)
  >    values (1, 's', 'i')"
  ERROR: File "_none_", line 1, characters 40-41
  │ insert into profiles(user_id, settings, x)
  │                                         ⮬ no such column in table: x
  │    values (1, 's', 'i')
  
  [1]

TODO: error message is off
  $ p "insert into profiles(user_id, settings, info)
  >    select id, name, name from users group by id"
  ERROR: File "_none_", line 1
  │ insert into profiles(user_id, settings, info)
  
  [1]
