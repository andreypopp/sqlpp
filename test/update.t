
  $ alias p='./test.exe sqlpp-analyze --require ./schema.sql'
  $ alias s='./test.exe sqlpp-sql --require ./schema.sql'

  $ p 'update users set id = 42'
  UPDATE users SET id = 42

  $ p 'update users set id = 42 where id = 43'
  UPDATE users SET id = 42 WHERE users.id = 43

  $ p 'update users set id = u.id from users as u where u.id = users.id'
  UPDATE users SET id = u.id
  FROM users AS u WHERE u.id = users.id

  $ p 'update users set id = u.id from (select ... from users) as u where u.id = users.id'
  UPDATE users SET id = u._0
  FROM (SELECT users.id AS _0 FROM users) AS u
  WHERE
    u._0 = users.id

test that params require no annotation:
  $ p 'update users set id = ?id'
  UPDATE users SET id = ?id

  $ p 'update x set id = 42'
  ERROR: File "_none_", line 1, characters 7-8
  │ update x set id = 42
  │        ⮬ no such table: x
  
  [1]

  $ p 'update users set id = 42 where x = 43'
  ERROR: File "_none_", line 1, characters 31-32
  │ update users set id = 42 where x = 43
  │                                ⮬ no such column: x
  
  [1]

  $ p 'update users set id = u.id from x as u where u.id = users.id'
  ERROR: File "_none_", line 1, characters 32-38
  │ update users set id = u.id from x as u where u.id = users.id
  │                                 ⮬ no such table x
  
  [1]

`RETURNING` clause is supported:

  $ s 'update users set id = id + 1 returning id'
  UPDATE "users" SET "id" = ("users"."id" + 1) RETURNING "users"."id" AS "id"

TODO: decide if/how we want to allow/disallow that (referencing tables from
`FROM` in `RETURNING`), e.g. SQLight does not support it but other databases
IIRC do:
  $ s 'update users set id = u.id
  >    from users as u where u.id = users.id
  >    returning u.id as u_id, users.id as users_id'
  UPDATE "users" SET "id" = "u"."id" FROM "users" AS "u" WHERE ("u"."id" = "users"."id") RETURNING "u"."id" AS "u_id", "users"."id" AS "users_id"

TODO: error message is off
  $ p 'update users set id = u.id from (select ... from users group by name) as u where u.id = users.id'
  ERROR: File "_none_", line 1
  │ update users set id = u.id from (select ... from users group by name) as u where u.id = users.id
  │ ⮬ expression `users.id` is not in GROUP BY clause and is not under aggregate function
  
  [1]

