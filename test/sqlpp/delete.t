
  $ alias p='./test.exe sqlpp-analyze --require ./schema.sql'
  $ alias s='./test.exe sqlpp-sql --require ./schema.sql'

  $ p 'delete from users'
  DELETE FROM users

  $ p 'delete from users where id = 1'
  DELETE FROM users WHERE users.id = 1

  $ p 'delete from users where x = 1'
  ERROR: File "_none_", line 1, characters 24-25
  │ delete from users where x = 1
  │                         ⮬ no such column: x
  
  [1]

  $ p 'delete from x'
  ERROR: File "_none_", line 1, characters 12-13
  │ delete from x
  │             ⮬ no such table: x
  
  [1]

`RETURNING` clause is supported.

  $ s 'delete from users returning id'
  DELETE FROM "users" RETURNING "users"."id" AS "id"
