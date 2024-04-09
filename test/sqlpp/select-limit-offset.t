
  $ export DATABASE="sqlite::memory:"
  $ alias s='./test.exe sqlpp-sql --require ./schema.sql'

  $ s 'select id from users limit 1'
  SELECT "users"."id" AS "id" FROM "users" LIMIT 1

  $ s 'select id from users limit 1 offset 1'
  SELECT "users"."id" AS "id" FROM "users" LIMIT 1 OFFSET 1

TODO: need to disable perhaps?
  $ s 'select id from users offset 1'
  SELECT "users"."id" AS "id" FROM "users" OFFSET 1
