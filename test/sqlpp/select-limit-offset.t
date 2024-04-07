
  $ alias s='./test.exe sqlpp-sql --require ./schema.sql'

  $ s 'select id from users limit 1'

  $ s 'select id from users limit 1 offset 1'

TODO: need to disable perhaps?
  $ s 'select id from users offset 1'
