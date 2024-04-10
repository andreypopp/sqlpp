
  $ alias p='./test_ppx'

  $ p 'select exists(select true from users where id = 1)' -print
  SELECT
    EXISTS(SELECT
      TRUE AS _0
    FROM users WHERE users.id = 1)
      AS _0
  
  let q =
    (assert false
      : unit -> (_0:bool -> 'acc -> 'acc, 'acc) Sqlpp_db.query)
