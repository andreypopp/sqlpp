
  $ alias p='./test_ppx'

using aggregate functions in a non-agg context is forbidden
  $ p 'select count(1)'
  let q =
    [%ocaml.error
      "aggregate function `count(..)` is not allowed without \
       GROUP BY"]

  $ p 'select count(1) from users'
  let q =
    [%ocaml.error
      "aggregate function `count(..)` is not allowed without \
       GROUP BY"]

for now we require explicit `group by` clause (can be lifted in the future)
  $ p 'select count(1) from users group by ()'
  let q =
    (assert false
      : unit -> (_0:int -> 'acc -> 'acc, 'acc) Sqlpp_db.query)

nested aggregate functions are forbidden
  $ p 'select count(count(1)) from users group by ()'
  let q =
    [%ocaml.error
      "aggregate function `count(..)` is not allowed without \
       GROUP BY"]

using columns which are not part of `group by` is forbidden
  $ p 'select count(1), id from users group by ()'
  let q =
    [%ocaml.error
      "expression `users.id` is not in GROUP BY clause and \
       is not under aggregate function"]

but if we group by columns, we can use them directly
  $ p 'select count(1), id from users group by id'
  let q =
    (assert false
      : unit ->
        ( _0:int -> id:int -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)

and even form more complex expressions with them
  $ p 'select count(1), id + id from users group by id'
  let q =
    (assert false
      : unit ->
        ( _0:int -> _1:int -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)

again, `created_at` is not part of `group by`
  $ p 'select count(1), id, created_at from users group by id'
  let q =
    [%ocaml.error
      "expression `users.created_at` is not in GROUP BY \
       clause and is not under aggregate function"]

but ok if we use it through aggregate function
  $ p 'select count(1), id, max(created_at) from users group by id'
  let q =
    (assert false
      : unit ->
        ( _0:int -> id:int -> _2:float -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)

a test case with more complex expressions used in `group by`
  $ p 'select count(1), id + id from users group by id + id'
  let q =
    (assert false
      : unit ->
        ( _0:int -> _1:int -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)

  $ p 'select count(1), 1 + (id + id) from users group by id + id'
  let q =
    (assert false
      : unit ->
        ( _0:int -> _1:int -> 'acc -> 'acc,
          'acc )
        Sqlpp_db.query)
