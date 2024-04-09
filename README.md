# sqlpp

**WARNING: EXPERIMENTAL, DO NOT USE**

an extension to sql with typed embedding into ocaml 

## motivation

embedding sql into ocaml is notoriously hard and results in very complex and
unergonomic apis, especially if some form of query composition and reuse is
present.

sqlpp takes another approach. it extends sql, the language, to recover
composability, so the typed embedding into ocaml results in a simpler api.

it is worth stating that, sqlpp is just a conservative and backward compatible
extension to sql. it's not a new language.

## project structure

project structure:
- `sqlpp` implements sqlpp language parser, analyzer and generic SQL printer
- `sqlpp_ppx` implements typed embedding into ocaml as ppx
- `sqlpp_sqlite` Sqlite dialect/driver
- `sqlpp_mariadb` MariaDB dialect/driver, uses lwt for I/O
- `sqlpp_postgresql` PostgreSQL dialect/driver, uses lwt for I/O
- `sqlpp_manage` database management (sqlite only for now)

example project structure:
- `test/sqlpp_sqlite/db.ml` defines the database schema, also acts as a ppx
- `test/sqlpp_sqlite/main.ml` the application itself, also database management interface

## features

### query parameters

query can have parameters:

```sql
select id, name
from users
where id = ?id
```

### variant query parameters

the `match ?PARAM with` construct can match on variant query parameters and
drive SQL query generation:

```sql
select id, name
from users
where 
  match ?where with
  | by_id ?id -> id = ?id
  | by_name ?name -> name = ?name
  end
```

### nested scopes

the `select ...` syntax allows to build new expressions within a query:

```sql
create query users_with_invited_users as
select ...
from users
join (select parent_id, ... from users group by parent_id) as invited_users
on users.id = invited_users.parent_id
```

the usage looks like this:

```sql
select
  users.id,
  invited_users.count(1) as num_invited,
  invited_users.argMax(id, created_at) as last_invited
from users_with_invited_users
```

### named expression bindings

the `with EXPR as NAME` synax predefines an expression for further usage:

```sql
select
  with deleted_at is not null and not is_disabled as is_active,
  is_active
from users
```

such expressions won't be queried unless they are used

### named scope bindings

consider query like this:

```sql
select ...
from (select ... from users) as u
:- (from (users: users) as u)
```

the `:- SCOPE` at the bottom shows the scope query has, now with the following
`withscope SCOPE as NAME` syntax one can alias scopes as well:

```sql
select 
  withscope u.users as users
from (select ... from users) as u
:- (from users)
```

and another, more elaborate, example:

```sql
select 
  withscope q.users as users,
  withscope q.profiles as profiles
from (
  select
    withscope u.users as users
  from (select ... from users) as u
  join profiles
) q
:- (from users, from profiles)
```

### reusable fieldsets

the `create fieldset` declaration defines a reusable fieldset:

```sql
create fieldset users(from users as u) as
select
  u.id as user_id,
  u.name as user_name,
  u.created_at as user_created_at
```

which then could be used as:

```sql
select 
  with ...users(users)
from users
:- (user_id int, user_name string user_created_at int)
```

now consider a more elaborate example:

```sql
create fieldset profiles(from profiles as p) as
select
  p.email as profile_email

create fieldset users_agg(from users as u) as
select
  u.count(1) as count,
  u.max(created_at) as last_created

select 
  with ...users(q.users), 
  with ...profiles(q.profiles),
  with q.invited_users as invited_users,
from (
  select with u.useres as users
  from (from users) u
  join profiles
  on u.users.id = profiles.user_id
  join (
    select parent_id, ...users_agg(users)
    from users
    group by parent_id
  ) as invited_users
  on u.users.id = invited_users.parent_id
) as q
:- (
     user_id int,
     user_name string,
     user_created_at int,
     profile_email string,
     from (count int, last_created int) as invited_users
   )
```

we used `with ...FIELDSET(..)` syntax so far, but it's also possible to drop
the `with` and make fieldset actually select the fields. this is useful in case
you have several queries selecting similar data.

### (todo) optional joins

## ocaml ppx

ocaml embedding is implemented via ppx, there are following forms available

### `[%exec QUERY]` execute a query

define a query which doesn't fetch any data:

```ocaml
let create_todo = [%exec "INSERT INTO todos(text) VALUES(?text)"]
let () = create_todo db ~text:"ship it"
```

### `[%fetch_list QUERY]` fetch a list of tuples

define a query to fetch a list of tuples:

```ocaml
let all_todos = [%exec "SELECT id, text, completed FROM todos"]
List.iter (all_todos db) ~f:(fun (id, text, completed) ->
    print_endline (Printf.sprintf "id=%i text=%s completed=%b" id text completed)
```

### `[%fetch_list QUERY ~record:row]` fetch a list of records

define a query to fetch a list of records:

```ocaml
type todo = {id: int; text: string; completed: bool}
let all_todos = [%exec "SELECT id, text, completed FROM todos" ~record:todo]
List.iter (all_todos db) ~f:(fun t ->
    print_endline (Printf.sprintf "id=%i text=%s completed=%b" t.id t.text t.completed)
```

### `[%fetch_option QUERY]` maybe fetch a tuple value

define a query to fetch an optional tuple value:

```ocaml
let last = [%exec "SELECT id, text, completed FROM todos
                   ORDER BY created DESC
                   LIMIT 1"]
let () =
  match last with
  | None -> print_endline "no todos"
  | Some (id, text, completed) -> 
    print_endline (Printf.sprintf "id=%i text=%s completed=%b" id text completed)
```

### `[%fetch_option QUERY ~record:row]` maybe fetch a record value

define a query to fetch an optional record value:

```ocaml
type todo = {id: int; text: string; completed: bool}
let last = [%exec "SELECT id, text, completed FROM todos
                   ORDER BY created DESC
                   LIMIT 1" ~record:todo]
let () =
  match last with
  | None -> print_endline "no todos"
  | Some t -> 
    print_endline (Printf.sprintf "id=%i text=%s completed=%b" t.id t.text t.completed)
```

## bugs

- when instantiating a query, scopes within the `Expr_in` are not being copied
  fresh

## references

- [HTSQL](https://www.htsql.org)
- [FunSQL.jl](https://github.com/MechanicalRabbit/FunSQL.jl)
- [sqlgg](https://github.com/ygrek/sqlgg)
