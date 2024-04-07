create table users (
  id int not null,
  name string,
  info string not null,
  created_at float not null
);

create table profiles (
  user_id int not null,
  settings string not null,
  info string not null
);

create fieldset users_fields(from users) as
select
  users.id as user_id,
  users.name as user_name
;

create fieldset users_agg_fields(from users) as
select
  count(1) as count,
  max(created_at) as last_created_at
;

create fieldset users_agg_fields2(from (from users) as agg) as
select
  agg.count(1) as count,
  agg.max(created_at) as last_created_at
;

create query user_stats as
select 
  id as user_id, 
  with count(1) as count,
  ... 
from users
group by id
;
