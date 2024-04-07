setup and run the test database:

  $ mkdir ./db
  $ mariadb-install-db \
  > --datadir=./db \
  > --skip-networking \
  > --user root \
  > --skip-test-db \
  > >/dev/null
  $ mariadbd \
  > --console \
  > --skip-networking \
  > --socket=/tmp/mariadb.socket \
  > --datadir=$PWD/db \
  > 2>/dev/null &

stop the database when the script exits:

  $ DB_PID="$!"
  $ function on_done() { kill -9 $DB_PID; }
  $ trap on_done EXIT

wait till the database is ready:

  $ sleep 1

create the schema:

  $ mysql --socket=/tmp/mariadb.socket < schema.sql

finally, run the test suite:

  $ ./main.exe
  connecting to mariadb...
  inserting planets...
  listing planets...
  planet_id=0, name=Tatooine, climate=arid, population=200000
  planet_id=1, name=Alderaan, climate=temperate, population=2000000000
  planet_id=2, name=Yavin IV, climate=temperate, tropical, population=1000
  planet_id=3, name=Hoth, climate=frozen, population=0
  planet_id=4, name=Dagobah, climate=murky, population=0
  planet_id=5, name=Bespin, climate=temperate, population=6000000
  planet_id=6, name=Endor, climate=forests, mountains, lakes, population=30000000
  planet_id=7, name=Naboo, climate=temperate, population=4500000000
  planet_id=8, name=Coruscant, climate=temperate, population=1000000000000
  planet_id=9, name=Kamino, climate=-, population=1000000000
  done
