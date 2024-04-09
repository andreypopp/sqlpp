setup and run the test database:

  $ mkdir ./db
  $ mysql_install_db \
  > --datadir=$PWD/db \
  > --skip-networking \
  > --user root \
  > --skip-test-db \
  > >/dev/null 2>&1 
  $ mysqld \
  > --console \
  > --skip-networking \
  > --socket=/tmp/mariadb.socket \
  > --pid-file=/tmp/mariadb.pid \
  > --datadir=$PWD/db \
  > >/dev/null 2>&1 &

stop the database when the script exits:

  $ DB_PID="$!"
  $ on_done() { kill $DB_PID; }
  $ trap on_done EXIT

wait till the database is ready:

  $ sleep 1

create the schema:

  $ cat schema.sql | mysql --socket=/tmp/mariadb.socket -B

finally, run the test suite:

  $ ./main.exe "mariadb:/tmp/mariadb.socket?db=sw"
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
