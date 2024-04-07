
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
  $ DB_PID="$!"
  $ function on_done() { kill -9 $DB_PID; }
  $ trap on_done EXIT
  $ sleep 1

  $ mysql --socket=/tmp/mariadb.socket -e "select 1"
  1
  1
