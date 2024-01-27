  $ export DATABASE=./test.db
  $ alias sanitize='sed -E "s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]/DATETIME/g"'

  $ ./main.exe migrate --verbose
  -- MIGRATE: create todos
  BEGIN;
  CREATE TABLE todos("id" INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, "text" TEXT NULL, "created" REAL NOT NULL, "done" INTEGER NOT NULL) STRICT;
  COMMIT;
  -- MIGRATE: todos: done -> completed
  BEGIN;
  ALTER TABLE "todos" RENAME COLUMN "done" TO "completed";
  COMMIT;
  -- MIGRATE: todos: text null -> text not null
  BEGIN;
  ALTER TABLE "todos" RENAME COLUMN "text" TO "text_prev";
  ALTER TABLE "todos" ADD COLUMN "text" TEXT NOT NULL;
  UPDATE "todos" SET "text" = coalesce("todos"."text_prev", '');
  ALTER TABLE "todos" DROP COLUMN "text_prev";
  COMMIT;
  -- MIGRATE: todos: created float -> created text
  BEGIN;
  ALTER TABLE "todos" RENAME COLUMN "created" TO "created_prev";
  ALTER TABLE "todos" ADD COLUMN "created" TEXT NOT NULL;
  UPDATE "todos" SET "created" = cast(CURRENT_TIMESTAMP as text);
  ALTER TABLE "todos" DROP COLUMN "created_prev";
  COMMIT;
  -- MIGRATE: create projects
  BEGIN;
  CREATE TABLE projects("name" TEXT NOT NULL PRIMARY KEY, "created" TEXT NOT NULL) STRICT;
  INSERT INTO "projects" ("name", "created") VALUES ('default', cast(CURRENT_TIMESTAMP as text));
  ALTER TABLE "todos" ADD COLUMN "project" TEXT NOT NULL DEFAULT ('default');
  COMMIT;
  -- MIGRATE: todos/projects: created text -> created datetime
  BEGIN;
  ALTER TABLE "todos" RENAME COLUMN "created" TO "created_prev";
  ALTER TABLE "todos" ADD COLUMN "created" TEXT NOT NULL DEFAULT ('1970-01-01T00:00:00');
  UPDATE "todos" SET "created" = coalesce(datetime("todos"."created_prev"), CURRENT_TIMESTAMP);
  ALTER TABLE "todos" DROP COLUMN "created_prev";
  ALTER TABLE "projects" RENAME COLUMN "created" TO "created_prev";
  ALTER TABLE "projects" ADD COLUMN "created" TEXT NOT NULL DEFAULT ('1970-01-01T00:00:00');
  UPDATE "projects" SET "created" = coalesce(datetime("projects"."created_prev"), CURRENT_TIMESTAMP);
  ALTER TABLE "projects" DROP COLUMN "created_prev";
  COMMIT;

  $ ./main.exe todos-insert "buy milk"
  $ ./main.exe todos-insert "buy bread"
  $ ./main.exe todos-insert "write ocaml" --project job

  $ ./main.exe todos-ls | sanitize
  1: [ ] buy milk (DATETIME) #default
  2: [ ] buy bread (DATETIME) #default
  3: [ ] write ocaml (DATETIME) #job

  $ ./main.exe todos-ls --project job | sanitize
  3: [ ] write ocaml (DATETIME) #job

  $ ./main.exe todos-completed 3

  $ ./main.exe todos-ls --completed | sanitize
  1: [ ] buy milk (DATETIME) #default
  2: [ ] buy bread (DATETIME) #default
  3: [x] write ocaml (DATETIME) #job

  $ ./main.exe todos-project-stats
  default: 2/2 pending/total
  job: 0/1 pending/total
