.PHONY: init
init:
	opam switch create . 4.14.2 -y --deps-only
	opam install -y dune sedlex menhir ppxlib pprint containers ocaml-lsp-server ocamlformat sqlite3 mariadb ppx_blob ppx_hash

.PHONY: build fmt clean watch
build fmt clean watch:
	dune $@

.PHONY: test test_core test_mariadb test_sqlite
test:
	dune build @runtest @test_sqlpp_mariadb @test_sqlpp_sqlite
test_core:
	dune runtest
test_mariadb:
	dune build @test_sqlpp_mariadb
test_sqlite:
	dune build @test_sqlpp_sqlite

.PHONY: b
b: build
