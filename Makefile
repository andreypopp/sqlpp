.PHONY: init
init:
	opam switch create . 4.14.2 -y --deps-only
	opam install -y dune sedlex menhir ppxlib pprint containers ocaml-lsp-server ocamlformat sqlite3 ppx_blob ppx_hash

.PHONY: build test fmt clean watch
build test fmt clean watch:
	dune $@

.PHONY: b
b: build

.PHONY: t
t: test

.PHONY: commit
commit:
	git add -u .
	git commit --amend --no-edit
	git push --force
