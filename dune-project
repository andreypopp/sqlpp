(lang dune 3.14)
(using menhir 3.0)
(name sqlpp)
(generate_opam_files true)
(source (github andreypopp/sqlpp))
(authors "Andrey Popp")
(maintainers "Andrey Popp")
(license MIT)

(package
 (name sqlpp)
 (synopsis "typesafe SQL for OCaml via preprocessing")
 (depends (ocaml (>= 4.14))
          dune yojson 
          menhir 
          menhirLib 
          pprint 
          re 
          containers 
          sedlex
          ppx_compare
          ppx_hash
          ppxlib
          cmdliner
          (ocamlformat :with-test)
          (ppx_blob :with-test)))

(package
 (name sqlpp_sqlite)
 (synopsis "sqlite backend for sqlpp")
 (depends (ocaml (>= 4.14)) dune sqlpp pprint sqlite3))
