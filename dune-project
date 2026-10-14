(lang dune 3.18)
(name rowtype)

(implicit_transitive_deps false)

(generate_opam_files true)

(maintainers "Hypha Technologies Ltd <dev@hyphatech.io>")

(authors "Hypha Technologies Ltd")

(license MIT)

(maintenance_intent "(latest)")

(source
 (github hyphatech/rowtype))

; postgres-eio is in no opam repository yet: dune takes it from its
; repository at the release's tag, and opam from the pin-depends of
; rowtype-postgres.opam.template, at the same tag.

(pin
 (url "git+https://github.com/hyphatech/postgres-eio.git#0.5.0")
 (package
  (name postgres-eio)
  (version 0.5.0)))

(package
 (name rowtype)
 (synopsis "One value describes a query's parameters and rows, for any SQL database")
 (description
  "A query's parameters and rows described once, binding and decoding both, and run by a backend. Reads no SQL and links no database's library.")
 (depends
  (ocaml
   (>= 5.5))
  (ptime
   (>= 1.2.0))
  (uuidm
   (>= 0.9.10))
  (alcotest
   (and
    :with-test
    (>= 1.9.1)))
  (qcheck-core
   (and
    :with-test
    (>= 0.91)))
  (qcheck-alcotest
   (and
    :with-test
    (>= 0.91)))))

(package
 (name rowtype-postgres)
 (synopsis "The Postgres backend for rowtype, over postgres-eio")
 (description
  "Connections and a pool, notifications, transactions whose own failures are values, a load through one COPY, and a statement checked against the database without running it.")
 (depends
  (ocaml
   (>= 5.5))
  (rowtype
   (= :version))
  (postgres-eio
   (>= 0.5.0))
  (ptime
   (>= 1.2.0))
  (uuidm
   (>= 0.9.10))
  (eio
   (>= 1.5))
  (logs
   (>= 0.10.0))
  (mtime
   (>= 2.1.0))
  (eio_main
   (and
    :with-test
    (>= 1.5)))
  (alcotest
   (and
    :with-test
    (>= 1.9.1)))
  (qcheck-core
   (and
    :with-test
    (>= 0.91)))
  (cstruct
   (and
    :with-test
    (>= 6.3.0)))))

(package
 (name rowtype-migrate)
 (synopsis
  "Migrations for Postgres: plain SQL files, forward only, and the command that applies them")
 (description
  "A directory of timestamped SQL files, applied in order under an advisory lock, each in one transaction with the row that records it and a digest of its text, or on its own where Postgres refuses one; a schema dump that changes only when the schema does; and a history squashed into one baseline.")
 (depends
  (ocaml
   (>= 5.5))
  (rowtype
   (= :version))
  (rowtype-postgres
   (= :version))
  (ptime
   (>= 1.2.0))
  (cmdliner
   (>= 2.1.1))
  (eio
   (>= 1.5))
  (mtime
   (>= 2.1.0))
  (eio_main
   (>= 1.5))
  (alcotest
   (and
    :with-test
    (>= 1.9.1)))))
