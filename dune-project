(lang dune 3.22)

(name observe)

(generate_opam_files true)

(implicit_transitive_deps false)

(source
 (github abdufelsayed/observe))

(documentation "https://abdufelsayed.github.io/observe/")

(authors "Abdullah Elsayed <hey@abdufelsayed.dev>")

(maintainers "Abdullah Elsayed <hey@abdufelsayed.dev>")

(license MIT)

(package
 (name observe)
 (synopsis "Structured logging and observability for OCaml")
 (description
 "Structured logging with a portable core, typed values, deterministic capture, and a ready Lwt-Unix composition.")
 (depends
  (ocaml
   (>= 4.14))
  (repr
   (>= 0.7.0))
  (jsonm
   (>= 1.0.2))
  lwt
  ptime
  ppxlib
  (ppx_repr
   (>= 0.7.0))
  (dune-release :with-dev-setup)
  (opam-publish :with-dev-setup)
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
    (>= 0.91)))
  (ocamlfind :with-test)
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.29.0)))))
