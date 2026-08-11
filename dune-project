(lang dune 3.22)

(name observe)

(generate_opam_files true)

(implicit_transitive_deps false)

(authors "Abdullah Elsayed <hey@abdufelsayed.dev>")

(maintainers "Abdullah Elsayed <hey@abdufelsayed.dev>")

(package
 (name observe)
 (synopsis "Structured logging and observability for OCaml")
 (description
 "A portable structured logging foundation with typed values, filtering, formatting, drains, diagnostics, and deterministic capture.")
 (depends
  (ocaml
   (>= 4.14))
  (repr
   (>= 0.7.0))
  ppxlib
  (ppx_repr
   (>= 0.7.0))
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
  (yojson :with-test)
  (ocamlfind :with-test)
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.29.0)))))
