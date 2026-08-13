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
 (synopsis "Structured logging for OCaml")
 (description
 "Portable structured logging with typed values and deterministic capture.")
 (depends
  (ocaml
   (>= 4.14))
  (repr
   (>= 0.8.0))
  (jsonm
   (>= 1.0.2))
  (ppxlib
   (>= 0.38.0))
  (ppx_repr
   (>= 0.8.0))
  (dune-release :with-dev-setup)
  (opam-publish :with-dev-setup)
  (alcotest
   (and
    :with-test
    (>= 1.9.1)))
  (fmt :with-test)
  (qcheck-core
   (and
    :with-test
    (>= 0.91)))
  (qcheck-alcotest
   (and
    :with-test
    (>= 0.91)))
  (bechamel
   (and
    :with-dev-setup
    (>= 0.5.0)))
  (ocamlfind :with-test)
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.29.0)))))

(package
 (name observe-lwt)
 (synopsis "Configurable Lwt I/O for Observe")
 (description
  "Lwt effects and dynamic context completed with caller-provided Observe clock and console functions.")
 (depends
  (ocaml
   (>= 4.14))
  (observe
   (= :version))
  lwt
  (alcotest
   (and
    :with-test
    (>= 1.9.1)))))

(package
 (name observe-lwt-unix)
 (synopsis "Ready Lwt-Unix composition for Observe")
 (description
  "Ready Observe initialization and scoped capture for Lwt applications on Unix.")
 (depends
  (ocaml
   (>= 4.14))
  (observe
   (= :version))
  (observe-lwt
   (= :version))
  lwt
  ptime
  (ocamlfind :with-test)))

(package
 (name observe-fs)
 (synopsis "Portable daily filesystem delivery for Observe")
 (description
  "Portable bounded daily NDJSON filesystem delivery with injectable I/O capabilities.")
 (depends
  (ocaml
   (>= 4.14))
  (observe
   (= :version))
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
  (observe-lwt
   (and
    :with-test
    (= :version)))
  (lwt :with-test)
  (ptime :with-test)))

(package
 (name observe-fs-lwt)
 (synopsis "Lwt execution for Observe filesystem delivery")
 (description
  "Lwt completion of portable Observe filesystem delivery for injected filesystems.")
 (depends
  (ocaml
   (>= 4.14))
  (observe-fs
   (= :version))
  (observe
   (= :version))
  (observe-lwt
   (= :version))
  lwt
  (alcotest
   (and
    :with-test
    (>= 1.9.1)))))

(package
 (name observe-fs-lwt-unix)
 (synopsis "Ready Lwt-Unix daily filesystem delivery for Observe")
 (description
  "Ready bounded daily NDJSON files for Observe applications using Lwt-Unix.")
 (depends
  (ocaml
   (>= 4.14))
  (observe
   (= :version))
  (observe-fs-lwt
   (= :version))
  (observe-lwt-unix
   (= :version))
  lwt
  (jsonm :with-test)
  (ocamlfind :with-test)))
