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
 "Portable bounded structured logging with typed and open point logs, wide events, explicit redaction, sampling, routing, pure formatters, drains, and deterministic capture.")
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
  "Lwt execution and dynamic context for Observe, completed with caller-provided clocks, occurrence identity, sampling decisions, and console capabilities.")
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
  "Ready Observe initialization for Lwt-Unix with bounded console delivery, UUID operation identity, scoped wide events, deterministic capture, flush, and shutdown.")
 (depends
  (ocaml
   (>= 4.14))
  (observe
   (= :version))
  (observe-lwt
   (= :version))
  lwt
  ptime
  mtime
  uuidm
  mirage-crypto-rng
  (ocamlfind :with-test)))

(package
 (name observe-fs)
 (synopsis "Portable daily filesystem delivery for Observe")
 (description
  "Portable bounded daily NDJSON delivery with prompt drain acceptance, finite writer state, barriers, and injectable filesystem capabilities.")
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
  "Lwt execution and lifecycle completion for portable Observe filesystem delivery over caller-provided capabilities.")
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
  "Ready bounded daily NDJSON files for Lwt-Unix with recursive directory setup and automatic Observe lifecycle registration.")
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
