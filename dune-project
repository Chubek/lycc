(lang dune 3.11)
(name lycc)
(version 0.1.0)

(generate_opam_files true)

(source
 (github Chubek/lycc))

(license MIT)

(authors "Chubak Bidpaa <chubakbidpaa@riseup.net>")

(maintainers "chubakbidpaa@riseup.net")

(package
 (name lycc)
 (synopsis "A modern C compiler written in OCaml")
 (description "LyCC implements ISO C11 with advanced optimizations")
 (depends
  ocaml
  dune
  (angstrom (>= 0.15.0))
  (ocamlgraph (>= 2.0.0))
  (core (>= v0.16.0))
  (alcotest :with-test)
  (qcheck :with-test)
  (ppx_inline_test :with-test)
  (ppx_expect :with-test)
  (bisect_ppx :with-dev)))

(using menhir 2.1)

