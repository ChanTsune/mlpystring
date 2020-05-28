(lang dune 2.5)
(version 0.0.0)
(generate_opam_files true)

(source (github ChanTsune/mlpystring))
(license MIT)
(authors "ChanTsune")
(maintainers "ChanTsune")

(package
 (name mlpystring)
 (synopsis "An OCaml library for Python like string operation")
 (description "")
 (depends
  (ounit :with-test)
  (dune (> 1.5))))
