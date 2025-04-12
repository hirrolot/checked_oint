(lang dune 3.14)

(name checked_oint)

(version 0.6.0)

(generate_opam_files true)

(source
 (github hirrolot/checked_oint))

(authors "hirrolot <hirrolot@gmail.com>")

(maintainers "hirrolot <hirrolot@gmail.com>")

(license MIT)

(package
 (name checked_oint)
 (synopsis "An OCaml library for checked integer arithmetic")
 (depends
  (ocaml
   (>= 4.13))
  dune
  ppx_deriving
  ppx_enumerate
  bisect_ppx
  (alcotest
   (>= 1.7.0))))
