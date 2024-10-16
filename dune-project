(lang dune 3.14)

(name checked_oint)

(version 0.2.1)

(generate_opam_files true)

(source
 (github hirrolot/checked_oint))

(authors "Lewis Pearson <hirrolot@gmail.com>")

(maintainers "Lewis Pearson <hirrolot@gmail.com>")

(license MIT)

(package
 (name checked_oint)
 (synopsis "An OCaml library for checked integer arithmetic")
 (depends
  (ocaml
   (>= 4.13))
  dune
  ctypes
  (ctypes-foreign
   (>= 0.21.1))
  ppx_deriving
  ppx_enumerate
  bisect_ppx
  (alcotest
   (>= 1.7.0))))
