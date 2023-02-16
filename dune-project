(lang dune 2.9)

(name deep-lense)

(version 0.1.0)

(generate_opam_files true)

(license LGPL-3.0-or-later)

(maintainers "mefyl <mefyl@gruntech.org>")

(authors "mefyl <mefyl@gruntech.org>")

(source
 (github mefyl/ocaml-deep-lense))

(package
 (name deep-lense)
 (synopsis "PPX rewriter for deep functional record lenses")
 (depends
  (alcotest :with-test)
  (ppx_deriving :with-test)
  ppxlib))
