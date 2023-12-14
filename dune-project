(lang dune 3.10)
(generate_opam_files true)
  (authors "Hongyi Liu" "Christopher Li")
  (maintainers "liuhongyi@jhu.edu" "cli135@jhu.edu")
  (package
   (name moebius) ; put in your own project name here
   (synopsis "Moebius Transformation in ASCII Art")
   (description "Rendering the Moebius Transformation in the terminal")
   (depends
    (ounit2 (>= 2.2.7)) ; for each opam dependency list the version
    (core (>= 0.16.2)) ; `opam list core` will display which version of core you have
    (imagelib (>= 20221222)) ; `opam list core` will display which version of core you have
    (yojson (>= 2.1.1)) ; `opam list core` will display which version of core you have
 ))
