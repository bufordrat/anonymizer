(lang dune 3.13)
(name anonymizer)
(generate_opam_files true)
(package
 (name anonymizer)
 (description "Insert project description here.")
 (synopsis "Insert project synopsis, which is supposedly different, here.")
 (maintainers "Your Email <your.email@gmail.com>" "Your Buddy's Email <your.buddy@gmail.com>")
 (authors "Your Email <your.email@gmail.com>" "Your Buddy's Email <your.buddy@gmail.com>")
 (homepage https://your.website.here)
 (bug_reports https://your.documentation.here)
 (depends
  (ocaml (>= 4.14.0))
   dune
   camlp-streams
   prelude
   etude
   utop
   ocp-index
   cryptokit
   merlin)
 (source (github bufordrat/anonymizer)))

; Local Variables:
; mode: dune
; End:
