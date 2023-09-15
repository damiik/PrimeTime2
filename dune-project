(lang dune 3.10)

(name PrimeTime2)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name PrimeTime2)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune base str digestif dream tyxml)
 (allow_empty) 
 (tags
  (topics "to describe" your project))
)
(package
 (name cool_lib)
 (depends ocaml dune base str dream tyxml)
 (version 1.0)
)
(package
 (name layouts)
 (depends ocaml dune base str dream tyxml)
 (version 1.0)
)
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
