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
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
