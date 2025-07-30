(lang dune 3.17)

(name parse)

(generate_opam_files true)

(source
 (github PADeguilhen/parse))

(authors "Paul-Alexandre DEGUILHEN <padeguilhen@live.fr>")

(maintainers "Paul-Alexandre DEGUILHEN <padeguilhen@live.fr>")

(license "MIT")

(package
 (name parse)
 (synopsis "Parse is a monadic parser combinator library with an extensive example for arithmetic expressions")
 (description "Parse is a monadic parser combinator library with an extensive example for arithmetic expressions based on the Angstrom parser library: https://github.com/inhabitedtype/angstrom.")
 (depends ocaml alcotest))
