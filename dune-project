(lang dune 3.17)

(name parse)

(generate_opam_files true)

(source
 (github PADeguilhen/parse))

(authors "Paul-Alexandre DEGUILHEN <padeguilhen@live.fr>")

(package
 (name parse)
 (synopsis "parse is a monadic parser combinator library with an extensive example for arithmetic expressions")
 (description "parse is a monadic parser combinator library with an extensive example for arithmetic expressions based on the Angstrom parser library: https://github.com/inhabitedtype/angstrom.")
 (depends ocaml))
