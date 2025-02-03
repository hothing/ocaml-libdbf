(lang dune 3.17)

(name dbf)

(generate_opam_files true)

(version 0.1.3)

(source
 (github hothing/ocaml-libdbf))

(authors "Mykhayl Puzanov <pum@example.com>")

(maintainers
 "Mykhayl Puzanov <pum@example.com>"
 "John Doe <john.doe@example.com>")

(license GPLv3)

(documentation https://github.com/hothing/ocaml-libdbf/doc)

(package
 (name dbf)
 (synopsis "A DBF reader library")
 (description "A DBF reader library with MEMO support and sequental record access")
 (depends ocaml)
 (tags
  ("xBASE" "DBF" "data access")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
