(lang dune 3.17)

(name dbf)

(generate_opam_files true)

(version 0.1.3)

(source
 (github hothing/libdbf))

(authors "Mykhayl Puzanov <pum@example.com>")

(maintainers
 "Mykhayl Puzanov <pum@example.com>"
 "John Doe <john.doe@example.com>")

(license BSD-2-Clause)

(documentation https://mysite.me/libdbf/documentation)

(package
 (name dbf)
 (synopsis "A DBF reader library")
 (description "A DBF reader library with MEMO support and seqental record access")
 (depends ocaml)
 (tags
  ("xBASE" "DBF" "data access")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
