(lang dune 3.14)

(name schooloprog)

(generate_opam_files true)

(source
 (github monomon/reponame))

(authors "Mois Moshev <mois@monomon.me>")

(maintainers "Mois Moshev <mois@monomon.me>")

(license "ICS")

(documentation https://url/to/documentation)

(package
 (name schooloprog)
 (synopsis "Sexp to iCal")
 (description "Generate an icalendar from a provided sexp description. Used for school classes.")
 (depends ocaml dune icalendar sexplib ptime)
 (tags
  (school schedule generate sexp icalendar ical)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
