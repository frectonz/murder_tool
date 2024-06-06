(lang dune 3.10)

(name murder_tool)

(generate_opam_files true)

(source
 (github frectonz/murder_tool))

(authors "frectonz")

(maintainers "frectonz")

(license MIT)

(package
 (name murder_tool)
 (synopsis "Kill processes with a TUI")
 (depends ocaml dune feather))
