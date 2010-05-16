; import.arc


; An sobj is an obj where each value is wrapped in a singleton list.
(def import-sobj (sobj)
  (unless (isa sobj 'table)
    (err "A non-table was passed to import-sobj."))
  (each (name svalue) sobj
    (unless (isa name 'sym)
      (err:+ "A table with a non-symbol key was passed to "
             "import-sobj."))
    (unless (acons&single svalue)
      (err:+ "A table with a non-singleton member was passed to "
             "import-sobj.")))
  (w/table overwritten
    (each (name (value)) sobj
      (= .name.overwritten (list global.name)
         global.name value))))

; An nmap is a table mapping friendly global names to obscure global
; names. It's the kind of table 'nspace takes.
;
; Importing an nmap with import-nmap still returns an sobj, not an
; nmap, to refer to the overwritten values. There's no reason to lock
; values to somewhere in the global namespace (where they can't be
; garbage-collected) when there's an easy way to avoid that.
;
(def import-nmap (nmap)
  (unless (isa nmap 'table)
    (err "A non-table was given to import-nmap."))
  (each (name target) nmap
    (unless (and anormalsym.name anormalsym.target)
      (err "A nil, ssyntax, or non-symbol name was in an nmap.")))
  (w/table overwritten
    (each (name target) nmap
      (= .name.overwritten (list global.name)
         global.name global.target))))