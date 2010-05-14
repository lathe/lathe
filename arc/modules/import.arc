; import.arc

(once-tl "load import.arc"


; This is an obj where each value is wrapped in a singleton list.
(mac sobj bindings
  `(obj ,@(mappend [do `(,_.0 (list ,_.1))] pair.bindings)))

; Use this inside an nspaced form in order to save off the values of
; the given names into a table of singleton lists, using the names
; themselves as keys. This table can then be sent to import-sobj in
; order to conveniently use the values in some other context.
(mac mine-as-sobj names
  `(sobj ,@(mappend [do `(,_ (my ,_))] names)))


(def import-obj (obj)
  (unless (isa obj 'table)
    (err "A non-table was passed to import-obj."))
  (each (name value) obj
    (unless (isa name 'sym)
      (err:+ "A table with a non-symbol key was passed to "
             "import-obj.")))
  (w/table overwritten
    (each (name value) obj
      (= .name.overwritten (list global.name)
         global.name value))))

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

; You can use these obj-to-mine and sobj-to-mine inside an nspaced
; form to avoid polluting friendly names in the global environment.
; This will assign each value foo to my.foo (which will macroexpand to
; whatever gensym the nspaced form identified with that local name.

(def obj-to-mine (obj)
  (unless (isa obj 'table)
    (err "A non-table was passed to obj-to-mine."))
  (each (name value) obj
    (unless (isa name 'sym)
      (err:+ "A table with a non-symbol key was passed to "
             "obj-to-mine.")))
  (w/table overwritten
    (each (name value) obj
      (= .name.overwritten (list:global my.name)
         (global my.name) value))))

(def sobj-to-mine (sobj)
  (unless (isa sobj 'table)
    (err "A non-table was passed to sobj-to-mine."))
  (each (name svalue) sobj
    (unless (isa name 'sym)
      (err:+ "A table with a non-symbol key was passed to "
             "sobj-to-mine."))
    (unless (acons&single svalue)
      (err:+ "A table with a non-singleton member was passed to "
             "sobj-to-mine.")))
  (w/table overwritten
    (each (name (value)) sobj
      (= .name.overwritten (list:global my.name)
         (global my.name) value))))


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


)