; import.arc

(once-tl "load import.arc"


; This is an obj where each value is wrapped in a singleton list.
(mac sobj bindings
  `(obj ,@(mappend (fn ((k v)) `(,k (list ,v))) pair.bindings)))

; Use this inside an nspaced form in order to save off the values of
; the given names into a table of singleton lists, using the names
; themselves as keys. This table can then be sent to import-sobj in
; order to conveniently use the values in some other context.
(mac mine-as-sobj names
  `(sobj ,@(mappend [do `(,_ (my ,_))] names)))


; This is an alternate definition of import-obj which works just as
; well but doesn't rely on 'nspaced.
;
;(w/uniq g-temp
;  (eval
;    `(def import-obj (t)
;       (unless (isa t 'table)
;         (err "A non-table was passed to import-obj."))
;       (each (k v) t
;         (unless (isa k 'sym)
;           (err:+ "A table with a non-symbol key was passed to "
;                  "import-obj.")))
;       (after
;         (w/table overwritten
;           (each (k v) t
;             (= ,g-temp v
;                .k.overwritten (list (bound&eval k)))
;             (eval `(= ,k ,',g-temp))))
;         (wipe ,g-temp)))))

(nspaced:def import-obj (t)
  (unless (isa t 'table)
    (err "A non-table was passed to import-obj."))
  (each (k v) t
    (unless (isa k 'sym)
      (err:+ "A table with a non-symbol key was passed to "
             "import-obj.")))
  (after
    (w/table overwritten
      (each (k v) t
        (= my.temp v
           .k.overwritten (list (bound&eval k)))
        (eval `(= ,k ,my!temp))))
    (wipe my.temp)))

(nspaced:def import-sobj (t)
  (unless (isa t 'table)
    (err "A non-table was passed to import-sobj."))
  (each (k v) t
    (unless (isa k 'sym)
      (err:+ "A table with a non-symbol key was passed to "
             "import-sobj."))
    (unless (acons&single v)
      (err:+ "A table with a non-singleton member was passed to "
             "import-sobj.")))
  (after
    (w/table overwritten
      (each (k (v)) t
        (= my.temp v
           .k.overwritten (list (bound&eval k)))
        (eval `(= ,k ,my!temp))))
    (wipe my.temp)))

; You can use these obj-to-mine and sobj-to-mine inside an nspaced
; form to avoid polluting friendly names in the global environment.
; This will assign each value foo to my.foo (which will macroexpand to
; whatever gensym the nspaced form identified with that local name.

(nspaced:def obj-to-mine (t)
  (unless (isa t 'table)
    (err "A non-table was passed to obj-to-mine."))
  (each (k v) t
    (unless (isa k 'sym)
      (err:+ "A table with a non-symbol key was passed to "
             "obj-to-mine.")))
  (after
    (w/table overwritten
      (each (k v) t
        (= my.temp v
           .k.overwritten (list:eval `(bound&eval ',k)))
        (eval `(= (my ,k) ,my!temp))))
    (wipe my.temp)))

(nspaced:def sobj-to-mine (t)
  (unless (isa t 'table)
    (err "A non-table was passed to sobj-to-mine."))
  (each (k v) t
    (unless (isa k 'sym)
      (err:+ "A table with a non-symbol key was passed to "
             "sobj-to-mine."))
    (unless (acons&single v)
      (err:+ "A table with a non-singleton member was passed to "
             "sobj-to-mine.")))
  (after
    (w/table overwritten
      (each (k (v)) t
        (= my.temp v
           .k.overwritten (list:eval `(bound&eval ',k)))
        (eval `(= (my ,k) ,my!temp))))
    (wipe my.temp)))


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
    (unless (and name target
                 (isa name 'sym) (isa target 'sym)
                 (~ssyntax name) (~ssyntax target))
      (err:+ "A nil, ssyntax, or non-symbol name was in an nmap.")))
  (w/table overwritten-sobj
    (each (name target) nmap
      (= .name.overwritten-sobj (list (bound&eval name)))
      (eval `(= ,name (bound&eval ',target))))))


)