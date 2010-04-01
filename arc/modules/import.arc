; import.arc

(once-tl "load import.arc"


; This is an obj where each value is wrapped in a singleton list.
(mac sobj bindings
  `(obj ,@(mappend (fn ((k v)) `(,k (list ,v))) pair.bindings)))

; Use this inside an nspaced form in order to save off the values of
; the given names into a table of singleton lists, using the names
; themselves as keys. This table can then be sent to import-sobj in
; order to conveniently use the values in some other context.
(mac locals names
  `(sobj ,@(mappend [do `(,_ (local ,_))] names)))


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
        (= local.temp v
           .k.overwritten (list (bound&eval k)))
        (eval `(= ,k ,local!temp))))
    (wipe local.temp)))

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
        (= local.temp v
           .k.overwritten (list (bound&eval k)))
        (eval `(= ,k ,local!temp))))
    (wipe local.temp)))

; You can use these obj-to-local and sobj-to-local inside an nspaced
; form to avoid polluting friendly names in the global environment.
; This will assign the values to local.my-var (which will macroexpand
; to whatever gensym the nspaced form identified with that local
; name).

(nspaced:def obj-to-local (t)
  (unless (isa t 'table)
    (err "A non-table was passed to obj-to-local."))
  (each (k v) t
    (unless (isa k 'sym)
      (err:+ "A table with a non-symbol key was passed to "
             "obj-to-local.")))
  (after
    (w/table overwritten
      (each (k v) t
        (= local.temp v
           .k.overwritten (list:eval `(bound&eval ',k)))
        (eval `(= (local ,k) ,local!temp))))
    (wipe local.temp)))

(nspaced:def sobj-to-local (t)
  (unless (isa t 'table)
    (err "A non-table was passed to sobj-to-local."))
  (each (k v) t
    (unless (isa k 'sym)
      (err:+ "A table with a non-symbol key was passed to "
             "sobj-to-local."))
    (unless (acons&single v)
      (err:+ "A table with a non-singleton member was passed to "
             "sobj-to-local.")))
  (after
    (w/table overwritten
      (each (k (v)) t
        (= local.temp v
           .k.overwritten (list:eval `(bound&eval ',k)))
        (eval `(= (local ,k) ,local!temp))))
    (wipe local.temp)))


)