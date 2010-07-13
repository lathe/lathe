; more-module-stuff.arc
;
; This is a collection of utilities for use with the Lathe module
; system loaded by loadfirst.arc.
;
; These were originally part of the module system proper, but it
; turned out that they weren't especially necessary. Rather than
; remaining in the module system core where they would pollute the
; global namespace, they're here in an independent module.
;
; If you especially like these utilities, you can place them in the
; global namespace yourself by running this command:
;
;   (activate `(rel ,(+ lathe-dir* "more-module-stuff.arc")))
;
; Actually, 'activate itself could be placed in this module too, but
; if it were here, it would be slightly more difficult to accomplish
; the same effect as the above command, so it's staying as part of the
; core.

(packed


; originally from module/once.arc

(= my.onces* '(()))

(=mc my.once (id . body)
  `(,my!fn-once ,id (fn () ,@body)))

(=fn my.fn-once (id body)
  (unless (some [iso id _] (car my.onces*))
    (fn-once-at-a-time id body)
    (push id (car my.onces*))))

(=mc my.once-tl (id . body)
  `(,my!once ,id (tldo ,@body)))


; originally from module/nspace.arc

(=mc my.copy-to-mine names
  (unless (all anormalsym names)
    (err:+ "A non-symbol or special symbol expression was passed to "
           "copy-to-mine."))
  `(= ,@(mappend [do `((my ,_) ,_)] names)))

(=mc my.copy-to-nspace (ns . names)
  (unless (all anormalsym names)
    (err:+ "A non-symbol or special symbol expression was passed to "
           "copy-to-nspace."))
  (w/uniq g-ns
    `(= ,g-ns ,ns
        ,@(mappend [do `((,g-ns ,_) ,_)] names)
        ,g-ns nil)))

; This is a spoof form that imitates an nspaced form without actually
; protecting any variables. The point is that if you really don't like
; the effect nspaced has on Arc code, then you can drop in this
; non-implementation, maybe even saying (= nspaced not-nspaced) at the
; top level, and thereby use most code that's targeted at nspaced in a
; way that's closer to what you want without very much hassle.
(=mc my.not-nspaced body
  (let name (or (and acons.body cdr.body (check car.body anormalsym))
                'my)
    `(w/global ,name (mc (what) what)
       (tldo ,@body))))

; originally from module/import.arc

; This is an obj where each value is wrapped in a singleton list.
(=mc my.sobj bindings
  `(obj ,@(mappend [do `(,_.0 (list ,_.1))] pair.bindings)))

; Use this inside an nspaced form in order to save off the values of
; the given names into a table of singleton lists, using the names
; themselves as keys. This table can then be sent to import-sobj in
; order to conveniently use the values in some other context.
(=mc my.mine-as-sobj names
  `(sobj ,@(mappend [do `(,_ (my ,_))] names)))

(=fn my.import-obj (obj)
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

; You can use these obj-to-mine and sobj-to-mine inside an nspaced
; form to avoid polluting friendly names in the global environment.
; This will assign each value foo to my.foo (which will macroexpand to
; whatever gensym the nspaced form identified with that local name.

(=fn my.obj-to-mine (obj)
  (unless (isa obj 'table)
    (err "A non-table was passed to obj-to-mine."))
  (each (name value) obj
    (unless anormalsym.name
      (err:+ "A table with a nil, ssyntax, or non-symbol key was "
             "passed to obj-to-mine.")))
  (w/table overwritten
    (each (name value) obj
      (= .name.overwritten (list:eval `(my ,name))
         (global:eval `(my ',name)) value))))

(=fn my.sobj-to-mine (sobj)
  (unless (isa sobj 'table)
    (err "A non-table was passed to sobj-to-mine."))
  (each (name svalue) sobj
    (unless anormalsym.name
      (err:+ "A table with a nil, ssyntax, or non-symbol key was "
             "passed to sobj-to-mine."))
    (unless single.svalue
      (err:+ "A table with a non-singleton member was passed to "
             "sobj-to-mine.")))
  (w/table overwritten
    (each (name (value)) sobj
      (= .name.overwritten (list:eval `(my ,name))
         (global:eval `(my ',name)) value))))


; originally from module/package.arc

(=mc my.using (dependency . body)
  `(protect (fn () ,@body) (activate ,dependency)))

(=mc my.usings (dependencies . body)
  (if no.dependencies
    `(tldo ,@body)
      atom.dependencies
    `(,my!using ,dependencies ,@body)
    `(,my!using ,car.dependencies
       (,my!usings ,cdr.dependencies ,@body))))

; Note that this creates a package with an empty nspace.
(=mc my.pack-sobj (sobj)
  (let export (obj sobj sobj)
    (= !nspace.export (let ns (nspace) (fn () ns)))
    (=fn !activate.export ()
      (let overwritten-sobj (import-sobj do.export!sobj)
        (fn ()
          (zap [rem [is export _] _] activated-packages*)
          import-sobj.overwritten-sobj)))
    export))

; The 'names here must be either a list of symbols or a symbol.
(=mc my.packing (names . body)
  (unless alist.names (zap list names))
  (unless (all anormalsym names)
    (err:+ "A nil, ssyntax, or non-symbol name was given to "
           "'packing."))
  `(let nmap (table)
     (w/global my nspace.nmap
       (tldo ,@body))
     (pack-nmap:obj ,@(mappend [do `(,_ (do.nmap ',_))] names))))

; The 'names here must be either a list of symbols or a symbol.
(=mc my.pack-hiding (names . body)
  (unless alist.names (zap list names))
  (unless (all anormalsym names)
    (err:+ "A nil, ssyntax, or non-symbol name was given to "
           "'pack-hiding."))
  `(let nmap (table)
     (w/global my nspace.nmap
       (tldo ,@body))
     (pack-nmap:copy nmap ,@(mappend [do `(',_ nil)] names))))


; originally from modules/path.arc

(=mc my.using-rels (relpaths . body)
  (unless alist.relpaths (zap list relpaths))
  `(,my!usings ,(map [do ``(rel ,,_)] relpaths) ,@body))


)