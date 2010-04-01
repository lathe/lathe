; package.arc
;
; At runtime, some packages are prepared, and some of those packages
; are active. The point of this distinction is so that packages that
; interfere with each other (particularly when overwriting global
; bindings) can be activated over and over in different sequences as
; the program requires.
;
; It's assumed that activating a package twice will have no particular
; benefit. Accordingly, when 'activate is called, that doesn't
; necessarily activate a package. First, currently activated packages
; are checked to see if they already fit the bill, and only if they
; don't does the activation actually take place.

(once-tl "load package.arc"


(= prepared-packages* '())
(def prepared (dependency)
  (let compiled compile-dependency-mandatory.dependency
    (some [package-satisfies _ compiled] prepared-packages*)))

(= activated-packages* '())
(def activated (dependency)
  (let compiled compile-dependency-mandatory.dependency
    (some [package-satisfies _ compiled] activated-packages*)))

; This returns the package object.
(def prepare (dependency)
  (let compiled compile-dependency-mandatory.dependency
    (or prepared.compiled
      (iflet package (!prepare.compiled)
        (do (push package prepared-packages*)
            package)
        (err:+ "Couldn't prepare " dependency ".")))))  ; NOT compiled

; This returns a procedure which will undo the activation.
(def activate (dependency)
  (let compiled compile-dependency-mandatory.dependency
    (unless activated.compiled
      (once-at-a-time `(activate ,dependency)  ; NOT compiled
        (let package prepare.compiled
          (do1 (package!activate)
               (zap [cons package (rem [deactivates package _] _)]
                    activated-packages*)))))))

(def fn-using (dependency body)
  (let undo activate.dependency
    call.body
    call.undo))

(mac using (dependency . body)
  `(fn-using ,dependency (fn () ,@body)))

(mac usings (dependencies . body)
  (if no.dependencies
    `(do ,@body)
      atom.dependencies
    `(using ,dependencies ,@body)
    `(using ,car.dependencies (usings ,cdr.dependencies ,@body))))


; Each of these rules should behave like this, as far as types go:
;
; (fn (dependency)
;   (when (this-rule-applies)
;     (obj type 'compiled-dependency
;          prepare (fn ()
;                    (when (can-get-resources)
;                      (obj activate
;                           (fn ()
;                             (have-side-effects)
;                             (fn () (undo-those-side-effects))))))
;          accepts (fn (package)
;                    (bool-implementation))))
;
; Note that 'accepts should return t for any package which 'prepare
; ould return, and preferably for only those packages, unless there's
; a good reason for this dependency to be satisfied by a package
; prepared from some other dependency.
;
(= compile-dependency-rules* '(()))
(def compile-dependency (dependency)
  (if (and (isa dependency 'table)
           (is !type.dependency 'compiled-dependency))
    dependency
    (some [_ dependency] car.compile-dependency-rules*)))

(def compile-dependency-mandatory (dependency)
  (or compile-dependency.dependency
    (err:+ "Not a valid dependency: " dependency)))

(def package-satisfies (package dependency)
  (let compiled compile-dependency-mandatory.dependency
    (!accepts.compiled package)))

; Each of these rules should behave like this, as far as types go:
;
; (fn (package-one package-two)
;   (when (this-rule-applies)
;     (list (bool-implementation))))
;
; By default, every package deactivates every other one. Furthermore,
; no package can deactivate one iso to it, even if a rule would
; suggest otherwise.
;
(= deactivates-rules* '(()))
(def deactivates (package-one package-two)
  (aif (iso package-one package-two)
    nil
       (some [_ package-one package-two] car.deactivates-rules*)
    car.it
    t))


; The 'names here has to be an sobj.
(def pack (names)
  (let export (obj names names)
    (= !activate.export
         (fn ()
           (let overwritten (import-sobj !names.export)
             (fn ()
               (zap [rem [is export _] _] activated-packages*)
               import-sobj.overwritten))))
    export))

; The 'names here must be either a list of symbols or a symbol.
(mac packing (names . body)
  (unless acons.names (zap list names))
  `(nspaced ,@body (pack:locals ,@names)))


)