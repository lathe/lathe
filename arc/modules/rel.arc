; rel.arc
;
; A type of dependency, given as '(rel "relative/path/to/code.arc"),
; which will load a file from a path relative to the currently loading
; file's path. In order to accomplish this, the load-dir* global
; varible is defined, and it keeps track of the path to the currently
; loading file's directory (relative to the path 'load uses).
;
; It is assumed that the result of loading the file itself will be a
; package. For instance, (activate '(rel "path/whatever.arc")) will
; try to activate the loaded result as a package, and if this doesn't
; work, there will be an error. You can say
; (prepare '(rel "path/whatever.arc")) in order to avoid the
; activation, but this will still place the result in the
; available-packages* list, where it doesn't really belong.
;
; If you don't like any of this hullaballoo, or even if you do, two
; functions are provided to interact with load-dir*: loadrel and
; loadabs. The loadabs function works exactly like load but does the
; proper bookkeeping of load-dir* so that the loaded file can use
; relative paths correctly. The loadrel function does all that but
; itself takes a relative address.

(once-tl "load rel.arc"


(= load-dir* "")

(def split-at-dir (str)
  (catch
    (down i (- len.str 1) 0
      (when (in do.str.i #\/ #\\)
        (throw:split str (+ i 1))))
    (split str 0)))

(def normalize-path (path)
  (zap [subst "/" "\\" _] path)
  (case len.path 0
    ""
    (apply + ""         ; would be "(string", except fails on Rainbow
      (intersperse "/"
        (withs (acc nil
                segments (tokens path #\/)
                final (case (do.path (- len.path 1)) #\/
                        ""
                        (reclist [when (single:cdr _) (pop:cdr _)]
                                 segments)))
          (when (is #\/ .0.path) (push "" acc))
          (each segment segments
            (case segment
              "."   nil
              ".."  (if (in car.acc ".." nil)
                      (push ".." acc)
                      pop.acc)
                    (push segment acc)))
          (rev:cons final acc))))))

([push _ car.compile-dependency-rules*]
 (fn (dependency)
   (when (and acons.dependency
              (is car.dependency 'rel)
              (single cdr.dependency))
     (let relpath cadr.dependency
       (when (isa relpath 'string)
         (withs ((reldir filename) split-at-dir.relpath
                 absdir (normalize-path:string load-dir* reldir)
                 abspath (+ absdir filename))
           (obj type 'compiled-dependency
                prepare (fn ()
                          (withs (original loadabs.abspath
                                  result (obj type 'loaded-package
                                              path abspath
                                              original original))
                            (=fn !nspace.result ()
                              (!original.result!nspace))
                            (=fn !activate.result ()
                              (let undo-original
                                     (!original.result!activate)
                                (fn ()
                                  call.undo-original
                                  (zap [rem result _]
                                       activated-packages*))))
                            result))
                accepts (fn (package)
                          (and (isa package 'table)
                               (iso !type.package 'loaded-package)
                               (iso !path.package abspath))))))))))

(def loadrel (relpath)
  (loadabs:string load-dir* relpath))

(def loadabs (abspath)
  (zap normalize-path abspath)
  (with ((absdir filename) split-at-dir.abspath
         old-load-dir load-dir*)
    (after
      (do
        (= load-dir* absdir)
        load.abspath)
      (= load-dir* old-load-dir))))

(mac using-rels (relpaths . body)
  (unless alist.relpaths (zap list relpaths))
  `(usings ,(map [do ``(rel ,,_)] relpaths) ,@body))

(mac using-rels-as withbody
  (let (binds . body) (parse-magic-withlike withbody
                        (+ "An odd-sized list of bindings was given "
                           "to using-rels-as."))
    `(using-as ,(mappend [do `(,_.0 `(rel ,,_.1))] binds) ,@body)))


)