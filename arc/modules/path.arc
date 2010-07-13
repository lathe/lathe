; path.arc
;
; These are utilities for manipulating filesystem paths, as well as
; two kinds of dependency, 'fromwd and 'rel, for loading files from
; paths.
;
; This also provides a 'load-dir* global variable for storing the
; currently loading path as a relative path based from the working
; directory, as well as two variants of 'load which set 'load-dir*
; properly: 'loadfromwd and 'loadrel. The 'loadfromwd function works
; very much like load (except possibly with regards to absolute
; paths), and the 'loadrel function works the same way except that it
; resolves its relative paths based on 'load-dir*.
;
; Analagously to 'loadfromwd and 'loadrel, the 'fromwd dependency,
; given as '(fromwd "relative/path/to/code.arc"), will load a file
; from a path relative to the working directory, and the 'rel
; dependency will load a file from a path relative to 'load-dir*. Both
; of them will update 'load-dir* properly as well, thanks to the fact
; that they use 'loadfromwd internally.
;
; When using these dependency forms, it is assumed that the result of
; loading the file itself will be a package. For instance,
; (activate '(rel "path/whatever.arc")) will try to activate the
; loaded result as a package, and if this doesn't work, there will be
; an error. You can say (prepare '(rel "path/whatever.arc")) in order
; to avoid the activation, but this will still place a non-package
; result in the 'available-packages* list, where it doesn't really
; belong. When loading resources that don't result in packages, you
; should usually use 'loadfromwd and 'loadrel in preference to the
; package system's 'prepare and 'activate procedures.


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
          (when (is #\/ do.path.0) (push "" acc))
          (each segment segments
            (case segment
              "."   nil
              ".."  (if (in car.acc ".." nil)
                      (push ".." acc)
                      pop.acc)
                    (push segment acc)))
          (rev:cons final acc))))))


(= load-dir* "")

(def loadfromwd (wdpath)
  (zap normalize-path wdpath)
  (with ((wddir filename) split-at-dir.wdpath
         old-load-dir load-dir*)
    (after
      (do (= load-dir* wddir)
          loadval.wdpath)
      (= load-dir* old-load-dir))))

(def loadrel (relpath)
  (loadfromwd:string load-dir* relpath))


; The path given here is assumed to have been normalized already.
(def compile-wd-dependency (wdpath)
  (obj type 'compiled-dependency
       prepare (fn ()
                 (withs (original loadfromwd.wdpath
                         result (obj type 'loaded-package
                                     path wdpath
                                     original original))
                   (=fn !nspace.result ()
                     (do.result!original!nspace))
                   (=fn !activate.result ()
                     (let undo-original (do.result!original!activate)
                       (fn ()
                         call.undo-original
                         (zap [rem result _] activated-packages*))))
                   result))
       accepts (fn (package)
                 (and (isa package 'table)
                      (is do.package!type 'loaded-package)
                      (is do.package!path wdpath)))))


; ===== The 'fromwd dependency form ==================================

([push _ car.compile-dependency-rules*]
 (fn (dependency)
   (when (and acons.dependency
              (is car.dependency 'fromwd)
              (single cdr.dependency))
     (let ostensiblepath cadr.dependency
       (when (isa ostensiblepath 'sym) (zap string ostensiblepath))
       (when (isa ostensiblepath 'string)
         (let (wddir filename) split-at-dir.ostensiblepath
           (compile-wd-dependency:+
             normalize-path.wddir filename)))))))

(mac using-fromwds-as withbody
  (let (binds . body) (parse-magic-withlike withbody
                        (+ "An odd-sized list of bindings was given "
                           "to using-fromwds-as."))
    `(using-as ,(mappend [do `(,_.0 `(fromwd ,,_.1))] binds) ,@body)))

(mac use-fromwds-as bindings
  (when (odd:len bindings)
    (err:+ "An odd-sized list of bindings was given to "
           "use-fromwds-as."))
  `(use-as ,@(mappend [do `(,_.0 `(fromwds ,,_.1))] pair.bindings)))


; ===== The 'rel dependency form =====================================

([push _ car.compile-dependency-rules*]
 (fn (dependency)
   (when (and acons.dependency
              (is car.dependency 'rel)
              (single cdr.dependency))
     (let ostensiblepath cadr.dependency
       (when (isa ostensiblepath 'sym) (zap string ostensiblepath))
       (when (isa ostensiblepath 'string)
         (let (reldir filename) split-at-dir.ostensiblepath
           (compile-wd-dependency:+
             (normalize-path:string load-dir* reldir) filename)))))))

(mac using-rels-as withbody
  (let (binds . body) (parse-magic-withlike withbody
                        (+ "An odd-sized list of bindings was given "
                           "to using-rels-as."))
    `(using-as ,(mappend [do `(,_.0 `(rel ,,_.1))] binds) ,@body)))

(mac use-rels-as bindings
  (when (odd:len bindings)
    (err "An odd-sized list of bindings was given to use-rels-as."))
  `(use-as ,@(mappend [do `(,_.0 `(rel ,,_.1))] pair.bindings)))