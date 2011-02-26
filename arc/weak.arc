; weak.arc
;
; Utilities related to weak references.

(packed:using-rels-as ut "utils.arc"
                      sn "imp/sniff.arc"
                      jv "imp/jvm.arc"


(if
  ; Racket-based setups
  sn.plt
  (do (=fn my.weak-set ()
        (annotate my!weak-set (sn:plt:make-weak-hasheq)))
      
      (=fn my.weak-set-add (set elem)
        (= rep.set.elem t))
      
      (=fn my.weak-set-has (set elem)
        rep.set.elem)
      )
  
  ; JVM-based setups
  sn.jvm
  (ut:lets jwhm jv.jvm!java-util-WeakHashMap
           jnew do.jwhm!new
           jput do.jwhm!put
           jhas do.jwhm!containsKey
    
    (=fn my.weak-set ()
      (annotate my!weak-set call.j-weak-hash-table))
    
    (=fn my.weak-set-add (set elem)
      (unless (isa elem 'fn)
        (err:+ "Weak sets can't have non-function elements yet on the "
               "JVM. Only a type whose equals() is == is easy to "
               "support, and for now that means we only support "
               "functions."))
      (do.jput rep.set elem))
    
    (=fn my.weak-set-has (set elem)
      (do.jhas rep.set elem))
    )
  )


)
