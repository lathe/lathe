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
  jv.jvm
  (with (jnew jv.jvm!java-util-WeakHashMap-new
         (jput jget) (map (if jv.jclass!jarc-table
                            jv.jvm!jarc-Table
                            jv.jvm!java-util-Map)
                          '(put get)))
    
    (=fn my.weak-set ()
      (annotate my!weak-set call.jnew))
    
    (=fn my.weak-set-add (set elem)
      (unless (isa elem 'fn)
        (err:+ "Weak sets can't have non-function elements yet on the "
               "JVM. Only a type whose equals() is == is easy to "
               "support, and for now that means we only support "
               "functions."))
      (do.jput rep.set elem t))
    
    (=fn my.weak-set-has (set elem)
      (do.jget rep.set elem))
    )
  )


)
