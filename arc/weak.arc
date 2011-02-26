; weak.arc
;
; Utilities related to weak references.

(packed:using-rels-as ut "utils.arc"
                      sn "imp/sniff.arc"
                      jv "imp/jvm.arc"


(if
  ; Racket-based setups
  sn.plt
  (do (=fn my.weqtable ()
        (annotate my!weqtable (sn:plt:make-weak-hasheq)))
      
      (=fn my.weqtable-get (set elem)
        rep.set.elem)
      
      (=fn my.weqtable-set (set elem val)
        (= rep.set.elem val))
      )
  
  ; JVM-based setups
  jv.jvm
  (with (jnew jv.jvm!java-util-WeakHashMap-new
         (jget jset) (map (if jv.jclass!jarc-table
                            jv.jvm!jarc-Table
                            jv.jvm!java-util-Map)
                          '(get put)))
    
    (=fn my.weqtable ()
      (annotate my!weqtable call.jnew))
    
    (=fn my.weqtable-get (set elem)
      (do.jget rep.set elem))
    
    (=fn my.weqtable-set (set elem val)
      (unless (isa elem 'fn)
        (err:+ "A 'weqtable can't have a non-function key yet on the "
               "JVM. Only a type whose equals() is == is easy to "
               "support, and for now that means we only support "
               "functions."))
      (do.jset rep.set elem val))
    )
  )


)
