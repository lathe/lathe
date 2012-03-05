; weak.arc
;
; Utilities related to weak references.

(packed:using-rels-as ut "utils.arc"
                      sn "imp/sniff.arc"
                      jv "imp/jvm.arc"


(if
  ; Racket-based setups besides ar
  sn.plt*
  (do (= my.weqtable-support* t)
      
      (=fn my.weqtable ()
        (annotate my!weqtable (sn:plt:make-weak-hasheq)))
      
      (=fn my.weqtable-get (set elem)
        rep.set.elem)
      
      (=fn my.weqtable-set (set elem val)
        (= rep.set.elem val))
      )
  
  ; Ar
  sn.ar-plt*
  (do (= my.weqtable-support* t)
      
      (=fn my.weqtable ()
        (annotate my!weqtable (racket-make-weak-hasheq)))
      
      (=fn my.weqtable-get (set elem)
        rep.set.elem)
      
      (=fn my.weqtable-set (set elem val)
        (= rep.set.elem val))
      )
  
  ; JVM-based setups
  jv.jvm*
  (with (jnew jv.jvm!java-util-WeakHashMap-new
         (jget jset) (map (if jv.jclass!jarc-table
                            jv.jvm!jarc-Table
                            jv.jvm!java-util-Map)
                          '(get put)))
    
    (= my.weqtable-support* t)
    
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
  
  ; Others (just Rainbow.js for now)
  ; TODO: If Rainbow.js ever supports threads, try to do something
  ; like the JVM implementation there.
  ; TODO: Once we support Arcueid, handle Arcueid here too.
  (do (= my.weqtable-support* nil)
      
      (=fn my.weqtable ()
        (annotate my!weqtable (table)))
      
      (=fn my.weqtable-get (set elem)
        rep.set.elem)
      
      (=fn my.weqtable-set (set elem val)
        (unless (isa elem 'fn)
          (err:+ "A 'weqtable can't have a non-function key yet on "
                 "Rainbow.js."))
        (= rep.set.elem val))
      )
  )


)
