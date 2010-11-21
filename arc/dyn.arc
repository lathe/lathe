; dyn.arc
;
; Dynamically bound parameters.

(packed:using-rels-as ut "utils.arc"
                      sn "imp/sniff.arc"
                      jv "imp/jvm.arc"


; Our dynamic parameter quantifications (my!param-let) can correspond
; perfectly to the quantification's dynamic extent on Jarc 17 (where
; reentrant continuations aren't supported), and on Racket-based
; setups (where we can use Racket's 'parameterize).
(= my.reentrant-params* (or (no sn.cccraziness*) sn.plt))

; On non-Racket setups, our implementation of my!param-let uses what
; amounts to a "finally" cleanup phase, so it isn't a tail call.
;
; NOTE: Jarc doesn't like ~~.
;
(= my.param-let-uses-a-tail-call* (~no sn.plt))

(=fn my.aparam (x)
  (isa x my!param))


; TODO: Make sure the two implementations of my!param-let are
; equivalent with regard to threads and my!param-set.
(if
  ; Racket-based setups
  sn.plt
  (let make-parameter (sn:plt make-parameter)
    
    (=fn my.make-param ((o initial-value))
      (annotate my!param make-parameter.initial-value))
    
    (=fn my.param-get (param)
      (unless my.aparam.param
        (err "A non-parameter was given to 'param-get."))
      (rep.param))
    
    (=fn my.param-set (param new-value)
      (unless my.aparam.param
        (err "A non-parameter was given to 'param-set."))
      (rep.param thunk.new-value)
      (rep.param))
    
    (=mc my.param-let body
      (let binds (if (alist car.body)  pop.body
                     cdr.body          (list pop.body pop.body))
        (when (odd len.binds)
          (err "A 'param-let form had an odd-length binding list."))
        (zap [map [do `(,(uniq) (rep:check ,_.0 ,my!aparam
                                  (err:+ "A 'param-let form was "
                                         "given at least one "
                                         "non-parameter to bind."))
                        ,(uniq) (fn () ,_.1))]
                  pair._]
             binds)
        ; NOTE: Instead of figuring out what hoops to jump through to
        ; get (parameterize () (body)) rather than
        ; (parameterize nil (body)), we just cut to the chase and skip
        ; the 'parameterize.
        (case binds nil
          `(do ,@body)
          `(with (,@(apply join binds) body (fn () ,@body))
             (,sn!plt (parameterize ,(map [do `(,_.0 (,_.2))] binds)
                        (body)))))))
    )
  
  
  ; JVM-based setups
  jv.jclass!java-lang-InheritableThreadLocal
  (do
    (=fn my.make-param ((o initial-value))
      (ut:ret param (annotate my!param
                      (jv.jvm!java-lang-InheritableThreadLocal-new))
        (my.param-set param initial-value)))
    
    (=fn my.param-get (param)
      (unless my.aparam.param
        (err "A non-parameter was given to 'param-get."))
      (call:jv.jvm!get rep.param))
    
    ; NOTE: Since my!param-set is used from an 'after block in
    ; my!param-let, we make sure it doesn't depend on 'on-err on
    ; Rainbow. Rainbow can't deal with 'on-err in an 'after block.
    (if sn.rainbowdrop*
      
      (=fn my.param-set (param new-value)
        (unless my.aparam.param
          (err "A non-parameter was given to 'param-set."))
        (rep.param 'set thunk.new-value)
        (rep.param!get))
      
      (=fn my.param-set (param new-value)
        (unless my.aparam.param
          (err "A non-parameter was given to 'param-set."))
        (jv.jvm!set rep.param thunk.new-value)
        (call:jv.jvm!get rep.param))
      )
    
    (=mc my.param-let body
      (let binds (if (alist car.body)  pop.body
                     cdr.body          (list pop.body pop.body))
        (when (odd len.binds)
          (err "A 'param-let form had an odd-length binding list."))
        (zap [map [do `((,(uniq) (check ,_.0 ,my!aparam
                                   (err:+ "A 'param-let form was "
                                          "given at least one "
                                          "non-parameter to bind.")))
                        (,(uniq) ,_.1))]
                  pair._]
             binds)
        (let resets (map [do `(,my!param-set ,_.0.0 ,(uniq))] binds)
          `(with ,(mappend car binds)
             (with ,(mappend cadr binds)
               (with ,(mappend [do `(,_.2 (,my!param-get ,_.1))]
                               resets)
                 ; NOTE: We only pray that there are no errors when we
                 ; try to set the values of the parameters. If the
                 ; parameters have been manipulated only according to
                 ; our own API, the only errors should end up being
                 ; pretty exceptional, like StackOverflowErrors.
                 ,@(map [do `(,my!param-set ,_.0.0 ,_.1.0)] binds)
                 (after (do ,@body)
                   ,@resets)))))))
    )
  )


)
