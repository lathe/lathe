; dyn.arc
;
; Dynamically bound parameters.

(packed:using-rels-as ut "utils.arc"
                      wk "weak.arc"
                      sn "imp/sniff.arc"
                      jv "imp/jvm.arc"


; Our dynamic parameter quantifications (my!param-let) can correspond
; perfectly to the quantification's dynamic extent on Jarc 17 (where
; reentrant continuations aren't supported), and on Racket-based
; setups (where we can use Racket's 'parameterize).
(= my.reentrant-params* (or (no sn.cccraziness*) sn.plt* sn.ar-plt*))

; On non-Racket setups, our implementation of my!param-let uses what
; amounts to a "finally" cleanup phase, so it isn't a tail call.
(= my.param-let-uses-a-tail-call* (or sn.plt* sn.ardrop*))

(=fn my.aparam (x)
  (isa x my!param))


; TODO: Make sure the two implementations of my!param-let are
; equivalent with regard to threads and my!param-set.
(if
  ; Racket-based setups besides ar
  sn.plt*
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
  
  ; Ar
  sn.ar-plt*
  (do
    
    (=fn my.make-param ((o initial-value))
      (annotate my!param racket-make-parameter.initial-value))
    
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
        ; get (racket-parameterize () (body)) rather than
        ; (racket-parameterize nil (body)), we just cut to the chase
        ; and skip the 'racket-parameterize.
        (case binds nil
          `(do ,@body)
          `(with (,@(apply join binds) body (fn () ,@body))
             (,sn!ar-plt
               (racket-parameterize
                   ,(map [do `(,_.0 (,_.2))] binds)
                 (body)))))))
    )
  
  
  ; JVM-based setups
  jv.jclass!java-lang-InheritableThreadLocal
  (let (jnew jget jset) (map jv.jvm!java-lang-InheritableThreadLocal
                             '(new get set))
    
    (=fn my.make-param ((o initial-value))
      (ut:ret param (annotate my!param call.jnew)
        (my.param-set param initial-value)))
    
    (=fn my.param-get (param)
      (unless my.aparam.param
        (err "A non-parameter was given to 'param-get."))
      (call:do.jget rep.param))
    
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
        (do.jset rep.param thunk.new-value)
        (call:do.jget rep.param))
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


; We also introduce the concept of a "secretarg," which is essentially
; a dynamic parameter that only lasts for a single function call, and
; which a my!secretarg-fn can use throughout the *lexical* context of
; the function. It's a lot like giving an extra argument to every
; function in the language, where most functions ignore the argument
; and most function calls provide the same default value for the
; argument.
;
; By default, core Arc utilities like 'apply and 'memo aren't prepared
; for secretargs. Neither are most Lathe utilities. To partially make
; up for that, we provide my!secretarg-aware-apply. Other utilities
; need to be similarly redefined as they're needed.

(= my.passing-secretargs* my.make-param.nil)
(= my.secretargs* (my.make-param))
(= my.secretarg-aware* (wk.weqtable))

(=fn my.secretarg (default)
  (annotate my!secretarg list.default))

(=fn my.secretargs ()
  (my.param-get my.secretargs*))

(=fn my.secretarg-get (secretarg)
  (aif (find [is _.0 secretarg] (my.secretargs))
    it.1
    rep.secretarg.0))

(=fn my.call-w/secrets (func secrets . args)
  (unless (all [and alist._ (is len._ 2)] secrets)
    (err:+ "The secrets passed to call-w/secrets weren't in an "
           "association list."))
  (if (wk.weqtable-get my.secretarg-aware* func)
    (my:param-let (my.passing-secretargs* t my.secretargs* secrets)
      (apply func args))
    (apply func args)))

(=fn my.secretarg-aware (inner-func)
  (ut:ret result (afn args
                   (if (my.param-get my.passing-secretargs*)
                     (my:param-let my.passing-secretargs* nil
                       (apply inner-func args))
                     (apply my.call-w/secrets self nil args)))
    (wk.weqtable-set my.secretarg-aware* result t)))

(=mc my.secretarg-fn (parms . secretbinds-and-body)
  (let (secretbinds . body) parse-magic-withlike.secretbinds-and-body
    `(,my!secretarg-aware
       (fn ,parms
         (with ,(mappend [list _.0 `(,my!secretarg-get ,_.1)]
                         secretbinds)
           ,@body)))))

(= my.secretarg-aware-apply
   ; NOTE: Ar parses a.b:c as (a b:c).
   (my:secretarg-aware:fn (func . args)
     (apply my.call-w/secrets func (my.secretargs) args)))


)
