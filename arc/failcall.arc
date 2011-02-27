; failcall.arc
;
; This simulates a parameter on *every* function call that specifies
; what to do instead if the function call doesn't make sense. This
; parameter is called "fail". In order to pass the parameter to a
; function, you must use my!failcall, and in order to make a function
; that's aware of its fail parameter, you should use my!failfn.
;
; The parameter passed using my!failcall and the parameter received by
; my!failfn aren't quite the same. First, my!failfn wraps the fail
; parameter in an escape continuation so that you can call it anywhere
; in the function and exit the computation.
;
; The default fail parameter--the one that's passed if you don't use
; 'failcall--is my!raise-failure, which calls 'err. Thus, if you call
; a failfn's fail parameter (and if you don't, why is it a failfn?),
; it helps to pass a helpful message describing the nature of the
; failure. If a computation tries a whole lot of things in succession
; and still fails, it should fail (or raise an error) containing all
; the individual failure messages, along with the nature of its own
; involvement in that process, just so that the error message as
; informative as possible. Whatever type you collect the messages in,
; you should make sure my!pprint-failure is extended for that type so
; that it can be turned into an error message properly.
;
; So why fail when you can just call 'err? Suppose your function uses
; an error of type X to signal an undefined case, but it calls a
; function that uses an error of type X to signal an undefined case
; *of its own*. (Maybe this is a recursive call.) You might not want
; the caller of your function to just try another case--maybe you've
; done a significant amount of computation for this case already, or
; maybe you've done I/O--so you need to explicitly wrap up those
; errors of type X that don't belong to you so that they can't be
; confused with yours. But what if the troublesome function you
; called was a higher-order function, and you wanted to raise your
; error of type X *from inside* a callback? Then you have to jump
; through an extra hoop to get around your own error-wrapping
; code--maybe a continuation jump, or maybe another layer of wrapping
; and unwrapping. With this framework, you instead get a fail
; parameter which is visible throughout the function's lexical scope,
; even callbacks it passes, and inner failures don't propagate unless
; you specifically make it so using a failcall.
;
; Why fail when you can just encode success or failure in the return
; type, like Haskell programmers do with Either? Because then callers
; to your function have to explicitly unwrap your return value before
; they use it. Often the undefined cases of a function aren't inherent
; in its meaning; instead, users can do things that augment the
; behavior of that function until it's defined in all the cases they
; need. (This makes less sense in a pure language like Haskell, but
; consider that a Haskell type class is little but a way for certain
; functions' meanings to transcend the designer's own uses for them.)
; Perhaps the *meaning* of the function applies everywhere; then the
; possibility for a "failure" return value is only be significant to
; an application in the making, and it doesn't have an actual impact
; on the final program's control flow. This framework lets the control
; flow be implicit whether or not the program's complete.

(packed:using-rels-as ut "utils.arc"
                      dy "dyn.arc"
                      wk "weak.arc"


; This is the default fail parameter. We'd wrap up a failure in a
; custom error type, but Arc doesn't support throwing arbitrary values
; as exceptions, so we just pretty-print the failure right here.
(=fn my.raise-failure (failure)
  (err my.pprint-failure.failure))

(= my.failsecret* (dy.secretarg my.raise-failure))

(=fn my.failcall (func fail . args)
  (apply dy.call-w/secrets func (list:list my.failsecret* fail) args))


; Make partial functions using this.
;
; TODO: Make number-of-argument errors into failures rather than what
; we're currently doing, which is binding things to nil using
; destructuring.
;
(=mc my.failfn (fail parms . body)
  (w/uniq (g-self g-args g-fail)
    `(,ut!named ,g-self
       (,dy!secretarg-fn ,g-args ,g-fail ,my!failsecret*
         ; We wrap the failures up in a value that should be easy to
         ; pretty-print and inspect.
         (,ut!onpoint ,fail [,g-fail
                              (annotate ',my!function-failure
                                (list ,g-self ,g-args
                                  '(failfn ,fail ,parms ,@body) _))]
            (let ,parms ,g-args
             ,@body))))))


; TODO: Make this a rulebook.
(=fn my.pprint-failure (failure)
  (if (isa failure 'string)
    failure
      (isa failure my!function-failure)
    (let (func args impl complaint) rep.failure
      (+ "/\n"
         "Calling function " (tostring write.func) " with "
           (tostring write.args) ",\n"
         "using implementation\n"
         "  " (tostring write.impl) "\n"
         "failed with this complaint:\n"
         my.pprint-failure.complaint "\n"
         "\\\n"))
      (isa failure my!rulebook-failure)
    (let (name args complaints) rep.failure
      (+ "/\n"
         "Calling function " (tostring write.func) " with "
           (tostring write.args) "\n"
         "failed with these complaints:\n"
         ; TODO: See if this can use 'string on all platforms.
         (apply + "" (map [+ "\n" my.pprint-failure._] complaints))
         "\\\n"))
    ; If it's an unrecognized type, we just use its 'disp appearance.
    (tostring pr.failure)))


(=fn my.fn-ifsuccess (thunk then else)
  (my.failcall (my:failfn fail ()
                 (do.then:my.failcall thunk fail))
               else))

(=mc my.ifsuccess (success failure thunk then . elses)
  `(,my!fn-ifsuccess ,thunk
     (fn (,success) ,then) (fn (,failure) (if ,@elses))))

(=fn my.failcall-cases (cases fail . args)
  (ut:xloop cases cases collected-details nil
    (iflet (case . rest) cases
      (my:ifsuccess
          success failure (my:failfn fail ()
                            (apply my.failcall case fail args))
        success
        (do.next rest (cons failure collected-details)))
      do.fail.collected-details)))


; Without further ado, here's one way to explicitly set up a generic
; function.

(= my.fact-cases* nil)
(= my.fact (my:failfn fail args
             (apply my.failcall-cases my.fact-cases*
              ; We wrap the failures up in a value that should be easy
              ; to pretty-print and inspect.
              [do.fail:annotate my!rulebook-failure
                (list 'fact args rev._)]
              args)))

(push (my:failfn fail (n)
        (* n (my.fact:- n 1)))
      my.fact-cases*)

(push (my:failfn fail (n)
        (unless (is n 0)
          (do.fail "The number wasn't 0."))
        1)
      my.fact-cases*)

)

