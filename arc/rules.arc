; rules.arc
;
; ===== Basic rulebooks ==============================================
;
; A basic rulebook is just a list of functions that either result
; normally (by returning or raising an error) or call a supplied
; escape continuation with a failure message. Calling a basic rulebook
; (via my!call-basic-rulebook) will apply each function one by one to
; the arguments and either propagate the first normal result or error
; out with a description of all the rule failure messages.
;
; The escape continuation is an anaphoric parameter named 'fail. Using
; it should look a lot like using 'err, with code like
; (unless (arguments-are-correct) (fail "The arguments weren't...")).
; Note, however, that rules are still free to raise errors using 'err
; if they positively determine that the rulebook should result in an
; error without trying any other rules.
;
; There are other responses a rulebook could theoretically support.
; For example, in Inform 7's rulebooks and CLOS's multimethods, a
; single node can cause the rest of the chain to continue with
; different arguments. That's an easy feature to implement for these
; rulebooks too, but I'm purposefully being conservative with the
; design of these rulebooks. I'd rather encourage wackier kinds of
; rulebooks to be implemented on top of these so that those rulebooks
; can be extended with other wacky behaviors as desired.

(packed:using-rels-as ut "utils.arc"


(if (catch:no:point intercept throw.nil)
  
  ; NOTE: On Jarc 17, any escape continuation's boundary is eligible
  ; to receive any escape continuation's result. (The innermost
  ; boundary wins.) Therefore, in order to seamlessly take advantage
  ; of escape continuations for rule failure, we have to identify
  ; whether the result we get actually belongs to us. If it doesn't,
  ; we have to throw it back. We'll use a gensym for the
  ; identification.
  (=fn my.call-basic-rulebook (rulebook . args)
    (w/uniq g-token
      ; NOTE: We would use a 'catch and 'throw pattern for this loop,
      ; except that we would have to go through all the same hurdles to
      ; keep from intercepting non-failure escape continuations.
      (ut:xloop rest rulebook complaints nil
        (iflet (rule . rest) rest
          (let full-result
                 (catch `(,g-token t
                           ,(apply rule
                              (fn ((o complaint))
                                (throw `(,g-token nil ,complaint)))
                              args)))
            (if (caris full-result g-token)
              (if do.full-result.1
                do.full-result.2
                (do.next rest (consif do.full-result.2 complaints)))
              
              ; NOTE: This is how we throw it back.
              catch.throw.full-result
              ))
          (err:if complaints
            (apply +
              "No rule accepted the given arguments. The specific "
              "complaint" (if single.complaints " was " "s were ")
              "as follows:\n"
              "\n"
              (intersperse "\n" rev.complaints))
            (+ "No rule accepted the given arguments or even had a "
               "specific complaint."))))))
  
  ; NOTE: The above implementation should work on all platforms, but
  ; this is a real performance bottleneck for rule-heavy programs, so
  ; we provide a more efficient implementation for non-Jarc platforms.
  ;
  ; TODO: This doesn't actually give an escape continuation for the
  ; 'fail parameter. If rules try to do weird things with
  ; continuations, dynamic scope, and/or error catching, this will
  ; probably break. Figure out whether this gotcha should be part of
  ; the specification for basic rulebooks, just for speed's sake.
  ;
  (=fn my.call-basic-rulebook (rulebook . args)
    ; NOTE: We're avoiding 'catch, 'while, 'push, and 'pop for speed's
    ; sake. These decisions were made based on Rainbow's profiler, so
    ; there's a chance they aren't as productive on other platforms.
    ; Also note that 'if seems to be slower than 'cons on Rainbow, so
    ; we collect every complaint regardless of whether it's nil.
    (ccc:fn (throw)
      (withs (complaints nil
              next (fn (fail)
                     (if rulebook
                       (throw:apply car.rulebook fail args)
                       (err:if (zap [rem no _] complaints)
                         (apply +
                           "No rule accepted the given arguments. "
                           "The specific complaint"
                           (if single.complaints " was" "s were")
                           " as follows:\n"
                           "\n"
                           (intersperse "\n" rev.complaints))
                         (+ "No rule accepted the given arguments or "
                            "even had a specific complaint.")))))
        (do.next:afn ((o complaint))
          (= complaints (cons complaint complaints)
             rulebook cdr.rulebook)
          do.next.self))))
  )

(=mc my.ru (parms . body)
  `(fn ,(cons 'fail parms)
     ,@body))


)
