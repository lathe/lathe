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


(=fn my.call-basic-rulebook (rulebook . args)
  ; NOTE: On Jarc 17, any escape continuation's boundary is eligible
  ; to receive any escape continuation's result. (The innermost
  ; boundary wins.) Therefore, in order to seamlessly take advantage
  ; of escape continuations for rule failure, we have to identify
  ; whether the result we get actually belongs to us. If it doesn't,
  ; we have to throw it back. We'll use a gensym for the
  ; identification.
  (w/uniq g-token
    ; NOTE: We would use a 'catch and 'throw pattern for this loop,
    ; hexcept that we would have to go through all the same hurdles to
    ; keep from intercepting non-failure escape continuations.
    (ut:xloop rest rulebook failures nil
      (iflet (rule . rest) rest
        (let full-result
               (catch `(,g-token t
                         ,(apply rule
                            (fn ((o message))
                              (throw `(,g-token nil ,message)))
                            args)))
          (if (caris full-result g-token)
            (if do.full-result.1
              do.full-result.2
              (do.next rest (consif do.full-result.2 failures)))
            
            ; NOTE: This is how we throw it back. We should only get
            ; to here on Jarc.
            catch.throw.full-result
            ))
        (err:if failures
          (apply +
            "No rule accepted the given arguments. The specific "
            "complaint" (if single.failures " was" "s were") " as "
            "follows:\n"
            "\n"
            (intersperse "\n" rev.failures))
          (+ "No rule accepted the given arguments or even had a "
             "specific complaint."))))))

(=mc my.ru (parms . body)
  `(fn ,(cons 'fail parms)
     ,@body))


)