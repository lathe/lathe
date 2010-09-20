; rules.arc
;
; ===== Basic rulebooks ==============================================
;
; A basic rulebook is just a list of functions that return either a
; rule-success or a rule-failure. Calling a basic rulebook (via
; call-basic-rulebook) will apply each function one by one to the
; arguments and either return the first success value or error out
; with a description of all the failures.
;
; A rule-failure is sort of a soft failure. A rule can also fail hard
; by just causing a normal Arc error or by returning a value through
; rule-success that signifies failure, such as nil.
;
; There are other responses a rulebook could theoretically support.
; For example, in Inform 7's rulebooks and CLOS's multimethods, a
; single node can cause the rest of the chain to continue with
; different arguments. That's an easy feature to implement for these
; rulebooks too, but I'm purposefully being conservative with the
; design of these rulebooks. I'd rather encourage wackier kinds of
; rulebooks to be implemented on top of these so that those rulebooks
; can be extended with other wacky behaviors as desired.

(packed


(=fn my.rule-success (return-value)
  `(success ,return-value))

(=fn my.rule-failure ((o description))
  `(failure ,description))

(=fn my.call-basic-rulebook (rulebook . args)
  ; NOTE: We would use a 'catch and 'throw pattern here, except that
  ; we would capture escape continuation calls in the calls to the
  ; rules on Jarc 17.
  (ut:xloop rest rulebook failures nil
    (iflet (rule . rest) rest
      (let (result-type result-details) (apply rule args)
        (case result-type
          success  result-details
          failure  (do.next rest (if result-details
                                   (cons result-details failures)
                                   failures))
            (err "There was an unrecognized rule result type.")))
      (err:if failures
        (apply +
          "No rule accepted the given arguments. The specific "
          "complaint" (if single.failures " was" "s were") " as "
          "follows:\n"
          "\n"
          (intersperse "\n" rev.failures))
        (+ "No rule accepted the given arguments or even had a "
           "specific complaint.")))))

(=mc my.ru (parms . body)
  ; NOTE: On Jarc 17, any escape continuation's boundary is eligible
  ; to receive any escape continuation's result. (The innermost
  ; boundary wins.) Therefore, in order to seamlessly take advantage
  ; of escape continuations, we have to identify whether the result we
  ; get actually belongs to us. If it doesn't, we have to throw it
  ; back. We'll use a gensym for the identification.
  (w/uniq (g-return g-token)
    `(fn ,parms
       (w/uniq ,g-token
         (let result-holder
                (point ,g-return
                  (let fail [,g-return
                              (list ,g-token (,my!rule-failure _))]
                    (list ,g-token (,my!rule-success (do ,@body)))))
           (if (and acons.result-holder (caris result-holder ,g-token))
             result-holder.1
             
             ; NOTE: This is how we throw it back. We should only get
             ; to here on Jarc.
             catch.throw.result-holder
             ))))))


)