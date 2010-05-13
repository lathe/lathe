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
  (mccmp catch let failures '()
    (each rule rulebook
      (let (result-type result-details) (apply rule args)
        (case result-type
          success  throw.result-details
          failure  (when result-details
                     (push result-details failures))
                   (err:+ "There was an unknown rule result type."))))
    (mccmp err if failures
      (apply +
        "No rule accepted the given arguments. The specific "
        "complaint" (if single.failures " was" "s were") " as "
        "follows:\n"
        "\n"
        (intersperse "\n" rev.failures))
      (+ "No rule accepted the given arguments or even had a "
         "specific complaint."))))

(=mc my.ru (parms . body)
  (w/uniq g-return
    `(fn ,parms
       (point ,g-return
         (let fail (fn (msg) (,g-return (,my!rule-failure msg)))
           (,my!rule-success (do ,@body)))))))


)