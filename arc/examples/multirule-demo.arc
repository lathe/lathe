; multirule-demo.arc

(prn "starting multirule-demo")

; This is a package so that it's possible to use the REPL to do manual
; tests on the functions defined here.
(packed:using-rels-as mr "../multival/multirule.arc"
                      oc "../multival/order-contribs.arc"


wipe.fail

(mr:rule my.factorial (n)
  (unless (< 0 n) (fail "The parameter isn't positive."))
  (* n (my.factorial:- n 1)))

(mr:rule my.factorial (n)
  (unless (is n 0) (fail "The parameter isn't zero."))
  1)

(mr:rule my.expbysquare (base exp) my.one
  (unless (is exp 1) (fail "The exponent isn't one."))
  (list base 0))

(mr:rule my.expbysquare (base exp)
  (unless even.exp (fail "The exponent isn't even."))
  (let (prevresult mults) (my.expbysquare base (/ exp 2))
    (list (* prevresult prevresult) (+ mults 1))))

(mr:rule my.expbysquare (base exp)
  ; This condition subsumes the condition for the 'one contribution on
  ; purpose, so that we can test 'prefer. Since this contribution
  ; comes later in the code, it ends up farther toward the beginning
  ; of the contribution stack, and ultimately this rule would be tried
  ; before the 'one rule (catastrophically) if not for the presence of
  ; [iso (map _ '(name label)) (list my!expbysquare my!one)] in the
  ; 'prefer line below.
  (unless odd.exp (fail "The exponent isn't odd."))
  (let (prevresult mults) (my.expbysquare base (- exp 1))
    (list (* base prevresult) (+ mults 1))))

(mr:rule my.expbysquare (base exp) my.diabolical
  (err:+ "Somehow the 'diabolical contribution to 'expbysquare was "
         "used."))

(oc.prefer [iso (map _ '(name label)) (list my!expbysquare my!one)]
           [~iso (map _ '(name label))
                 (list my!expbysquare my!diabolical)])


(for i 0 10
  (prn "The factorial of " i " is " my.factorial.i "."))

(for i 1 10
  (let (result mults) (my.expbysquare 2 i)
    (prn "Two to the " i " gives " result " after "
         (plural mults "multiplication") ".")))

(prn "finishing multirule-demo")

nil


)