; multirule-demo.arc

(prn "starting multirule-demo")

(using-rels-as mr "../multival/multirule.arc"
               oc "../multival/order-contribs.arc"
               mt "../multival/multival.arc"


wipe.fail

(mr:rule factorial (n)
  (unless (< 0 n) (fail "The parameter isn't positive."))
  (* n (factorial:- n 1)))

(mr:rule factorial (n)
  (unless (is n 0) (fail "The parameter isn't zero."))
  1)

(mr:rule expbysquare (base exp) one
  (unless (is exp 1) (fail "The exponent isn't one."))
  (list base 0))

(mr:rule expbysquare (base exp) even
  (unless even.exp (fail "The exponent isn't even."))
  (let (prevresult mults) (expbysquare base (/ exp 2))
    (list (* prevresult prevresult) (+ mults 1))))

(mr:rule expbysquare (base exp) odd
  ; This condition subsumes the condition for the 'one contribution on
  ; purpose, so that we can test 'prefer-contribs. Since this
  ; contribution comes later in the code, it ends up farther toward
  ; the beginning of the contribution stack, and ultimately this rule
  ; would be tried before 'one (catastrophically) if not for the
  ; [is car._ 'one] in the 'prefer-contribs line below.
  (unless odd.exp (fail "The exponent isn't odd."))
  (let (prevresult mults) (expbysquare base (- exp 1))
    (list (* base prevresult) (+ mults 1))))

(mr:rule expbysquare (base exp) diabolical
  (err:+ "Somehow the 'diabolical contribution to 'expbysquare was "
         "used."))

(oc.prefer-contribs [is car._ 'one] [isnt car._ 'diabolical])


(for i 0 10
  (prn "The factorial of " i " is " factorial.i "."))

(for i 1 10
  (let (result mults) (expbysquare 2 i)
    (prn "Two to the " i " gives " result " after "
         (plural mults "multiplication") ".")))

(prn "finishing multirule-demo")

nil


)