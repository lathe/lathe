; multirule-demo.arc

(prn "starting multirule-demo")

(using-rels "../multival/multirule.arc"
  
  wipe.fail
  
  (rule factorial (n)
    (unless (< 0 n) (fail "The parameter isn't positive."))
    (* n (factorial:- n 1)))
  
  (rule factorial (n)
    (unless (is n 0) (fail "The parameter isn't zero."))
    1)
  )

(for i 0 10
  (prn "The factorial of " i " is " factorial.i "."))

(prn "finishing multirule-demo")

nil