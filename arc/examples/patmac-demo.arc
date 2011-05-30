; patmac-demo.arc

(prn "starting patmac-demo")


(nspaced:using-rels-as pm "../patmac.arc"


(= my.tests-succeeded t)

(=mc my.test-similar (relation simple complicated)
  (w/uniq (g-relation g-simple g-complicated)
    `(with (,g-relation ,relation
            ,g-simple ,simple
            ,g-complicated ,complicated)
       (unless (,g-relation ,g-simple ,g-complicated)
         (wipe ,my!tests-succeeded)
         (prn:+ "FAILED: These should satisfy " ',relation ": "
                ,g-simple " and " ,g-complicated ". The latter's "
                "expression was " ',complicated ".")))))

(=mc my.test-iso (simple complicated)
  `(,my!test-similar iso ,simple ,complicated))


(my:test-iso '((nil d) (a nil))
             (accum acc
               (pm:each-match (or `(a ,b) `(,c d)) '(a d)
                 ; NOTE: Ar parses a.b:c as (a b:c).
                 (do.acc (list c b)))))

(my:test-iso '((nil d))
             (accum acc
               (pm:each-match (atomic (or `(a ,b) `(,c d))) '(a d)
                 ; NOTE: Ar parses a.b:c as (a b:c).
                 (do.acc (list c b)))))


(if my.tests-succeeded (prn "All tests succeeded!"))

(prn "finishing patmac-demo")

nil


)
