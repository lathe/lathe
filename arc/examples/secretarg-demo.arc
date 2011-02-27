; secretarg-demo.arc

(prn "starting secretarg-demo")

(nspaced:using-rels-as dy "../dyn.arc"


(= my.tests-succeeded t)

(=mc my.test-iso (simple complicated)
  (w/uniq (g-simple g-complicated)
    `(with (,g-simple ,simple ,g-complicated ,complicated)
       (unless (iso ,g-simple ,g-complicated)
         (wipe ,my!tests-succeeded)
         (prn:+ "FAILED: These should be 'iso: " ,g-simple " and "
                ,g-complicated ". The latter's expression was "
                ',complicated ".")))))


; This was originally posted as an example at
; http://arclanguage.org/item?id=13825.
(with (secret1 (dy.secretarg 4)
       secret2 (dy.secretarg 5)
       secret3 (dy.secretarg 6))
  
  (my:test-iso `(1 2 3 9 5 ((,secret1 9) (,secret3 200)))
    (dy:call-w/secrets (dy:secretarg-fn (a b c)
                           d secret1
                           e secret2
                         (list a b c d e (dy.secretargs)))
      (list (list secret1 9)
            (list secret3 200))
      1 2 3))
  )


(if my.tests-succeeded (prn "All tests succeeded!"))

(prn "finishing secretarg-demo")

nil


)
