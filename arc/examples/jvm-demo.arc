; jvm-demo.arc

(prn "starting jvm-demo")


(nspaced:using-rels-as jv     "../imp/jvm.arc"
; ...


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

(=mc my.test-jiso (simple complicated)
  `(,my!test-similar ,jv!jiso ,simple ,complicated))


(iflet jvm jv.jvm
; ...


(withs (ju jvm!java-util
        Rect jvm!java-awt-Rectangle
        rect (Rect!new 1 2 3 4))
  
  (my:test-iso 1 Rect!OUT_LEFT)
  (my:test-iso 2 (jvm!get (ju!ArrayList-new '(1 2 3)) 1))
  (my:test-iso 3 (jvm!getValue:ju!AbstractMap-SimpleEntry-new 2 3))
  (my:test-iso 4 jvm!java-lang-Integer-valueOf.4)
  
  (my:test-jiso rect (Rect!new 1 2 3 4))
  (my:test-iso 1 jvm!OUT_LEFT.rect)
  
  
  ; Now for some side effects.
  
  (my:test-iso 5 (= (jv:jget rect 'x) (+ 1 (jv.jget rect 'height))))
  
  (my:test-iso 6
    (jv.jset rect 'y (+ 1 (= (jv:jget rect 'x)
                             (+ 1 (jv.jget rect 'height))))))
  
  (my:test-iso 7 (jvm!width= rect (+ 1 jvm!y.rect)))
  
  (my:test-iso nil (jvm!add rect 5 14))
  
  
  (my:test-jiso rect (Rect!new 5 6 7 8))
  )


; else if no jv.jvm

(do
  
  ; We don't have anything really interesting to test here, but we're
  ; testing it anyway!
  (my:test-iso nil jv.jclass!java-lang-Object)
  (my:test-iso t (jv.jiso))
  )


)  ; end else clause of (iflet jvm jv.jvm ...)


(if my.tests-succeeded (prn "All tests succeeded!"))

(prn "finishing jvm-demo")

nil


)  ; end (nspaced:using-rels-as ...)

; NOTE: This comment ends in a newline for Rainbow's sake.
