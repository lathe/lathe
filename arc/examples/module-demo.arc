; module-demo.arc

(prn "starting module-demo")

(using-rels-as ms "../more-module-stuff.arc"
  (ms:using-rels "../amb.arc"
    (prn "starting using-rels block")
    (if (and bound!make-amb (isa make-amb 'fn))
      (prn "yay, make-amb was loaded")
      (prn "whoops, make-amb wasn't loaded"))))

(if (and bound!make-amb no.make-amb)
  (prn "yay, make-amb was unloaded")
  (prn "whoops, make-amb wasn't unloaded"))

(if (is (prepare '(rel "../amb.arc"))
        (prepare '(rel "../amb.arc")))
  (prn "yay, dependencies are prepared only once each")
  (prn "whoops, dependencies are prepared more than once each"))

(prn "finishing module-demo")

nil