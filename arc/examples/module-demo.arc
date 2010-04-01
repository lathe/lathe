; module-demo.arc

(prn "starting module-demo")

(using-rels "../amb.arc"
  (prn "starting using-rels block")
  (if (and bound!make-amb (isa make-amb 'fn))
    (prn "yay, make-amb was loaded")
    (prn "whoops, make-amb wasn't loaded")))

(if (and bound!make-amb no.make-amb)
  (prn "yay, make-amb was unloaded")
  (prn "whoops, make-amb wasn't unloaded"))

(prn "finishing module-demo")