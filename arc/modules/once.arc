; once.arc


(= once-at-a-times* '(()))

(mac once-at-a-time (id . body)
  `(fn-once-at-a-time ,id (fn () ,@body)))

(def fn-once-at-a-time (id body)
  (let test [is id _]
    (when (some test car.once-at-a-times*)
      (err:+ "Circular dependency encountered among code which is "
             "only supposed to run once at a time: "
             car.once-at-a-times*))
    (after
      call.body
      (zap [rem test _] car.once-at-a-times*))))