; once.arc

(unless global!once-has-been-loaded* (tldo


(= once-has-been-loaded* t)


(= onces* '(()))
(= once-at-a-times* '(()))

(mac once-at-a-time (id . body)
  `(fn-once-at-a-time ,id (fn () ,@body)))

(def fn-once-at-a-time (id f)
  (let test [is id _]
    (when (some test car.once-at-a-times*)
      (err:+ "Circular dependency encountered among code which is "
             "only supposed to run once at a time: "
             car.once-at-a-times*))
    (after
      (f)
      (zap [rem test _] car.once-at-a-times*))))

(mac once (id . body)
  `(fn-once ,id (fn () ,@body)))

(def fn-once (id f)
  (unless (some [iso id _] car.onces*)
    (once-at-a-time id
      (f))
    (push id car.onces*)))

(mac once-tl (id . body)
  `(once ,id (tldo ,@body)))

(def niceuniq (name)
  (sym:string (uniq) "-" name))

(mac w/niceuniq (syms . body)
  (if acons.syms
    `(with ,(mappend [do `(,_ (niceuniq ',_))] syms) ,@body)
    `(let ,syms (niceuniq ',syms) ,@body)))


))  ; end (unless global!once-has-been-loaded* (tldo ...))

; In Rainbow, comments must end with newlines, not EOF, so keep a
; newline here.
