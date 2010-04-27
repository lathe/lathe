; utils.arc
;
; Miscellaneous utilities.
;
; The 'xloop, 'ret, and 'between tools here were inspired by those
; defined by Andrew Wilcox at http://awwx.ws/xloop0,
; http://awwx.ws/ret0, and http://awwx.ws/between0. The only
; noticeable difference is that this 'xloop requires fewer parentheses
; in the majority of practical cases, thanks to 'parse-magic-withlike
; (which is defined in modules/modulemisc.arc).

(packed


(mac my.xloop withbody
  ; NOTE: Jarc doesn't support (a . b) destructuring.
  (withs (binds-and-body parse-magic-withlike.withbody
          binds car.binds-and-body
          body cdr.binds-and-body)
    `((rfn next ,(map car binds)
        ,@body)
      ,@(map cadr binds))))

(mac my.ret (var val . body)
  (w/niceuniq g-result
    `(withs (,g-result ,val ,var ,g-result)
       ,@body
       ,g-result)))

(mac my.between (var val chorus . body)
  (w/niceuniq g-started
    `(with g-started nil
       (each ,var ,val
         (if ,g-started ,chorus (= ,g-started t))
         ,@body))))

(mac my.w/ withbody
  ; NOTE: Jarc doesn't support (a . b) destructuring.
  (withs (binds-and-body parse-magic-withlike.withbody
          binds car.binds-and-body
          body cdr.binds-and-body)
    `(withs ,(apply join binds) ,@body)))

(def my.alcons (al key val)
  `((,key ,val) ,@(rem [is car._ key] al)))

(def my.foldl (func start lst)
  ; NOTE: Jarc doesn't support (a . b) destructuring.
  (if lst
    (with (a car.lst b cdr.lst)
      (my.foldl func (do.func start a) b))
    start))

(def my.foldr (func end lst)
  (my.foldl (fn (a b) (do.func b a)) end rev.lst))

(mac my.foldlet (startvar start nextvar lst . body)
  `(,(my 'foldl) (fn (,startvar ,nextvar) ,@body) ,start ,lst))

(def my.tab+ args
  (w/table t
    (each arg args
      (each (k v) tablist.arg  ; tablist necessary for Jarc
        (= do.t.k v)))))

; This is a version of 'whilet that supports destructuring.
(mac my.dstwhilet (var val . body)
  (w/uniq g-var
    `(whilet ,g-var ,val
       (let ,var ,g-var
         ,@body))))


)