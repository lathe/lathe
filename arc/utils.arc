; utils.arc
;
; Miscellaneous utilities.
;
; The 'xloop, 'ret, 'between, and 'readwine tools here were inspired
; by those defined by Andrew Wilcox at http://awwx.ws/xloop0,
; http://awwx.ws/ret0, http://awwx.ws/between0, and
; http://awwx.ws/readline1. The only noticeable difference is that
; this 'xloop requires fewer parentheses in the majority of practical
; cases, thanks to 'parse-magic-withlike (which is defined in
; modules/modulemisc.arc).

(packed


(=mc my.xloop withbody
  ; NOTE: Jarc doesn't support (a . b) destructuring.
  (withs (binds-and-body parse-magic-withlike.withbody
          binds car.binds-and-body
          body cdr.binds-and-body)
    `((rfn next ,(map car binds)
        ,@body)
      ,@(map cadr binds))))

(=mc my.ret (var val . body)
  (w/niceuniq g-result
    `(withs (,g-result ,val ,var ,g-result)
       ,@body
       ,g-result)))

(=mc my.between (var val chorus . body)
  (w/niceuniq g-started
    `(with g-started nil
       (each ,var ,val
         (if ,g-started ,chorus (= ,g-started t))
         ,@body))))

(=fn my.readwine ((o stream (stdin)))
  (whenlet firstchar readc.stream
    (string (acm
              (my (xloop chr firstchar
                    (case chr
                      #\newline  nil
                      #\return   (case peekc.stream #\newline
                                   readc.stream)
                      nil        nil
                                 (do do.acc.chr
                                     (do.next readc.stream)))))))))

(=mc my.w/ withbody
  ; NOTE: Jarc doesn't support (a . b) destructuring.
  (withs (binds-and-body parse-magic-withlike.withbody
          binds car.binds-and-body
          body cdr.binds-and-body)
    `(withs ,(apply join binds) ,@body)))

(=fn my.tails (lst)
  (acm
    (while acons.lst
      do.acc.lst
      (zap cdr lst))
    do.acc.lst))

(=fn my.alcons (al key val)
  `((,key ,val) ,@(rem [is car._ key] al)))

(=fn my.foldl (func start lst)
  ; NOTE: Jarc doesn't support (a . b) destructuring.
  (if lst
    (with (a car.lst b cdr.lst)
      (my.foldl func (do.func start a) b))
    start))

(=fn my.foldr (func end lst)
  (my.foldl (fn (a b) (do.func b a)) end rev.lst))

(=mc my.foldlet (startvar start nextvar lst . body)
  (cons my!foldl `((fn (,startvar ,nextvar) ,@body) ,start ,lst)))

(=mc my.maplet (var lst . body)
  `(map (fn (,var) ,@body) ,lst))

(=mc my.mappendlet (var lst . body)
  `(mappend (fn (,var) ,@body) ,lst))

(=mc my.zapmappendlet (var lst . body)
  (w/uniq g-lst
    `(zap (fn (,g-lst) (mappend (fn (,var) ,@body) ,g-lst)) ,lst)))

(=fn my.tab+ args
  (w/table t
    (each arg args
      (each (k v) tablist.arg  ; tablist necessary for Jarc
        (= do.t.k v)))))

; This is a version of 'whilet that supports destructuring.
(=mc my.dstwhilet (var val . body)
  (w/uniq g-var
    `(whilet ,g-var ,val
       (let ,var ,g-var
         ,@body))))

; This takes a bunch of tables and returns a new table that's an
; extension of all of them. If the first parameter is a function, that
; function is used in order to decide whether two values for the same
; key are equivalent. Otherwise, 'iso is used. If any two values for
; the same key are not equivalent, nil is returned instead of a table.
; If they are equivalent, the one from the first table in the argument
; list is used.
(=fn my.mergetabs tabs
  (case tabs nil
    (table)
    ; NOTE: Jarc doesn't support (a . b) destructuring.
    (with (compare car.tabs actual-tabs cdr.tabs)
      (unless (isa compare 'fn)
        (= compare iso actual-tabs tabs))
      (catch (w/table result
               (each tab actual-tabs
                 (each (k v) tab
                   (iflet existing-v do.result.k
                     (unless (do.compare existing-v v)
                       (throw nil))
                     (= .k.result v)))))))))

(=fn my.unnesttab (key val2tab tab)
  (iflet val do.tab.key
    (cons (copy tab key nil) (my.unnesttab key do.val2tab.val))
    list.tab))

(=fn my.nesttab (key tab2val tabs)
  (whenlet last-and-rest rev.tabs
    ; NOTE: Jarc doesn't support (a . b) destructuring.
    (with (last car.last-and-rest rest cdr.last-and-rest)
      (my:foldlet result last
                  next rest
        (copy next key do.tab2val.tabs)))))


)