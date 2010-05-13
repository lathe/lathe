; utils.arc
;
; Miscellaneous utilities.
;
; The 'xloop, 'ret, 'between, and 'readwine tools here were inspired
; by those defined by Andrew Wilcox at http://awwx.ws/xloop0,
; http://awwx.ws/ret0, http://awwx.ws/between0, and
; http://awwx.ws/readline1. The only noticeable differences are as
; follows:
;
; - When this 'ret binds a non-nil, non-ssyntax symbol, any
;   assignments to the bound variable from within the body will modify
;   the result. This behavior is consistent with Sean Kenney's
;   original 'ret, which Andrew Wilcox cites. When destructuring is
;   involved, as in (ret (a b) some-pair ...), assignments do not
;   modify the result, which is consistent with Andrew Wilcox's 'ret.
;
; - This 'xloop requires fewer parentheses in the majority of
;   practical cases, thanks to 'parse-magic-withlike (which is defined
;   in modules/modulemisc.arc).

(packed


(mac my.xloop withbody
  (let (binds . body) parse-magic-withlike.withbody
    `((rfn next ,(map car binds)
        ,@body)
      ,@(map cadr binds))))

(mac my.ret (var val . body)
  (if anormalsym.var
    
    ; We will allow for (= the-var changed-result).
    `(let ,var ,val
       ,@body
       ,var)
    
    ; We will allow for destructuring, but the result will stay
    ; constant.
    (w/niceuniq g-result
      `(withs (,g-result ,val ,var ,g-result)
         ,@body
         ,g-result))))

(mac my.between (var val chorus . body)
  (w/niceuniq g-started
    `(with g-started nil
       (each ,var ,val
         (if ,g-started ,chorus (= ,g-started t))
         ,@body))))

(def my.readwine ((o stream (stdin)))
  (whenlet firstchar readc.stream
    (string:accum acc
      (my:xloop chr firstchar
        (case chr
          #\newline  nil
          #\return   (case peekc.stream #\newline
                       readc.stream)
          nil        nil
                     (do do.acc.chr
                         (do.next readc.stream)))))))

(mac my.w/ withbody
  (let (binds . body) parse-magic-withlike.withbody
    `(withs ,(apply join binds) ,@body)))

(def my.tails (lst)
  (accum acc
    (while acons.lst
      do.acc.lst
      (zap cdr lst))
    do.acc.lst))

(def my.alcons (al key val)
  `((,key ,val) ,@(rem [is car._ key] al)))

(def my.foldl (func start lst)
  (iflet (a . b) lst
    (my.foldl func (do.func start a) b)
    start))

(def my.foldr (func end lst)
  (my.foldl (fn (a b) (do.func b a)) end rev.lst))

(mac my.foldlet (startvar start nextvar lst . body)
  `(,my!foldl (fn (,startvar ,nextvar) ,@body) ,start ,lst))

(mac my.maplet (var lst . body)
  `(map (fn (,var) ,@body) ,lst))

(mac my.mappendlet (var lst . body)
  `(mappend (fn (,var) ,@body) ,lst))

(mac my.zapmappendlet (var lst . body)
  (w/uniq g-lst
    `(zap (fn (,g-lst) (mappend (fn (,var) ,@body) ,g-lst)) ,lst)))

(def my.tab+ args
  (w/table t
    (each arg args
      (each (k v) arg
        (= do.t.k v)))))

; This is a version of 'whilet that supports destructuring.
(mac my.dstwhilet (var val . body)
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
(def my.mergetabs tabs
  (case tabs nil
    (table)
    (let (compare . actual-tabs) tabs
      (unless (isa compare 'fn)
        (= compare iso actual-tabs tabs))
      (catch:w/table result
        (each tab actual-tabs
          (each (k v) tab
            (iflet existing-v do.result.k
              (unless (do.compare existing-v v)
                (throw nil))
              (= .k.result v))))))))

(def my.unnesttab (key val2tab tab)
  (iflet val do.tab.key
    (cons (copy tab key nil) (my.unnesttab key do.val2tab.val))
    list.tab))

(def my.nesttab (key tab2val tabs)
  (whenlet (last . rest) rev.tabs
    (my:foldlet result last
                next rest
      (copy next key do.tab2val.tabs))))


)