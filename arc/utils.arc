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


(=fn my.niceuniq (name)
  (sym:string (uniq) '- name))

(=mc my.w/niceuniq (syms . body)
  (if alist.syms
    `(with ,(mappend [do `(,_ (,my!niceuniq ',_))] syms) ,@body)
    `(let ,syms (,my!niceuniq ',syms) ,@body)))

(=mc my.xloop withbody
  (let (binds . body) parse-magic-withlike.withbody
    `((rfn next ,(map car binds)
        ,@body)
      ,@(map cadr binds))))

(=mc my.ret (var val . body)
  (if anormalsym.var
    
    ; We will allow for (= the-var changed-result).
    `(let ,var ,val
       ,@body
       ,var)
    
    ; We will allow for destructuring, but the result will stay
    ; constant.
    (my:w/niceuniq g-result
      `(withs (,g-result ,val ,var ,g-result)
         ,@body
         ,g-result))
    ))

(=mc my.between (var val chorus . body)
  (my:w/niceuniq g-started
    `(with g-started nil
       (each ,var ,val
         (if ,g-started ,chorus (= ,g-started t))
         ,@body))))

(=fn my.readwine ((o stream (stdin)))
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

(=mc my.w/ withbody
  (let (binds . body) parse-magic-withlike.withbody
    `(withs ,(apply join binds) ,@body)))

(=fn my.tails (lst)
  (accum acc
    (while acons.lst
      do.acc.lst
      (zap cdr lst))
    do.acc.lst))

(=fn my.alcons (al key val)
  `((,key ,val) ,@(rem [is car._ key] al)))

(=fn my.foldl (func start lst)
  (iflet (a . b) lst
    (my.foldl func (do.func start a) b)
    start))

(=fn my.foldr (func end lst)
  (my.foldl (fn (a b) (do.func b a)) end rev.lst))

(=mc my.foldlet (startvar start nextvar lst . body)
  `(,my!foldl (fn (,startvar ,nextvar) ,@body) ,start ,lst))

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
      (each (k v) arg
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
    (let (compare . actual-tabs) tabs
      (unless (isa compare 'fn)
        (= compare iso actual-tabs tabs))
      (catch:w/table result
        (each tab actual-tabs
          (each (k v) tab
            (iflet existing-v do.result.k
              (unless (do.compare existing-v v)
                throw.nil)
              (= .k.result v))))))))

(=fn my.unnesttab (key val2tab tab)
  (iflet val do.tab.key
    (cons (copy tab key nil) (my.unnesttab key do.val2tab.val))
    list.tab))

(=fn my.nesttab (key tab2val tabs)
  (whenlet (last . rest) rev.tabs
    (my:foldlet result last
                next rest
      (copy next key do.tab2val.tabs))))


(=fn my.== args
  (~whenlet (first . rest) args
    (some [or (< first _) (< _ first)] rest)))

(=fn my.an-int (x)
  (case type.x
    int  t
    num  (my.== x trunc.x)))


; These 'proper and 'split-end utilities were originally posted at
; http://arclanguage.org/item?id=11839.

; For proper lists, this returns a nil-terminated copy of the list.
; For improper lists, this returns a nil-terminated list whose last
; element is the value that terminates the improper list.
(=fn my.proper (lst)
  (accum acc
    (while acons.lst
      (do.acc pop.lst))
    only.acc.lst))

; This returns a two-element list containing a list of all the cars of
; the list and the final cdr of the list. For instance,
; (iso (split-end '(a b . c)) '((a b) c)). For proper lists, it's the
; same as (split lst len.lst).
(=fn my.split-end (lst)
  (let onset (accum acc
               (while acons.lst
                 (do.acc pop.lst)))
    (list onset lst)))

(=fn my.rev-improper (onset end)
  (my:xloop result end onset onset
    (iflet (last . nextonset) (check acons rev-onset)
      (do.next (cons first result) nextonset)
      result)))

(=fn my.join-end (onset end)
  (my.rev-improper rev.onset end))

(=fn my.numcars (lst)
  (summing acc
    (reclist [let at atom._ (do.acc no.at) at] lst)))


; These 'parse-lets and 'lets utilities were originally posted at
; http://arclanguage.org/item?id=11944 under the single name 'scope.

; This returns a cons cell where the car is a list of bindings and the
; cdr is either nil or a single-element list containing a final
; element. The bindings list is represented as an even-length list
; where each key is guaranteed to be a non-ssyntax symbol, possibly
; nil.
;
; The bindings list is drained from the body. If at some point the
; body's remainder contains a single element, that element is the
; final element. If at any other point the start of the body is nil or
; isn't a valid key (thanks to being a non-symbol or an ssyntax
; symbol), it's used as the value of the binding instead, and 'nil is
; used as the key.
;
; The point of this is to help parse a form body where it makes sense
; to have bindings at arbitrary places in the body, even after other
; calculations without explicit result bindings have taken place.
;
(=fn my.parse-lets (body)
  (withs (rev-bindings nil
          final nil
          acc [push _ rev-bindings])
    (while body
      (let var pop.body
        (if no.body
          (= final list.var)
            anormalsym.var
          (do do.acc.var (do.acc pop.body))
          (do do.acc.nil do.acc.var))))
    (cons rev.rev-bindings final)))

; This uses 'parse-lets to create a form that acts like a 'do form but
; allows for simple let bindings to happen within the form.
(=mc my.lets body
  `(withs ,@my.parse-lets.body))

; This uses 'parse-lets to create a form that acts like 'aand but
; allows a name other than 'it to be used.
;
; As a consequence of this, (andlets x y) will treat x as a name and y
; as the value to bind to that name, unlike (aand x y), which will
; treat both x and y as values. To get the (aand x y) behavior, it may
; be necessary to explicitly specify a variable name, as in
; (andlets it x y) or (andlets _ x y).
(=mc my.andlets body
  (let (bindings . final) my.parse-lets.body
    (zap [rev:map [copy _ 0 (or _.0 'it)] pair._] bindings)
    (when final (push (list nil final) bindings))
    (iflet (last . rest) bindings
      (my:foldlet result do.last.1
                  (key value) rest
        `(let ,key ,value (if ,key ,result)))
      't)))


(=fn my.soft-deglobalize-var (var)
  (zap expand var)
  (if anormalsym.var
    var
    
    ; else recognize anything of the form (global 'the-var)
    (catch:my:lets
      (unless (caris var 'global) throw.nil)
      cdr-var     cdr.var
      (unless single.cdr-var throw.nil)
      cadr-var    car.cdr-var
      (unless (caris cadr-var 'quote) throw.nil)
      cdadr-var   cdr.cadr-var
      (unless single.cdadr-var throw.nil)
      cadadr-var  car.cdadr-var
      (unless anormalsym.cadadr-var throw.nil)
      cadadr-var)
    ))

(=fn my.deglobalize-var (var)
  (or my.soft-deglobalize-var.var
      (err:+ "An unrecognized kind of name was passed to "
             "'deglobalize-var.")))


)