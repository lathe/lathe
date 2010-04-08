; utils.arc

(packed


(mac my.xloop withbody
  (let (binds . body) parse-magic-withlike.withbody
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
  (let (binds . body) parse-magic-withlike.withbody
    `(withs ,(apply join binds) ,@body)))

(def my.alcons (al key val)
  `((,key ,val) ,@(rem [is car._ key] al)))

(def my.foldl (func start lst)
  (iflet (a . b) lst
    (my.fold func (do.func start a) b)
    start))

(def my.foldr (func end lst)
  (my.foldl (fn (a b) (do.func b a)) end rev.lst))

(mac my.foldlet (startvar start nextvar lst . body)
  `(,my!foldl (fn (,startvar ,nextvar) ,@body) ,start ,lst))


; This is a table mapping global function names to singleton lists
; containing association lists, where each association list maps
; extension labels to extension details, where each extension detail
; is a two-element list containing the condition and the consequence.
; Extension entries earlier in an association list are attempted
; before those later in the association list, and the original
; function is called only if all of those fail.
;
; Note that an extension consequence is not just a function that takes
; arguments and does something with them. It's actually a function
; that takes a single argument (the result of applying the condition)
; and returns another function that takes the arguments and does the
; dirty work. This is ultimately so that, when using 'extend or
; 'label-extend, 'it can be bound in the consequence to the condition
; result in such a way that even defaults of optional arguments in the
; parameters see the new 'it.
;
(= my.extends* (table))

; This takes a global function name and replaces that function with a
; function that dispatches to the original function or one of its
; extensions, whichever is appropriate. This is done automatically the
; first time a function is extended. If the function is totally
; redefined after that, this has to be done manually in order to get
; the extensions to work again.
(def my.enable-extend (name)
  (let old global.name
    (=fn global.name args
      (catch
        (each (label (condition consequence)) (car my.extends*.name)
          (awhen (apply condition args)
            (throw:apply do.consequence.it args)))
        (apply old args))))
  'ok)

(def my.fn-extend (name label condition consequence)
  (let extends my.extends*.name
    (unless extends
      my.enable-extend.name
      ; Rainbow doesn't like "list.nil".
      (= extends (= my.extends*.name (list nil))))
    (zap [my.alcons _ label (list condition consequence)]
         car.extends))
  'ok)

; In the body, 'it is bound to the result of the condition. In fact,
; if one of the defaults of an optional parameter in 'parms depends on
; 'it, the parameter as seen in the body may be different than the one
; visible in the condition. This is also true if 'parms is, for
; instance, (arg1 arg2 (o arg3 (uniq))).
(mac my.label-extend (name label parms condition . body)
  `(,my!fn-extend ',expand.name ',expand.label
     (fn ,parms ,condition)
     (fn (it) (fn ,parms ,@body))))

(mac my.extend (name parms condition . body)
  `(,my!label-extend ,name ,(uniq) ,parms ,condition ,@body))


)