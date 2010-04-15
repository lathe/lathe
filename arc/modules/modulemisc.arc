; modulemisc.arc

(unless (and bound!modulemisc-has-been-loaded*
             eval!modulemisc-has-been-loaded*)


(= modulemisc-has-been-loaded* t)


; This will evaluate its body, one line at a time, in the top-level
; context. This lets the code modify global bindings even if a lexical
; scope would shadow them. Furthermore, the lexical scope will be
; totally inaccessible.
(mac tldo body
  `(do ,@(map [do `(eval ',_)] body)))


(eval '(tldo


(mac mc (parms . body)
  `(annotate 'mac (fn ,parms ,@body)))

(mac =mc (name parms . body)
  `(= ,name (mc ,parms ,@body)))

(mac =fn (name parms . body)
  `(= ,name (fn ,parms ,@body)))

; Expand both ssyntax and macros until neither is left.
(def expand (expr)
  (let nextexpr macex.expr
    (if ssyntax.nextexpr
      (expand:ssexpand nextexpr)
      nextexpr)))

; A (call a b c) form should act the same way as a plain (a b c) form,
; *except* when a is a symbol globally bound to a macro at the time
; the expression is compiled, in which case (call a b c) will
; effectively suppress that macro expansion.
(def call (f . args)
  (apply f args))

(def anormalsym (x)
  (and x (isa x 'sym) (~ssyntax x)))

; Jarc defines 'andf the way Arc 2 does, so that it returns t on
; success rather than the last-returned true value. Here's an
; alternative that does 'andf the Arc 3 way (except that it isn't a
; metafn and it doesn't have a dedicated ssyntax).
(def doandf args
  (case args nil
    idfn
    ; NOTE: Jarc doesn't support (a . b) destructuring.
    (withs (rev-args rev.args last car.rev-args rev-rest cdr.rev-args)
      (case rev-rest nil
        last
        (let restfn (apply andf rev.rev-rest)
          [and do.restfn._ do.last._])))))

; This will transform a list of parameters from
; ((var1 val1 var2 val2) body1 body2) format--as seen in Arc's
; 'with--into a Scheme- or CL-style
; (((var1 val1) (var2 val2)) body1 body2) format. If the pairerr
; argument is provided, that error will be raised if the binding list
; has an odd length.
;
; Furthermore, if the first parameter is *not* a list, this will
; magically find as many bindings from the beginning of the parameter
; list as it can. The only bindings that can be found this way are
; those whose names are non-ssyntax symbols, including the non-ssyntax
; symbol 'nil. If there's an odd number of parameters, the last
; parameter will not be put into a binding, since there's no
; expression to bind it with; instead, it will be part of the body.
;
; The restriction on "magic" binding names means that destructuring
; (which Arc's 'let supports) and setforms (which Arc's '= supports)
; are left out. However, a macro which uses destructuring or setforms
; can still take advantage of parse-magic-withlike, since whenever the
; user of the macro needs those features, he or she can just use
; with-style parentheses.
;
(def parse-magic-withlike (arglist (o pairerr))
  (if no.arglist
    '(())
    ; NOTE: Jarc doesn't support (a . b) destructuring.
    (with (first car.arglist rest cdr.arglist)
      (if alist.first
        (if (and pairerr (odd:len first))
          err.pairerr
          (cons pair.first rest))
        (let withlist (accum acc
                        (while (and cdr.arglist
                                    ((orf no anormalsym) car.arglist))
                          (withs (name pop.arglist val pop.arglist)
                            (do.acc (list name val)))))
          (cons withlist arglist))))))

(def global (name)
  (unless anormalsym.name
    (err "A nil, ssyntax, or non-symbol name was given to 'global."))
  ((doandf bound eval) name))

(defset global (name)
  (w/uniq (g-name g-val)
    `(((,g-name ,g-val) (let _ ,name (list _ global._)))
      ,g-val
      ; NOTE: Jarc doesn't support nested quasiquotes the same way.
      [eval (list '= ,g-name (list 'quote _))])))

; The above (defset global ...) relies on being able to say
; (eval `(= global-var ',local-expression) even when local-expression
; has a value that can't usually appear in syntax. In case that
; doesn't work on all Arc implementations, here's the previous version
; of the (defset global ...) that uses a temporary global variable
; instead.
;
;(w/uniq g-temp
;  (eval `(defset global (name)
;           (w/uniq (g-name g-val)
;             `(((,g-name ,g-val) (let _ ,name (list _ global._)))
;               ,g-val
;               [do (= ,',g-temp _)
;                   (eval `(= ,,g-name ,',',g-temp))
;                   (= ,',g-temp nil)])))))

; Set a global variable temporarily. This is neither thread-safe nor
; continuation-safe, although it will restore the original value of
; the variable upon abnormal exits (as well as normal ones).
(mac w/global (name val . body)
  (zap expand name)
  (w/uniq g-old-val
    `(let ,g-old-val (global ',name)
       (after
         (do (= (global ',name) ,val)
             ,@body)
         (= (global ',name) ,g-old-val)))))

; Jarc uses lexical scoping for macros, which renders pointless most
; of the global name shuffling this module system is based on.
;
; Note that the name 'call is being used here, as opposed to something
; more descriptive, just so that we don't end up making a pointless
; global binding to nil.
;
(w/global call (mc () nil)  ; NOTE: Jarc doesn't support (fn ()).
  (tldo (let call (mc () t)
          (= lexical-macros* (call)))))

(if lexical-macros*
  (do (mac w/mac (name val . body) `(let ,name ,val ,@body))
      (mac mcdo body `(do ,@body)))
  (do (mac w/mac (name val . body) `(w/global ,name ,val ,@body))
      (mac mcdo body `(tldo ,@body))))

; This is like 'load, but it returns the result of the final
; expression.
(def loadval (file)
  (with (stream infile.file eof (uniq))
    (let result nil
      (whiler expr (read stream eof) eof
        (= result eval.expr))
      result)))

; Jarc uses 'set, and other implementations use 'assign now (with 'set
; meaning the opposite of 'wipe), so here's a shot at a cross-platform
; synonym. Note that (set xassign 0) raises an error on 'assign
; platforms when 'set is unbound or when it attempts to get the
; setforms for 0 during macro expansion.
(if (errsafe (eval '(do (set xassign 0) t)))
  (=mc xassign args `(set ,@args))
  (=mc xassign args `(assign ,@args)))

; Change 'setforms so that when a place becomes macro-expanded into an
; unbound (lexically and globally) symbol, there isn't an error.
; Instead, a global variable is created, as is the usual behavior of =
; for unbound symbols.
;
; The simple way to assure this is to have the setforms be:
;
;  `(()                      ; atwith bindings
;    ,expansion              ; getter expression
;    [assign ,expansion _])  ; setter function expression
;
; However, that breaks functions that swap or rotate places, since one
; place has its setter executed before its getter. For this reason (I
; presume), the current arc.arc behavior of setforms is equivalent to:
;
;  (w/uniq g-place
;    `((,g-place ,expansion)
;      ,expansion
;      [assign ,expansion _]))
;
; As mentioned, this causes there to be an error when the variable is
; being set for the first time. The solution actually used here is to
; wrap ,expansion in (errsafe ...), thereby suppressing the error.
;
; This has the side effect that swapping or rotating places, one of
; which macro-expands to an unbound variable name, will have behavior
; as though the unbound variable had actually been bound to nil (which
; is the exceptional result of errsafe).
;
(let old-setforms setforms
  (=fn setforms (expr)
    (let expansion macex.expr
      (if anormalsym.expansion
        (w/uniq g-place
          `((,g-place (errsafe ,expansion))
            ,g-place
            [xassign ,expansion _]))
        do.old-setforms.expansion))))

; Jarc doesn't define 'get, so here's most of it. Unfortunately, it's
; actually a metafn, so this doesn't cover everything.
(unless bound!get
  
  (def get (val)
    [_ val])
  
  (let old-setforms setforms
    (=fn setforms (expr)
      (let expansion macex.expr
        (if (and acons.expansion
                 (acons car.expansion)
                 (is caar.expansion 'get))
          (setforms `(,cadr.expansion ,(cadr:car expansion)))
          old-setforms.expansion))))
  )

(defset global (name)
  (w/uniq (g-name g-val)
    `(((,g-name ,g-val) (let _ ,name (list _ global._)))
      ,g-val
      ; NOTE: Jarc doesn't support nested quasiquotes the same way.
      [eval (list '= ,g-name (list 'quote _))])))


))  ; end (eval '(tldo ...))

; end (unless (and bound!modulemisc-has-been-loaded*
;                  eval!modulemisc-has-been-loaded*)
)