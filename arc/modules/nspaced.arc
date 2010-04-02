; nspaced.arc
;
; This is a utility to help with making more hygienic Arc code. Within
; an (nspaced ...) form, forms escaped using the anaphoric 'local
; macro will be mangled so that it's more difficult for naming clashes
; to break them. Meanwhile, the anaphoric 'copy-to-local macro will
; create duplicate, mangled versions of externally defined global
; bindings, so that those values can continue to be used as-is even if
; later code changes what values are bound to the regular names.
;
; The main point of focusing on global variables is that ac (the Arc
; compiler) compiles one top-level expression (or expression sent to
; eval) at a time, and it expands a macro form in that expression only
; if the expression in function position is a symbol *globally* bound
; to a macro.
;
; In case there are times it's useful to redefine something in such a
; way that it changes the behavior of another function or macro
; protected by an nspaced form, there is an empty global list,
; hackable-names*, which can be extended with symbol names so that as
; any nspaced form is compiled, those names will be used verbatim
; rather than replaced with gensyms. The value of hackable-names* is
; checked in the lexical environment of the nspaced form, so a simple
; (let hackable-names* '(this that those) (nspaced ...)) will do.

(once-tl "load nspaced.arc"


(= hackable-names* '())

; The (local ...) macro works in three ways:
;
;  - You can pass it a symbol, in which case it will mangle that
;    symbol. This is the usual case.
;
;  - You can pass it a quoted symbol, in which case it will mangle
;    that symbol but yield a quoted version. This is useful when
;    generating code, since you need to know the *names* for
;    functions, macros, tables, etc. used in the generated code, not
;    just the values themselves.
;
;  - You can pass it a cons cell where the car is a symbol other than
;    'quote, in which case it will yield a cons cell with a mangled
;    car and the original cdr. This is useful when invoking a macro,
;    by way of the syntax (local:my-macro params). Unfortunately,
;    (local.my-macro params) doesn't work, since ac doesn't
;    macro-expand (local my-macro) until after it's determined that
;    the expression (local my-macro) isn't a symbol globally bound
;    to a macro.
;
; One downside of the 'local macro is that it can only be used in
; places macros are expanded. That means that (assign local.foo bar)
; doesn't work, and as such, anything which ultimately expands into
; such a form without doing its own macro-expansion won't work either.
; For that reason, mac, def, and safeset are redefined below so that
; each one macro-expands the name given to it.
;
(mac nspaced body
  (w/uniq (g-syms g-old-local g-old-save)
    `(after
       (do
         (= ,g-syms (table)
            ,g-old-local (bound&eval 'local)
            ,g-old-save (bound&eval 'save-to-local))
         ; Use the lexical binding of hackable-names*.
         (each name hackable-names*
           (= (,g-syms name) name))
;         ; Alternatively, we could use the global binding.
;         (= ,@(mappend [do `((,g-syms ',_) ',_)] hackable-names*))
         (tldo:let symfor [or (,g-syms _) (= (,g-syms _) niceuniq._)]
           (=mc local (what)
             (case type.what
               sym   symfor.what
               cons  (let (op . params) what
                       (unless (isa op 'sym)
                         (err:+ "A cons expression with a non-symbol "
                                "car was passed to local."))
                       (case op quote
                         (let (symwhat) params
                           (unless (isa symwhat 'sym)
                             (err:+ "A (quote foo) expression with a "
                                    "non-symbol foo was passed to "
                                    "local."))
                           `',symfor.symwhat)
                         `(,symfor.op ,@params)))
                     (err:+ "A non-symbol, non-cons expression was "
                            "passed to local.")))
           (=mc copy-to-local whats
             (each what whats
               (unless (isa what 'sym)
                 (err:+ "A non-symbol expression was passed to "
                        "copy-to-local."))
               (let result symfor.what
                 (when (and (~bound result) bound.what)
                   (eval `(= ,result ,what)))))
             nil))
         (tldo ,@body))
       (tldo:= local ,g-old-local
               save-to-local ,g-old-save)
       (wipe ,g-syms ,g-old-local ,g-old-save))))

; This is a spoof form that imitates an nspaced form without actually
; protecting any variables. The point is that if you really don't like
; the effect nspaced has on Arc code, then you can drop in this
; non-implementation, maybe even saying (= nspaced not-nspaced) at the
; top level, and thereby use most code that's targeted at nspaced in a
; way that's closer to what you want without very much hassle.
(mac not-nspaced body
  (w/uniq (g-old-local g-old-save)
    `(after
       (do
         (= ,g-old-local (bound&eval 'local)
            ,g-old-save (bound&eval 'save-to-local))
         (tldo
           (= local (mc (what) what)
              save-to-local (mc _))
           ,@body))
       (tldo:= local ,g-old-local
               save-to-local ,g-old-save)
       (wipe ,g-old-local ,g-old-save))))

; Redefine def, mac, and safeset so that (def local.foo ...),
; (mac local.foo ...), (defmemo local.foo ...), and
; (defcache local.foo ...) can be used.
(nspaced
  (copy-to-local =mc def mac safeset expand)
  (local:=mc def (name . rest)
    `(,local!def ,(local:expand name) ,@rest))
  (local:=mc mac (name . rest)
    `(,local!mac ,(local:expand name) ,@rest))
  (local:=mc safeset (var val)
    `(,local!safeset ,(local:expand var) ,val)))


)