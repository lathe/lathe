; nspaced.arc
;
; This is a utility to help with making more hygienic Arc code. Within
; an (nspaced ...) form, forms escaped using the anaphoric 'local
; macro will be mangled so that it's more difficult for naming clashes
; to break them. The macro (copy-to-local a b c) is provided as syntax
; sugar for (= local.a a local.b b local.c c), which makes it function
; a lot like importing the global bindings into the local context. The
; local names are essentially protected from modifications to those
; global bindings.
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
; any namespace is created, those names will be used verbatim rather
; than replaced with gensyms.

(once-tl "load nspaced.arc"


(= hackable-names* '())

; A namespace macro, such as the 'local macro inside an nspaced form,
; works in three ways:
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
; One downside of these macros is that they can naturally only be used
; in places macros are expanded. That means that
; (assign local.foo bar) doesn't work, and as such, anything which
; ultimately expands into such a form without doing its own
; macro-expansion won't work either. For that reason, mac, def, and
; safeset are redefined below so that each one macro-expands the name
; given to it.
;
(def nspace ((o backing-table (table)))
  (let symfor [or ._.backing-table (= ._.backing-table niceuniq._)]
    (each name hackable-names*
      (= .name.backing-table name))
    (mc (what)
      (case type.what
        sym   symfor.what
        cons  (let (op . params) what
                (unless (isa op 'sym)
                  (err:+ "A cons expression with a non-symbol car "
                         "was passed to a namespace."))
                (case op quote
                  (let (symwhat) params
                    (unless (isa symwhat 'sym)
                      (err:+ "A (quote foo) expression with a "
                             "non-symbol foo was passed to a "
                             "namespace."))
                    `',symfor.symwhat)
                  `(,symfor.op ,@params)))
              (err:+ "A non-symbol, non-cons expression was passed "
                     "to a namespace.")))))

(mac nspaced body
  `(w/global local (nspace)
     (tldo ,@body)))

(mac copy-to-local whats
  (each what whats
    (unless (and what (isa what 'sym) (~ssyntax what))
      (err:+ "A non-symbol or special symbol expression was passed "
             "to copy-to-local.")))
  `(= ,@(mappend [do `((local ,_) ,_)] whats)))

(mac copy-to-nspace (ns . whats)
  (each what whats
    (unless (and what (isa what 'sym) (~ssyntax what))
      (err:+ "A non-symbol or special symbol expression was passed "
             "to copy-to-nspace.")))
  (w/uniq g-ns
    `(= ,g-ns ,ns
        ,@(mappend [do `((,g-ns ,_) ,_)] whats)
        ,g-ns nil)))

; This is a spoof form that imitates an nspaced form without actually
; protecting any variables. The point is that if you really don't like
; the effect nspaced has on Arc code, then you can drop in this
; non-implementation, maybe even saying (= nspaced not-nspaced) at the
; top level, and thereby use most code that's targeted at nspaced in a
; way that's closer to what you want without very much hassle.
(mac not-nspaced body
  `(w/global local (mc (what) what)
     (tldo ,@body)))

; Redefine def, mac, and safeset so that (def local.foo ...),
; (mac local.foo ...), (defmemo local.foo ...), and
; (defcache local.foo ...) can be used.
(nspaced
  (copy-to-local def mac safeset)
  (=mc def (name . rest)
    `(,local!def ,expand.name ,@rest))
  (=mc mac (name . rest)
    `(,local!mac ,expand.name ,@rest))
  (=mc safeset (var val)
    `(,local!safeset ,expand.var ,val)))


)