; nspace.arc
;
; This is a utility to help with making more hygienic Arc code. Within
; an (nspaced ...) form, forms escaped using the anaphoric 'my macro
; will be mangled so that it's more difficult for naming clashes to
; break them. The macro (copy-to-mine a b c) is provided as syntax
; sugar for (= my.a a my.b b my.c c), which makes it function a lot
; like importing the global bindings into the local context. The local
; names are essentially protected from modifications to those global
; bindings.
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


(= hackable-names* '())

; A namespace macro, such as the 'my macro inside an nspaced form,
; works in three ways:
;
;  - You can pass it a symbol, in which case it will mangle that
;    symbol and result in gs1234-the-variable. This is the usual case.
;    At *expansion time*, this will also set that mangled variable to
;    nil if it isn't already bound. The 'global call is necessary
;    because 'setforms will cause an error if its input is ultimately
;    an unbound global variable, and although 'expand= special-cases
;    most unbound global variable assignments, it doesn't expand
;    ssyntax or macros before doing so, and it sends those cases to
;    'setforms to break.
;    NOTE: In the past, this has expanded to
;    (global 'gs1234-the-variable). It was changed for efficiency's
;    sake.
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
;    by way of the syntax (my:the-macro params). Unfortunately,
;    (my.the-macro params) doesn't work, since ac doesn't macro-expand
;    (my the-macro) until after it's determined that the expression
;    (my the-macro) isn't a symbol globally bound to a macro.
;
; One downside of these macros is that they can naturally only be used
; in places macros are expanded. That means that (assign my.foo bar)
; doesn't work, and as such, anything which ultimately expands into
; such a form without doing its own macro-expansion won't work either.
; For that reason, mac, def, and safeset are redefined below so that
; each one macro-expands the name given to it.
;
; NOTE: Rainbow's profiler doesn't like function calls in optional
; arguments.
;
(w/uniq missing
  (def nspace ((o backing-table missing))
    (when (is backing-table missing)
      (= backing-table (table)))
    (nspace-indirect thunk.backing-table)))

(def nspace-indirect (backing-table-getter)
  (withs (prefix (uniq)
          symfor [do (unless anormalsym._
                       (err:+ "A nil, ssyntax, or non-symbol name "
                              "was passed to a namespace."))
                     (or= call.backing-table-getter._
                       (sym:string prefix '- _))])
    (let backing-table call.backing-table-getter
      (each name hackable-names*
        (= .name.backing-table name)))
    (mc (what)
      (if atom.what
        (let global-name do.symfor.what
          (unless bound.global-name
            (wipe global.global-name))
          global-name)
        (let (op . params) what
          (if isa-quote.op
            (let (name . more) params
              (when more
                (err:+ "A (quote ...) expression with more than one "
                       "parameter was passed to a namespace."))
              `',do.symfor.name)
            `(,do.symfor.op ,@params)))))))

(mac nspaced body
  (let name (or (and acons.body cdr.body (check car.body anormalsym))
                'my)
    `(w/global ,name (nspace)
       (tldo ,@body))))
