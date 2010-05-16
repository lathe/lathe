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
;    symbol and wrap it up as (global 'gs1234-the-variable). This is
;    the usual case. The 'global call is necessary because 'setforms
;    will cause an error if its input is ultimately an unbound global
;    variable, and although 'expand= special-cases most unbound global
;    variable assignments, it doesn't expand ssyntax or macros before
;    doing so, and it sends those cases to 'setforms to break.
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
(def nspace ((o backing-table (table)))
  (nspace-indirect (fn () backing-table)))

(def nspace-indirect (backing-table-getter)
  (let symfor [do (unless anormalsym._
                    (err:+ "A nil, ssyntax, or non-symbol name was "
                           "passed to a namespace."))
                  (or= call.backing-table-getter._ niceuniq._)]
    (let backing-table call.backing-table-getter
      (each name hackable-names*
        (= .name.backing-table name)))
    (mc (what)
      (if atom.what
        `(global ',do.symfor.what)
        (let (op . params) what
          (case op quote
            (let (name . more) params
              (when more
                (err:+ "A (quote ...) expression with more than one "
                       "parameter was passed to a namespace."))
              `',do.symfor.name)
            `(,do.symfor.op ,@params)))))))

(def deglobalize-var (var)
  (zap expand var)
  (if anormalsym.var
    var
    
    ; else recognize anything of the form (global 'the-var)
    ;
    ; NOTE: Rainbow doesn't allow us to say (let nil 2 ...) to create
    ; no local variables at all, so we're using a throwaway variable
    ; '_ instead of nil.
    ;
    (withs (require     [unless _
                          (err:+ "An unrecognized kind of name was "
                                 "passed to 'deglobalize-var.")]
            _           (do.require (caris var 'global))
            cdr-var     cdr.var
            _           (do.require single.cdr-var)
            cadr-var    car.cdr-var
            _           (do.require (caris cadr-var 'quote))
            cdadr-var   cdr.cadr-var
            _           (do.require single.cdadr-var)
            cadadr-var  car.cdadr-var
            _           (do.require anormalsym.cadadr-var))
      cadadr-var)))

(mac nspaced body
  `(w/global my (nspace)
     (tldo ,@body)))

(mac copy-to-mine names
  (unless (all anormalsym names)
    (err:+ "A non-symbol or special symbol expression was passed to "
           "copy-to-mine."))
  `(= ,@(mappend [do `((my ,_) ,_)] names)))

(mac copy-to-nspace (ns . names)
  (unless (all anormalsym names)
    (err:+ "A non-symbol or special symbol expression was passed to "
           "copy-to-nspace."))
  (w/uniq g-ns
    `(= ,g-ns ,ns
        ,@(mappend [do `((,g-ns ,_) ,_)] names)
        ,g-ns nil)))

; This is a spoof form that imitates an nspaced form without actually
; protecting any variables. The point is that if you really don't like
; the effect nspaced has on Arc code, then you can drop in this
; non-implementation, maybe even saying (= nspaced not-nspaced) at the
; top level, and thereby use most code that's targeted at nspaced in a
; way that's closer to what you want without very much hassle.
(mac not-nspaced body
  `(w/global my (mc (what) what)
     (tldo ,@body)))