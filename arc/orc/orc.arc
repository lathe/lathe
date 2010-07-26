; orc.arc
;
; Miscellaneous utilities for more flexible treatment of custom data
; types in Arc.

(packed:using-rels-as mr "../multival/multirule.arc"
                      mu "../multival/multival.arc"
                      oc "../multival/order-contribs.arc"
                      st "../sort.arc"
                      ut "../utils.arc"


; ===== Simple inheritance registry ==================================
;
; This was originally posted at http://arclanguage.org/item?id=11981.

; This is a table from type symbols to lists of their supertypes.
; Indirect supertypes are included in these lists, but no type is a
; member of its own list. There are no inheritance loops, either;
; inheritance here is a directed acyclic graph, and this is the
; transitive closure of that graph.
(= my.indirect-inheritance* (table))

(=fn my.inherits (subtype supertype)
  (~~mem supertype (cons subtype my.indirect-inheritance*.subtype)))

(=fn my.fn-def-inherits (subtype . supertypes)
  (each supertype supertypes
    (when (~my.inherits subtype supertype)
      (when (my.inherits supertype subtype)
        (err "There was an inheritance loop."))
      (let supers (cons supertype my.indirect-inheritance*.supertype)
        (zap [union is supers _] my.indirect-inheritance*.subtype)
        (each (k v) my.indirect-inheritance*
          (when (mem subtype v)
            (zap [union is supers _] my.indirect-inheritance*.k)))))))

(=mc my.def-inherits (subtype . supertypes)
  `(,my!fn-def-inherits
     ,@(map [do `',ut.deglobalize-var._] (cons subtype supertypes))))

(=fn my.isinstance (x test-type)
  (my.inherits my.otype.x test-type))

(=fn my.a- (test-type)
  [my.isinstance _ test-type])

; For the purposes of these types, nil is a subtype of sym, and only
; nil has the type nil. (We assume that without this special
; treatment, nothing would be tagged with nil anyway.)

(my:def-inherits my.niltype sym)

; We also provide a list type, to make list methods nicer to create.
; Note that my!niltype inherits from both my!list and 'sym.
(my:def-inherits my.niltype my.list)
(my:def-inherits cons my.list)

; This can be overwritten to give special behavior to other kinds of
; types, such as Java objects and tables with their 'type fields set.
(=fn my.otype (x)
  (if x
    (catch:or (errsafe:throw:type x) my!unknown)
    my!niltype))


; ===== A mechanism for rules which use single dispatch ==============

; This is a table which maps multival names to tables which map
; multimethod labels to the types associated with them thanks to an
; 'ontype declaration. Labels which aren't associated with types are
; omitted. Multival names which would map to empty tables may be
; omitted.
(= my.ontype-types* (table))

; Within 'labeled-body, the anaphoric variable 'self refers to an
; implicit first argument, the argument which was dispatched on.
(=mc my.ontype (name parms test-type . labeled-body)
  (zap ut.deglobalize-var name)
  (zap ut.deglobalize-var test-type)
  (let (label body) ut.parse-named-body.labeled-body
    (or= label (sym:string (uniq) '-ontype- test-type))
    `(do (= ((or= (,my!ontype-types* ',name) (table)) ',label)
            ',test-type)
         (,mr!rule ,name ,(cons 'self parms) ,label
           (unless (,my!isinstance self ',test-type)
             (do.fail ,(+ "The first argument didn't match the type "
                          "\"" test-type "\".")))
           ,@body))))

; We give methods whose types are related by inheritance an automatic
; preference order so that the most specific method is tried first.
;
; Note that the supertype's method may still make a difference if the
; subtype's method specifically fails (using its anaphoric 'fail
; function). Therefore, subtype methods which need to fully override
; the supertype method, but which also need to be defined in multiple
; rules, should make sure to specify a rule that raises an error or
; returns a default value instead of failing.
;
(mu.contribute oc!order-contribs my!ontype-inheritance
  oc.self-orderer-reducer
  (st.<=>-to-bracketer:fn (a b)
    (with (aname do.a!name bname do.b!name)
      (or (ut:andlets
            (is aname bname)
            my.ontype-types*.aname
            atype  (it do.a!label)
            my.ontype-types*.bname
            btype  (it do.b!label)
            (if (my.inherits atype btype) '<
                (my.inherits btype atype) '>))
          '=))))


; ===== An extensible equivalence predicate ==========================
;
; This was originally posted at http://arclanguage.org/item?id=11784,
; but there it was phrased for REPL use, and it incorporated a
; "pickle" concept that wasn't especially necessary.

; Note that we don't actually expect the 'cons and 'table types to be
; inherited, since that would presumably require implementing every
; one of their non-reflective features, including compatibility with
; arc.arc utilities, [coerce _ 'fn] semantics, etc. For that reason,
; we're comfortable with using their reflective features--'acons and
; [isa _ 'table]--to identify them.

(mr:rule my.oiso2 (a b) my.cons
  (unless (and acons.a acons.b)
    (do.fail "The parameters weren't all cons cells."))
  (and (my.oiso2 car.a car.b) (my.oiso2 cdr.a cdr.b)))

(mr:rule my.oiso2 (a b) my.table
  (unless (all [isa _ 'table] (list a b))
    (do.fail "The parameters weren't all tables."))
  (and (is len.a len.b)
       (all [my.oiso2 _.1 (do.b _.0)] tablist.a)))

(mr:rule my.oiso2 (a b) my.is
  (unless (is a b)
    (do.fail "The parameters weren't reference-identical."))
  t)

(mr:rule my.oiso2 (a b) my.default
  nil)

; We want the default 'oiso2 to have the lowest preference since it
; never fails.
(oc:label-prefer-labels-last my.oiso2-default-last my!oiso2
  my!default)

; It's probably more efficient to test for (is a b) first, so we'll do
; that.
(oc:label-prefer-labels-first my.is-first my!oiso2 my!is)

(=fn my.oiso args
  (or no.args
      (let (first . rest) args
        (all [my.oiso2 first _] rest))))


(my:ontype my.otestify () fn my.fn
  self)

(mr:rule my.otestify (self) my.default
  [my.oiso2 self _])

(oc:label-prefer-labels-last my.otestify-default-last my!otestify
  my!default)


)