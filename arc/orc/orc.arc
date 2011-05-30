; orc.arc
;
; Miscellaneous utilities for more flexible treatment of custom data
; types in Arc.

(packed:using-rels-as mr "../multival/multirule.arc"
                      mu "../multival/multival.arc"
                      oc "../multival/order-contribs.arc"
                      ut "../utils.arc"


; ===== Simple inheritance registry ==================================
;
; This was originally posted at http://arclanguage.org/item?id=11981.

; This is a table from type symbols to lists of their supertypes other
; than my!any. Indirect supertypes are included in these lists, but no
; type is a member of its own list. There are no inheritance loops,
; either; inheritance here is a directed acyclic graph, and this is
; the transitive closure of that graph.
(= my.indirect-inheritance* (table))

(=fn my.inherits (subtype supertype)
  (or (in supertype subtype my!any)
      (~~mem [is supertype _] my.indirect-inheritance*.subtype)))

(=fn my.fn-def-inherits (subtype . supertypes)
  (each supertype supertypes
    (unless (my.inherits subtype supertype)
      (when (my.inherits supertype subtype)
        (err "There was an inheritance loop."))
      (let supers (cons supertype my.indirect-inheritance*.supertype)
        (zap [union is supers _] my.indirect-inheritance*.subtype)
        (each (k v) my.indirect-inheritance*
          (when (mem [is subtype _] v)
            (zap [union is supers _] my.indirect-inheritance*.k)))))))

(=mc my.def-inherits (subtype . supertypes)
  `(,my!fn-def-inherits
     ,@(map [do `',deglobalize._] (cons subtype supertypes))))

(=fn my.isinstance (x test-type)
  (my.inherits my.otype.x test-type))

(=fn my.a- (test-type)
  [my.isinstance _ test-type])

; For the purposes of these types, my!niltype is a subtype of sym, and
; only nil has the type my!niltype.

(my:def-inherits my.niltype sym)

; We also provide a list type, to make list methods nicer to create.
; Note that my!niltype inherits from both my!list and 'sym.
(my:def-inherits my.niltype my.list)
(my:def-inherits cons my.list)

; This can be overwritten to give special behavior to other kinds of
; types, such as Java objects and tables with their 'type fields set.
(=fn my.otype (x)
  (case x nil my!niltype
    (on-err [do my!any] (fn () type.x))))


; == A mechanism for rules which dispatch lexicographically by type ==
;
; (That is, they dispatch by the type of the first argument, then by
; the type of the second argument, and so forth.)

; This is a table which maps multival names to tables which map
; multimethod labels to proper lists containing the types associated
; with them thanks to a my!ontypes declaration. Any instances of the
; my!any type at the end of the list should be stripped off; all type
; lists are considered to end with an infinite string of my!any
; anyway, and this is the way they're expected to be normalized.
;
; Rules not created by my!ontypes are given the same preference as
; my!ontypes rules which specify an empty type list. Because of this,
; whether a label is omitted from the table or maps to nil makes no
; difference (even if they were different cases in Arc).
;
(= my.ontypes-types* (table))

(=mc my.ontypes (name parms test-types . labeled-body)
  (zap deglobalize name)
  ; TODO: See if we can 'deglobalize the types while also supporting
  ; types that aren't symbols (and consider whether that matters).
  (zap [rev:mem [~is my!any _] (rev:map deglobalize _)] test-types)
  (with ((label body) ut.parse-named-body.labeled-body
         g-test-types (uniq)
         g-args (uniq))
    (or= label (sym:apply string (uniq) '-ontypes--
                 (intersperse '-- test-types)))
    `(let ,g-test-types
            (= ((or= (,my!ontypes-types* ',name) (table)) ',label)
               (',thunk.test-types))
       (,mr!rule ,name ,g-args ,label
         (unless (and (len> ,g-args ,(- len.test-types 1))
                      ,@(accum acc
                          (forlen i test-types
                            (do.acc `(,my!isinstance
                                       (,g-args ,i)
                                       (,g-test-types ,i))))))
           (do.fail ,(if single.test-types
                       (+ "The first argument didn't match the type "
                          "\"" (tostring:write car.test-types) "\".")
                       (+ "The first arguments didn't match the "
                          "types "
                          "\"" (tostring write.test-types) "\"."))))
         (apply (fn ,parms ,@body) ,g-args)))))

; Within 'labeled-body, the anaphoric variable 'self refers to an
; implicit first argument, the argument which was dispatched on.
(=mc my.ontype (name parms test-type . labeled-body)
  `(,my!ontypes ,name ,(cons 'self parms) (,test-type)
     ,@labeled-body))

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
(mu.contribute oc!order-contribs my!ontypes-inheritance
  oc.self-orderer-reducer
  (fn (contribs)
    (accum acc
      (while contribs
        (let a pop.contribs
          (each b contribs
            (with (aname do.a!name bname do.b!name)
              (aand (is aname bname) my.ontypes-types*.aname
                (with (atypes (it do.a!label) btypes (it do.b!label))
                  (catch:while (or atypes btypes)
                    (with (atype (if atypes pop.atypes my!any)
                           btype (if btypes pop.btypes my!any))
                      ; NOTE: Ar parses a.b:c as (a b:c).
                      (only.throw (if
                        ; NOTE: Ar parses a.b:c as (a b:c).
                        (my.inherits atype btype)  (do.acc (list a b))
                        (my.inherits btype atype)  (do.acc (list b a))
                        
                        )))))))))))))


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

(my:ontypes my.oiso2 (a b) (cons cons) my.cons
  (and (my.oiso2 car.a car.b) (my.oiso2 cdr.a cdr.b)))

(my:ontypes my.oiso2 (a b) (table table) my.table
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
; that. Since the usual type-dispatching preference rule would
; specifically specify low preference, our preference rule has to
; override it.
(oc:label-prefer-labels-first my.is-first my!oiso2 my!is)
; TODO: The following seems like it implies too much. See if
; oc!order-contribs can be modified to handle semiorders rather than
; partial preorders, so that we can prioritize one rule over another
; without forcing ourselves to choose sides for all the other rules.
(oc:label-prefer-labels-last my.ontypes-inheritance-last
  oc!order-contribs my!ontypes-inheritance)

(=fn my.oiso args
  (iflet (first . rest) args
    (all [my.oiso2 first _] rest)
    t))


(my:ontype my.otestify () fn my.fn
  self)

(my:ontype my.otestify () my.any my.any
  [my.oiso2 self _])


)
