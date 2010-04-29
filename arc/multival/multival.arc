; multival.arc
;
; ===== Multival implementation and bare API =========================

(packed (using-rels-as ut "../utils.arc"


(= my.reducers* (table))
  ; A table mapping a symbol to a reducer.
  ;
  ; A reducer accepts a list of contribution details (tables of the
  ; form (obj val ... name ... label ... meta ...), where 'val maps
  ; to the essential contribution value) and returns a value of the
  ; form (obj val ... cares ...), where the val is the observable
  ; value of the multival and the cares is the list of symbols
  ; corresponding to other multivals whose values were needed while
  ; calculating the reduction, but not necessarily including any
  ; multivals which were only needed because some other cared-about
  ; multivalue needed them.
  ;
  ; If a reducer has side effects, no guarantees are made about when
  ; or how often those side effects will happen. This disclaimer
  ; exists mainly so that the reductions can be cached.
  ;
  ; NOTE: The 'cares bit accomplishes cache invalidation cascading,
  ; but it's kind of a hack; it might be possible to accomplish this
  ; some other way and let the reducer definitions be simpler. Then
  ; again, it might be possible to facilitate reducer definitions just
  ; as simple *on top of* this interface, and reducers are supposed to
  ; be abstracted away most of the time anyway, so it's probably
  ; better to do what's easiest here so that anyone who needs to
  ; unwrap the abstractions has it just as easy.

; A table mapping each multival name to a list of contribution
; details (tables with 'name, 'label, 'val, and 'meta fields, where
; 'val is the essential value of the contribution). The list of
; details is eventually passed to the multival's reducer.
(= my.contribs* (table))


(let multival-cache (table)
  
  (def my.get-multival (name)
    (!val:car
      (or= do.multival-cache.name
        (list ((car (my.reducers* name)) (my.contribs* name))))))
  
  (def my.invalidate-multival names
    (while names
      (each name names
        (wipe do.multival-cache.name))
      (= names (keep [iflet (reduction) do.multival-cache._
                       (some [mem _ names] !cares.reduction)]
                 keys.multival-cache))))
  
  (def my.invalidate-all-multivals ()
    (each key keys.multival-cache
      (wipe do.multival-cache.key)))
  )

(def my.submit-reducer (name reducer)
  (iflet (existing-reducer) (my.reducers* name)
    (unless (iso reducer existing-reducer)
      (err:+ "Multiple reducers have been submitted for the same "
             "multivalue. (This probably means the multivalue has "
             "been defined in multiple parts, where the parts are of "
             "completely different kinds.)"))
    (= my.reducers*.name list.reducer)))

(def my.submit-contribution (name label val (o meta))
  (zap [cons (obj name name label label val val meta meta)
             (rem [iso _!label label] _)]
       my.contribs*.name)
  (my.invalidate-multival name))

(def my.contribute (name label reducer contribution)
  (my.submit-reducer name reducer)
  (my.submit-contribution name label contribution))

(def my.fn-defmultifn-stub (name (o reducer))
  (when reducer
    (my.submit-reducer name reducer))
  (=fn global.name args
    (apply (my.get-multival name) args)))

(mac my.defmultifn-stub (name (o reducer))
  `(,(my 'fn-defmultifn-stub) ',expand.name ,reducer))


))