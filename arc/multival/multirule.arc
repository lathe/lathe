; multirule.arc

(packed:using-rels-as mt "multival.arc"
                      oc "order-contribs.arc"
                      ru "../rules.arc"
                      ut "../utils.arc"


; A basic-rulebook-reducer multival takes contributions that are rules
; (which are extracted here from the 'val entries of contribution
; detail tables), it sorts those contributions using order-contribs,
; and it ultimately becomes a function that calls those sorted rules
; as a basic rulebook.
(=fn my.basic-rulebook-reducer (contribs)
  (let rulebook (map !val (apply join oc.order-contribs.contribs))
    (obj val (fn args
               (apply ru.call-basic-rulebook rulebook args))
         cares `(,oc!order-contribs))))


(=mc my.rule (name parms . labeled-body)
  (zap deglobalize name)
  (let (label body) ut.parse-named-body.labeled-body
    `(do (,mt!defmultifn-stub ,name)
         (,mt!contribute ',name ',(or label (uniq))
           ,my!basic-rulebook-reducer (,ru!ru ,parms ,@body)))))


)
