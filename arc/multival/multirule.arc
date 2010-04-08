; multirule.arc

(packed:using-rels-as mt "multival.arc"
                      oc "order-contribs.arc"
                      ru "../rules.arc"


; A basic-rulebook-reducer multival takes contributions that are
; tables that each map 'fn to a rule, it sorts those contributions
; using order-contribs, and it ultimately becomes a function that
; calls those sorted rules as a basic rulebook.
(def my.basic-rulebook-reducer (contribs)
  (let rulebook (map !fn:cadr (apply join oc.order-contribs.contribs))
    (obj val (fn args
               (apply ru.call-basic-rulebook rulebook args))
         cares `(,oc!order-contribs))))


(mac my.rule (name parms . body)
  (zap expand name)
  (let (label . actualbody) body
    (zap expand label)
    (unless (and actualbody anormalsym.label)
      (= actualbody (cons label actualbody) label (uniq)))
    `(do (,mt!defmultifn-stub ,name)
         (,mt!contribute ',name ',label ,my!basic-rulebook-reducer
           (obj fn (,ru!ru ,parms ,@actualbody))))))


)