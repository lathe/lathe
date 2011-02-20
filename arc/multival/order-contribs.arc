; order-contribs.arc
;
; === An extensible, standard way to order the parts of a multival ===
;
; This defines order-contribs, a multival whose purpose is to order
; the contributions of multivals, which may be useful for implementing
; things like multimethod preference.
;
; Out of the box, order-contribs does nothing but produce a list of
; the elements of the contribution detail list passed to it, in an
; arbitrary order.
;
; To extend order-contribs, contribute to it a function that accepts a
; list of distinct contribution details (with regard to 'is) and
; outputs a list of recommendations, where a recommendation is a
; two-element list of distinct contribution details from the original
; list. A recommendation acts as a (very strong) hint to put its first
; element before its second element in the final order. The total
; recommendation list must be acyclic.
;
; A special aspect of 'order-contribs is the way that its *own*
; contributions are ordered. It orders them itself, using all its
; contributions' recommendations at once. As long as all the
; recommendations are consistent, this should be very intuitive. It
; should also work if they *disambiguate their own conflicts*, like
; so:
;
;   A recommends: X should come before Y.
;   B recommends: Y should come before X.
;   C recommends: A should come before B. (That is to say, it should
;                 take priority.)
;
; In this example, even though A and B conflict, C makes it clear
; which of their recommendations should be used.
;
; A full description of the algorithm used, along with some more
; examples, is in circularly-order.arc. (TODO: Make this true.)
;
; Be careful not to contribute a sort method that depends on a
; multival whose reducer uses 'order-contribs, or else you'll probably
; have some infinite recursion on your hands. Although 'order-contribs
; can bootstrap itself, it does so based on assumptions it makes about
; its own behavior, assumptions which can't necessarily be extended to
; the other multival.
;
; Speaking of assumptions, since 'order-contribs is intended for use
; within reducers, it's assumed to have no side effects. If a
; contributed sort method has side effects, no guarantees are made
; about when or how often those side effects will happen.

(packed:using-rels-as co "circularly-order.arc"
                      mu "multival.arc"
                      ut "../utils.arc"


; In case you want to have more than one 'order-contribs for different
; purposes, all it takes to make a new one is to define a multival
; that uses 'self-orderer-reducer.
;
; Keep in mind that 'contribs-that-order will be a list of
; contribution details (tables mapping 'val to the contributed value).
; Furthermore, the contributions to a 'self-orderer-reducer multival
; must be comparator functions which accept lists of contribution
; details and return recommendations.
;
(=fn my.self-orderer-reducer (contribs-that-order)
  (withs (rep2comp !val
          ordered-orderers (map rep2comp (my.circularly-order rep2comp
                                           contribs-that-order)))
    (obj val (fn (contribs-to-order)
               (my.normally-order ordered-orderers contribs-to-order))
         cares '())))

; On a lower level, if all you want to do is order a bunch of things
; based on themselves the way 'order-contribs does, you can use
; circularly-order. The 'comparator-reps argument is a list of
; comparator representations (in the case of 'order-contribs, the
; contributions). The 'rep2comp argument is a function that produces a
; comparator from its representation (in the case of 'order-contribs,
; by extracting the 'val entry from a contribution detail table). Once
; again, the kind of comparator needed here is a function that accepts
; a list of things and outputs a list of "recommendations," each
; recommendation being a two-element list of distinct things in the
; original list, such that the total recommendation graph is acyclic.
; The recommendations determine which things should come before which
; other things.
;
; NOTE: This could be optimized greatly. Then again, this should only
; be done once or twice per run of a program when calculating multival
; values (which are cached), so it might not be worth worrying about.
;
(= my.circularly-order co.circularly-order)

; Similarly, 'normally-order lets you sort a set of elements based on
; a list of comparator functions that return lists of recommendations.
; It's more straightforward. Recommendations from each comparator are
; set into stone one set at a time, starting with the first comparator
; and going through the list. A single recommendation that conflicts
; with the recommendations so far is ignored. However, multiple added
; recommendations may conflict in tandem but not individually, and
; that causes an error.
(= my.normally-order co.normally-order)

(mu:defmultifn-stub my.order-contribs my.self-orderer-reducer)

(=fn my.fn-label-prefer (label . tests)
  (zap [map testify _] tests)
  (mu.contribute my!order-contribs label my.self-orderer-reducer
    (fn (contribs)
      (accum acc
        (let ranks (accum acc
                     (each test tests
                       (let rank (keep test contribs)
                         do.acc.rank
                         (zap [co.set-minus is _ rank] contribs))))
          (while cdr.ranks
            (each a pop.ranks
              (each b car.ranks
                (do.acc:list a b)))))))))

(=fn my.prefer tests
  (apply my.fn-label-prefer (uniq) tests))

(=mc my.label-prefer (label . tests)
  `(,my!fn-label-prefer ',deglobalize.label ,@tests))


; These are utilities for making contribs with certain labels have
; high or low preference.
;
; These were originally posted at
; http://arclanguage.org/item?id=11784.

(let get-predicates (fn (name labels)
                      (map (fn (label)
                             [and (is _!name name)
                                  (is _!label label)])
                           labels))
  
  (=fn my.fn-label-prefer-labels-first
         (label multival-name . label-names)
    (let predicates (do.get-predicates multival-name label-names)
      (apply my.fn-label-prefer label predicates)))
  
  (=fn my.fn-label-prefer-labels-last
         (label multival-name . label-names)
    (let predicates (do.get-predicates multival-name label-names)
      (apply my.fn-label-prefer
        label [~some ._ predicates] predicates)))
  )

(=fn my.prefer-labels-first (multival-name . label-names)
  (apply my.fn-label-prefer-labels-first
    (uniq) multival-name label-names))

(=fn my.prefer-labels-last (multival-name . label-names)
  (apply my.fn-label-prefer-labels-last
    (uniq) multival-name label-names))

(=mc my.label-prefer-labels-first (label multival-name . label-names)
  `(,my!fn-label-prefer-labels-first
     ',deglobalize.label ,multival-name ,@label-names))

(=mc my.label-prefer-labels-last (label multival-name . label-names)
  `(,my!fn-label-prefer-labels-last
     ',deglobalize.label ,multival-name ,@label-names))


)
