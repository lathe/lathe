; order-contribs.arc
;
; === An extensible, standard way to order the parts of a multival ===
;
; This defines order-contribs, a multival whose purpose is to order
; the contributions of multivals, which may be useful for implementing
; things like multimethod preference.
;
; Out of the box, order-contribs does nothing but produce a singleton
; list that contains the list of contributions passed to it. This
; represents the idea that all the contributions have been given the
; same ranking. In other words, they're all in the same rank bracket,
; and the return value of order-contribs is a list of just that one
; bracket.
;
; To extend order-contribs, contribute to it a function that acts just
; like order-contribs, taking a list of contribution detail tables and
; returning a list of brackets of those tables. If all the functions
; contributed this way are stable sorts, in the sense that the orders
; of contributions within each resulting bracket are the same as the
; orders those contributions started with in the original list, then
; order-contribs will also be a stable sort. The initial list of
; contributions passed to order-contribs will be in the reverse order
; they were contributed (usually the same as the reverse code order),
; and it's a good idea to preserve this property, just so that when
; programmers decide that rearranging definitions is the most
; appropriate way to fix something, it's easier for them to predict
; and reason about what the new order should be.
;
; A special aspect of order-contribs is the way that its *own*
; contributions are ordered. Essentially, it orders them itself. For a
; better idea of how this works, suppose one of the contributed sort
; methods would sort itself after some other one. Then that can't be
; the first sort method; if it were, then it wouldn't be. An
; exhaustive search is done of all the possible orders, except that
; any search branches that end up being absurd like this are trimmed
; off. Once a non-absurd order is found (which will be an order that's
; finer than or equivalent to the total preorder (i.e. brackets) it
; determines), it's used.
;
; If there are multiple orders that could be found this way, then the
; *particular* one found is unspecified. For instance, if five sort
; methods each determine *total* orders (by returning lists of
; singleton brackets), and each puts itself at the beginning, then
; none of those orders is absurd, so any of them could be found. So be
; careful when contributing sort methods that might unduly trample
; over each other like that.
;
; Another thing: Be careful not to contribute a sort method that
; depends on a multival whose reducer uses order-contribs, or else
; you'll probably have some infinite recursion on your hands. Although
; order-contribs can bootstrap itself, it does so based on assumptions
; it makes about its own behavior, assumptions which can't necessarily
; be extended to the other multival.
;
; Speaking of assumptions, since order-contribs is intended for use
; within reducers, it's assumed to have no side effects. If a
; contributed sort method has side effects, no guarantees are made
; about when or how often those side effects will happen.

(packed:using-rels-as co "circularly-order-noccc.arc"
                      mu "multival.arc"
                      st "../sort.arc"
                      ut "../utils.arc"


; In case you want to have more than one order-contribs for different
; purposes, all it takes to make a new one is to define a multival
; that uses self-orderer-reducer.
;
; Keep in mind that contribs-that-order will be a list of contribution
; details (tables mapping 'val to the contributed value). Furthermore,
; the contributions to a self-orderer-reducer multival must be
; comparator functions which accept lists of contribution details and
; return ordered partitions ("brackets") of those lists.
;
(=fn my.self-orderer-reducer (contribs-that-order)
  (withs (rep2comp !val
          ordered-orderers (map rep2comp
                             (apply join (my.circularly-order rep2comp
                                           contribs-that-order))))
    (obj val (fn (contribs-to-order)
               (ut:foldlet rankings  list.contribs-to-order
                           orderer   ordered-orderers
                 (mappend orderer rankings)))
         cares '())))

; On a lower level, if all you want to do is order a bunch of things
; based on themselves the way order-contribs does, you can use
; circularly-order. The comparator-reps argument is a list of
; comparator representations (in order-contribs's case, the
; contributions). The rep2comp argument is a function that produces a
; comparator from its representation (in order-contribs's case, by
; extracting the 'val entry from a contribution detail table). Once
; again, the kind of comparator needed here is a function that accepts
; a list of things, partitions that list into multiple lists
; (brackets), sorts the brackets, and returns the list of sorted
; brackets.
;
; NOTE: This could be optimized greatly. Then again, this should only
; be done once or twice per run of a program when calculating multival
; values (which are cached), so it might not be worth worrying about.
;
(= my.circularly-order co.circularly-order)

(mu:defmultifn-stub my.order-contribs my.self-orderer-reducer)

(=fn my.fn-label-prefer (label . tests)
  (mu.contribute my!order-contribs label my.self-orderer-reducer
    (st:<=>-to-bracketer:apply st.order-by-tests tests)))

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