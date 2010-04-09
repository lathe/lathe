; sort.arc
;
; Utilities for comparing and sorting things.

(packed:using-rels-as ut "utils.arc"


; The "brackets" here are the equivalence classes determined by the
; total preorder (equivalently, the strict weak order) <=>. The
; brackets will be sorted such that they're increasing.
;
; These sorts are stable. That is, if two elements are equivalent
; under <=>, they will appear in the same order in their bracket as
; they appear in the original list. Hence, if <=> doesn't distinguish
; any two elements, the result of 'mergesort-to-brackets will simply
; be a singleton list containing the original list.

(def my.merge-brackets (<=> a b)
  (= a (keep idfn a) b (keep idfn b))
  (ut:xloop a a b b acc nil
    (if (and a b)
      (case (do.<=> caar.a caar.b)
        < (do.next cdr.a b (cons car.a acc))
        > (do.next a cdr.b (cons car.b acc))
        = (do.next cdr.a cdr.b (cons (join car.a car.b) acc))
          (err "Not a comparator."))
      (join rev.acc a b))))

(def my.mergesort-to-brackets (<=> lst)
  (when lst
    (let merge (fn (a b) (my.merge-brackets <=> a b))
      (ut:xloop sorted-lists (map list:list lst)
        (let num-of-lists len.sorted-lists
          (case num-of-lists 1
            car.sorted-lists
            (do.next (if odd.num-of-lists
                       (cons car.sorted-lists
                             (pair cdr.sorted-lists merge))
                       (pair sorted-lists merge)))))))))

(def my.<=>-to-bracketer (<=>)
  [my.mergesort-to-brackets <=> _])

(def my.<-to-<=> (<)
  (fn (a b)
    (if (do.< a b)  '<
        (do.< b a)  '>
                    '=)))

(def my.<=-to-<=> (<=)
  (fn (a b)
    (if (do.<= a b)
      (if (do.<= b a) '= '<)
      '>)))

(def my.inverse-<=> (<=>)
  (fn (a b)
    (do.<=> b a)))

(def my.inverse-bracketer (bracketer)
  rev:bracketer)

(def my.order-by-tests tests
  (zap [map testify _] tests)
  (fn (a b)
    (catch
      (each test tests
        (with (nta (no do.test.a) ntb (no do.test.b))
          (unless (is nta ntb)
            (throw:if nta '> '<))))
      '=)))


)