; iter-demo.arc

(prn "starting iter-demo")

(nspaced (using-rels-as ir "../iter.arc"


(= my.tests-succeeded t)

(mac my.test-iso (simple complicated)
  (w/uniq (g-simple g-complicated)
    `(with (,g-simple ,simple ,g-complicated ,complicated)
       (unless (iso ,g-simple ,g-complicated)
         (wipe ,my!tests-succeeded)
         (prn:+ "FAILED: These should be 'iso: " ,g-simple " and "
                ,g-complicated ". The latter's expression was "
                ',complicated ".")))))


(my (test-iso '(()) (ir.iter->list (ir.nonneg-tuples-by-sum 0 0))))

(my (test-iso '((0 0 3) (0 1 2) (0 2 1) (0 3 0)
                (1 0 2) (1 1 1) (1 2 0)
                (2 0 1) (2 1 0)
                (3 0 0))
      (ir.iter->list (ir.nonneg-tuples-by-sum 3 3))))

(my (test-iso '((a A 0) (b A 0) (c A 0)
                (a B 0) (b B 0) (c B 0)
                (a A 1) (b A 1) (c A 1)
                (a B 1) (b B 1) (c B 1))
      (ir.iter->list (ir.iter*colexico '(a b c) '(A B) '(0 1)))))

(my (test-iso '((0 0 0)
                (0 0 1) (0 1 0)
                (1 0 0)
                (0 0 2) (0 1 1) (0 2 0)
                (1 0 1) (1 1 0)
                (2 0 0)
                (0 0 3) (0 1 2) (0 2 1) (0 3 0)
                (1 0 2) (1 1 1) (1 2 0)
                (2 0 1) (2 1 0)
                (3 0 0))
      (ir.iter->list
        (ir.stoppingafter 20 (ir.sum-grouped-nonneg-tuples 3)))))

(my (test-iso '((A a a)
                (A a b) (A b a)
                (B a a)
                (A a c) (A b b) (A c a)
                (B a b) (B b a)
                (C a a))
      (ir.iter->list
        (ir.stoppingafter 10
          (with (lowers (ir.repeating '(a b c d e f g h i j k l m))
                 uppers (ir.repeating '(A B C D E F G H I J K L M)))
            (ir.iter*sum-grouped uppers lowers lowers))))))

(my (test-iso '((A a a)
                (B a a) (A b a)
                (A a b)
                (C a a) (B b a) (A c a)
                (B a b) (A b b)
                (A a c))
      (ir.iter->list
        (ir.stoppingafter 10
          (with (lowers (ir.repeating '(a b c d e f g h i j k l m))
                 uppers (ir.repeating '(A B C D E F G H I J K L M)))
            (ir.iter*sum-grouped-colexico uppers lowers lowers))))))

; This is commented out so that this demo can work on Jarc too.
;
;(= my.fibs (ir (yielder (a b)
;                 do.yield.a
;                 do.yield.b
;                 (while t
;                   (let next (+ a b)
;                     (= a b b next)
;                     do.yield.next)))))
;
;(my (test-iso '(-8 5 -3 2 -1 1 0 1 1 2 3 5 8)
;      (ir.iter->list (ir.stoppingafter 13 (my.fibs -8 5)))))


(if my.tests-succeeded (prn "All tests succeeded!"))

(prn "finishing iter-demo")

nil


))