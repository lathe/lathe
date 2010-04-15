; circularly-order-ccc.arc

(packed:using-rels-as am "../amb.arc"
                      ut "../utils.arc"


(def my.is-start-of-brackets (order-so-far rep-brackets)
  (with ((first-so-far . others-so-far) order-so-far
         (first-bracket . other-brackets) rep-brackets)
    (if no.order-so-far t
        no.rep-brackets nil
        no.first-bracket
      (my.is-start-of-brackets order-so-far other-brackets)
      (iflet the-pos (pos first-so-far first-bracket)
        (let (before it-and-after) (split first-bracket the-pos)
          (my.is-start-of-brackets
            others-so-far
            (cons (join before cdr.it-and-after) other-brackets)))))))

(def my.circularly-order (rep2comp comparator-reps)
  (let amb (am.make-amb
             (fn () (err "The comparators are circularly humble.")))
    (ut:xloop order-so-far '() rep-brackets list.comparator-reps
      (unless (my.is-start-of-brackets order-so-far rep-brackets)
        call.amb)
      (let reps (apply join rep-brackets)
        (if (is len.order-so-far len.reps)
          rep-brackets
          (let rep (apply amb car.rep-brackets)
            (do.next (join order-so-far list.rep)
                     do.rep2comp.rep.reps)))))))


)