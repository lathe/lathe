; circularly-order-noccc.arc

(packed:using-rels-as ir "../iter.arc"
                      ut "../utils.arc"


(def my.rempos (lst position)
  (let (before (removed . after)) (split lst position)
    (join before after)))

(def my.begins-with-unordered-is (lst prefix)
  (let len-prefix len.prefix
    (unless (< len.lst len-prefix)
      (catch:do1 t
        (for lst-position 0 (- len-prefix 1)
          (let element do.lst.lst-position
            (iflet prefix-position (pos [is element _] prefix)
              (zap [my.rempos _ prefix-position] prefix)
              (throw nil))))))))

; This returns nil on failure and a singleton list containing the
; difference on success. The singleton list is necessary because the
; difference itself may be nil.
(def my.hard-subtract-is (lst contents)
  (unless (< len.lst len.contents)
    (catch:list:ut:ret result lst
      (each element contents
        (iflet position (pos [is element _] result)
          (zap [my.rempos _ position] result)
          (throw nil))))))

; This takes a list of multisets (given as lists) of comparators and
; returns an iterable (see iter.arc) over lists of the same
; comparators, such that you can apply the first comparator in the
; output list to each of the input brackets to get a bunch more
; bracket collections, concatenate those bracket collections into a
; single collection of brackets, remove the *same* sorter from the
; *first* of these new brackets, and repeat the process (sorting by
; the next sorter and removing it from the new first bracket) until
; there are no sorters left.
;
; If 'must-come-first is nonempty, then the comparators in that list
; will be considered to have already sorted the given brackets. They
; must be at the beginning of the input brackets (the brackets'
; internal orders notwithstanding), and they will be returned first in
; every result. The intended reason for providing them in the first
; place is so that if they only represent part of the starting
; bracket, then further elements chosen from within that bracket can
; be tested to make sure they don't sort any 'must-come-first elements
; incorrectly when they sort that starting bracket.
;
; If in fact the 'must-come-first elements exist in the brackets but
; don't come first, it isn't an error. Instead, the resulting iterable
; is empty, signifying that there are no valid comparator orderings.
; That being the case, if the 'must-come-first elements don't exist in
; the brackets at all, that is an error.
;
(def my.sort-yourselves (rep2comp brackets (o must-come-first))
  (withs (sort-one-bracket (fn (bracket sorter)
                             (if (< len.bracket 2)
                               list.bracket
                               do.rep2comp.sorter.bracket))
          result-so-far nil
          len-brackets len.brackets)
    (catch:ut:dstwhilet (bracket . _) brackets
      (unless (my.begins-with-unordered-is must-come-first bracket)
        (throw nil))
      (let len-bracket len.bracket
        (zap [join _ (cut must-come-first 0 len-bracket)]
             result-so-far)
        (zap cdr brackets)
        (zap [cut _ len-bracket] must-come-first)))
    (iflet (first-bracket . rest-of-brackets) brackets
      (iflet (options) (my.hard-subtract-is first-bracket
                                            must-come-first)
        (ir.mapping [join result-so-far _]
          (ir:mappendinglet option ir.iterify.options
            (ut:foldlet result (my.sort-yourselves
                                 rep2comp
                                 (do.sort-one-bracket first-bracket
                                                      option)
                                 (cons option must-come-first))
                        bracket rest-of-brackets
              (ir:mappendinglet previous-sorted-stuff result
                (ir.mapping [join previous-sorted-stuff _]
                  (my.sort-yourselves
                    rep2comp
                    (ut:foldlet previous-bracket-brackets
                                  list.bracket
                                sorter previous-sorted-stuff
                      (mappend [do.sort-one-bracket _ sorter]
                               previous-bracket-brackets))))))))
        (if (my.hard-subtract-is (apply join brackets)
                                 must-come-first)
          (ir.empty-iter)
          (err:+ "Somehow there were sorters that were used but then "
                 "disappeared.")))
      (if must-come-first
        (err:+ "Somehow there were sorters that were used but then "
               "disappeared.")
        (ir.iterify list.result-so-far)))))

(def my.circularly-order (rep2comp comparator-reps)
  (or (call:call (my.sort-yourselves rep2comp
                   list.comparator-reps))
      (err "The comparators are circularly humble.")))


)