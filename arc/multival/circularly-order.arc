; circularly-order.arc

(packed:using-rels-as ut "../utils.arc"


(=fn my.set-minus (== a b)
  (ut:remlet elem a (mem [do.== elem _] b)))

(=fn my.subset (== a b)
  (~my.set-minus == a b))

(=fn my.make-transitive-dag (elems)
  (map [list _ nil nil] elems))

(=fn my.transitive-dag-has-edge (transitive-dag before after)
  ; NOTE: In Jarc 21, !2 uses the symbol |2|.
  (mem [is after _] (get.2:find [is before _.0] transitive-dag)))

(=fn my.transitive-dag-add-edge
       (transitive-dag before after error-thunk)
  (ut:xloop before before after after
    (unless (my.transitive-dag-has-edge transitive-dag before after)
      (when (my.transitive-dag-has-edge transitive-dag after before)
        call.error-thunk)
      (with (before-node (find [is before _.0] transitive-dag)
             after-node (find [is after _.0] transitive-dag))
         (each beforebefore do.before-node.1
           (do.next beforebefore after))
         (each afterafter do.after-node.2
           (do.next before afterafter))
         (push before do.after-node.1)
         (push after do.before-node.2)))))

(=fn my.transitive-dag-flatten (transitive-dag)
  (ut:ret result nil
    (let commit (fn (elems)
                  (each elem elems
                    (push elem result))
                  (zap [rem (fn (node) (mem [is do.node.0 _] elems))
                            _]
                       transitive-dag))
      (while transitive-dag
        ; NOTE: In Jarc 21, !0 uses the symbol |0|.
        (do.commit:map get.0
          (keep [my.subset is _.2 result] transitive-dag))))))

(=fn my.circularly-order (rep2comp comparator-reps)
  (accum acc
    (withs (; unpromoted recommendations
            ; ((recommender before after) ...)
            urs (mappend [ut:maplet rec do.rep2comp._.comparator-reps
                           (cons _ rec)]
                         comparator-reps)
            ; promoted recommendation graph, transitive closure
            ; ((comparator-rep befores afters) ...)
            prg my.make-transitive-dag.comparator-reps
            already-promoted
              (fn (before after)
                (my.transitive-dag-has-edge prg before after))
            add-rec (fn (before after)
                      (my.transitive-dag-add-edge prg before after
                        (thunk:err "Can't circularly-order.")))
            ucs comparator-reps  ; unpromoted comparator-reps
            pcs nil              ; promoted comparator-reps
            promote-recs
              (fn (recs)
                (each rec (rem [do.already-promoted _.2 _.1] recs)
                  (do.add-rec do.rec.1 do.rec.2))
                (zap [my.set-minus is _ recs] urs))
            promote-cs (fn (cs)
                         (each c cs
                           (do.promote-recs:keep [is c _.0] urs)
                           do.acc.c)
                         (zap [my.set-minus is _ cs] ucs)))
      (while ucs
        (withs (considered-cs (ut:remlet uc ucs
                                (some [do.already-promoted _ uc] ucs))
                considered-rs
                  (ut:keeplet ur urs
                    (and (mem [is do.ur.0 _] considered-cs)
                         (mem [is do.ur.1 _] ucs)
                         (mem [is do.ur.2 _] considered-cs))))
          (do.promote-cs:or (ut:remlet uc considered-cs
                              (some [is uc _.2] considered-rs))
                            ; NOTE: We would say
                            ; (map !0 considered-rs), except that that
                            ; could have duplicates.
                            (ut:keeplet uc considered-cs
                              (some [is uc _.0] considered-rs))))))))

; NOTE: We implement this in a sorta spaghetti way just to draw
; parallels with 'circularly-order. This should actually be totally
; consistent with 'circularly-order if the elements being sorted are
; seen as giving no recommendations of their own. However, they may in
; be exactly the same values as were used to represent the
; comparators, so we can't choose any single 'rep2comp function to
; encompass all the values. Besides, it's nice not to have to
; 'circularly-order the same things over and over again.
(=fn my.normally-order (comparators elements)
  (withs (; promoted recommendation graph, transitive closure
          ; ((element befores afters) ...)
          prg my.make-transitive-dag.elements
          already-promoted
            (fn (before after)
              (my.transitive-dag-has-edge prg before after))
          add-rec (fn (before after)
                    (my.transitive-dag-add-edge prg before after
                      (thunk:err "Can't normally-order.")))
          promote-recs
            (fn (recs)
              (each rec (rem [do.already-promoted _.1 _.0] recs)
                (do.add-rec do.rec.0 do.rec.1)))
          recommendation-sets (map .elements comparators))
    (each recommendation-set recommendation-sets
      do.promote-recs.recommendation-set)
    my.transitive-dag-flatten.prg))


)
