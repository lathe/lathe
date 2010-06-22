; amb.arc
;
; Backtracking utilities.

(packed


; This function, make-custom-amb, implements amb without any
; underlying global backtracking continuation stack. Instead, it takes
; callback parameters that simulate pushing and popping from such a
; stack. So any stack, whether it's global or local, cons list or some
; other implementation, can be turned into an amb with this.
;
; If the amb function created here runs out of options and can no
; longer backtrack, it will call the fail parameter, which by default
; will raise an exception. Extra care is taken to make sure that the
; fail callback will be executed within the dynamic context of the
; call to make-custom-amb, rather than that of any of the calls to the
; resulting amb function. This way only one function call needs to be
; wrapped with error-handling logic.
;
(=fn my.make-custom-amb
       (push-frame
        has-frame
        pop-frame
        (o fail (fn () (err:+ "All amb branches have run out of "
                              "options."))))
  (point return-from-maker
    (let goto-fail (point store-continuation-in-goto-fail
                     ccc.store-continuation-in-goto-fail
                     
                     ; The previous line stores this position so
                     ; that (return-from-maker:fail) can be executed
                     ; in this dynamic context when goto-fail is
                     ; called.
                     
                     (call return-from-maker call.fail)
                     )
      (fn branches
        (point return-from-amb
          
          ; First, try each branch in turn.
          (each branch branches
            
            (point backtrack
              do.push-frame.backtrack
              do.return-from-amb.branch)
            
            ; We should only get to here if the continuation we just
            ; pushed has been popped by an unsuccessful amb call.
            ; (Technically pop-frame may not honor this assumption,
            ; but we can't do anything about that.)
            )
          
          ; Now that we're out of branches, pop the next continuation
          ; to backtrack to, and call it. If it doesn't exist, use
          ; goto-fail as the continuation instead.
          (if call.has-frame
            call.pop-frame.nil
            do.goto-fail.nil)
          )))))

; This function will create a new backtracking continuation stack and
; encapsulate it within a function that can be used as an amb. One
; advantage of using this instead of a global backtracking stack is
; that once the result of the backtracking search is known and the amb
; usage is over, the amb function can simply go out of scope, leaving
; all its continuations to be garbage-collected.
(=fn my.make-amb ((o fail (fn () (err:+ "All amb branches have run "
                                        "out of options."))))
  (let amb-frames nil
    (my.make-custom-amb [push _ amb-frames]
                        (fn () amb-frames)
                        (fn () pop.amb-frames)
                        fail)))


)