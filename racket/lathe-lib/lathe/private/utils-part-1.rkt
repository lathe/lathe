; utils-part-1.arc
;
; Miscellaneous utilities, part 1.
;
; This is in several parts so that certain utilities can be used in
; the transformation phases of other utilities.

#lang racket
(provide (all-defined-out))


(define (idfn result)
  result)

(define voidval (void))

(define-syntax-rule (fn parms body ...)
  (lambda parms
    voidval
    body ...))


(define-syntax ifs
  (syntax-rules ()
    [ (ifs)
      voidval]
    [ (ifs else)
      else]
    [ (ifs condition then elses ...)
      (if condition then (ifs elses ...))]))
