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


; This is based on a strategy introduced at or around
; <http://arclanguage.org/item?id=13584>. An alternate strategy is at
; <http://arclanguage.org/item?id=12916>.

(define (fn-as-non-syntax syntax body)
  (if (syntax? syntax)
    (let ([result (body (syntax-e syntax))])
      (if (syntax? result)
        result
        (datum->syntax syntax result syntax)))
    (body syntax)))

(define-syntax-rule (as-non-syntax var syntax body ...)
  (fn-as-non-syntax syntax (fn (var) body ...)))


(define-syntax ifs
  (syntax-rules ()
    [ (ifs)
      voidval]
    [ (ifs else)
      else]
    [ (ifs condition then elses ...)
      (if condition then (ifs elses ...))]))
