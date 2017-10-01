; utils-part-2.arc
;
; Miscellaneous utilities, part 2.
;
; This is in several parts so that certain utilities can be used in
; the transformation phases of other utilities.

#lang racket
(require "utils-part-1.rkt")
(require (for-syntax "utils-part-1.rkt"))
(provide (all-from-out "utils-part-1.rkt"))
(provide (all-defined-out))

; This is based on <http://arclanguage.org/item?id=13450>, but it
; isn't based on the refined idea at
; <http://arclanguage.org/item?id=13888> of a reader macro.
;
; TODO: See if it's actually helpful in the long run.
;
(define-syntax (: stx)
  (syntax-case stx ()
    [ (_ body ...)
      (let loop ([body #'(body ...)])
        (syntax-case body (:)
          [() body]
          [(: rest ...) #`(#,(loop #'(rest ...)))]
          [(first rest ...) #`(first #,@(loop #'(rest ...)))]))]))


(define-syntax-rule (each var seq body ...)
  (for-each (fn (var) body ...) seq))

(define-syntax-rule (maplet var seq body ...)
  (map (fn (var) body ...) seq))


(define-syntax-rule (thunk body ...)
  (fn () body ...))

(define (listish? x)
  (or (null? x) (pair? x)))


(define (like-syntax model imitator)
  (if (syntax? model)
    (datum->syntax model imitator model)
    imitator))


; This was introduced at <http://arclanguage.org/item?id=13407>.
(define-syntax delisting
  (syntax-rules ()
    [ (delisting rest seq)
      (let ([rest seq])
        voidval)]
    [ (delisting rest seq default)
      (let ([rest seq])
        default)]
    [ (delisting rest seq then nextvar defaults ...)
      (let ([rest seq])
        (match rest
          [ (cons nextvar our-rest)
            (delisting rest our-rest
              defaults ...)]
          [_ then]))]))

(define (parse-pair stx)
  (syntax-case stx ()
    [() stx]
    [(a b rest ...) #`((a b) #,@(parse-pair #'(rest ...)))]
    [_ (error "Expected a syntax that could be paired")]))

(define (parse-magic-withlike body [pairerr #f])
  (syntax-case body ()
    [ ((raw-binds ...) body ...)
      #`(#,(parse-pair #'(raw-binds ...)) body ...)]
    [_
      (let loop ([body body])
        (syntax-case body ()
          [ (var val body ...)
            (identifier? #'var)
            (syntax-case (loop #'(body ...)) ()
              [ ((binds ...) body ...)
                #'(([var val] binds ...) body ...)])]
          [_ #`(() #,@body)]))]))
