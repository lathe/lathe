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

(define (non-syntax syntax)
  (if (syntax? syntax)
    (syntax-e syntax)
    syntax))

; This is based on <http://arclanguage.org/item?id=13450>, but it
; isn't based on the refined idea at
; <http://arclanguage.org/item?id=13888> of a reader macro.
;
; TODO: See if it's actually helpful in the long run.
;
(define-syntax (: stx)
  (let loop ([stx (fn-as-non-syntax stx cdr)])
    (as-non-syntax stx stx
      (ifs (null? stx)
        stx
           (eqv? ': (syntax-e (car stx)))
        (list (loop (cdr stx)))
        (cons (car stx) (loop (cdr stx)))))))


(define-syntax-rule (each var seq body ...)
  (for-each (fn (var) body ...) seq))

(define-syntax-rule (maplet var seq body ...)
  (map (fn (var) body ...) seq))


(define-syntax-rule (thunk body ...)
  (fn () body ...))

(define (has-first? seq)
  (: cons? : non-syntax seq))

(define (empty? seq)
  (: null? : non-syntax seq))

(define (listish? x)
  (or (null? x) (pair? x)))

(define (seq? x)
  (or (empty? x) (has-first? x)))


(define (like-syntax model imitator)
  (if (syntax? model)
    (datum->syntax model imitator model)
    imitator))


(define (fn-iffirst-lenient seq then else)
  (if (: pair? : non-syntax seq)
    (then (: car : non-syntax seq) (fn-as-non-syntax seq cdr))
    (else)))

(define-syntax-rule (iffirst-lenient first rest seq then elses ...)
  (fn-iffirst-lenient seq (fn (first rest) then)
                          (: thunk : ifs elses ...)))

(define (fn-iffirst seq then [else error] [fail error])
  (iffirst-lenient first rest seq
    (then first rest)
      (empty? seq)
    (else)
    (fail)))

(define-syntax-rule (letfirst first rest seq then ...)
  (fn-iffirst seq (fn (first rest) then ...)))

(define-syntax-rule (iffirst first rest seq then elses ...)
  (fn-iffirst seq (fn (first rest) then) (: thunk : ifs elses ...)))

(define-syntax-rule (iffirst-fail first rest seq then else fail)
  (fn-iffirst seq (fn (first rest) then) (thunk else) (thunk fail)))

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
        (iffirst-lenient nextvar our-rest rest
          (delisting rest our-rest
            defaults ...)
          then))]))

(define (rev seq [end null])
  (as-non-syntax seq seq
    (iffirst first rest seq
      (: rev rest : cons first end)
      end)))

(define (tuples n seq [pairerr #f])
  (unless (0 . < . n)
    (error))
  (as-non-syntax seq seq
    (let loop ([seq seq] [rev-result null] [rev-tuple null] [i 0])
      (iffirst-fail first rest seq
        (if (<= n i)
          (loop rest (cons (rev rev-tuple) rev-result) (list first) 0)
          (loop rest rev-result (cons first rev-tuple) (add1 i)))
        (: rev : cons (rev rev-tuple) rev-result)
        (if pairerr
          (raise pairerr)
          (: rev : cons (rev rev-tuple seq) rev-result))))))

(define (pair seq [pairerr #f])
  (tuples 2 seq pairerr))

(define (parse-magic-withlike body [pairerr #f])
  (: as-non-syntax body body : delisting rest body
           (cons null body)
    first  (if (has-first? first)
             (cons (pair first pairerr) rest)
             (let loop ([rev-binds null] [body body])
               (delisting rest body
                      (cons (rev rev-binds) body)
                 var  (cons (rev rev-binds) body)
                 val  (if (: symbol? : non-syntax var)
                        (loop (cons (list var val) rev-binds) rest)
                        (cons (rev rev-binds) body)))))))
