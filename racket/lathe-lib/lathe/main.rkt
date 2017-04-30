; utils.arc
;
; Miscellaneous utilities, final part.
;
; This is in several parts so that certain utilities can be used in
; the transformation phases of other utilities.

#lang racket
(require racket/stxparam)
(require "private/utils-part-2.rkt")
(require (for-syntax "private/utils-part-2.rkt"))
(provide (all-from-out "private/utils-part-2.rkt"))
(provide (all-defined-out))

(define-syntax-rule (zap! func place args ...)
  (set! place (func place args ...)))

(define-syntax (w- stx)
  (as-non-syntax stx (fn-as-non-syntax stx cdr)
    (delisting body stx
           voidval
      var  var
      val  (if (: symbol? : non-syntax var)
             #`(: let ([#,var #,val]) : w- #,@body)
             #`(: begin #,var : w- #,val #,@body)))))

; NOTE: Racket already has something named 'do, and 'begin is too
; verbose.
(define-syntax-rule (du body ...)
  (begin voidval body ...))

(define-syntax-rule (du1 val body ...)
  (begin0 val voidval body ...))

(define-syntax-rule (ret var val body ...)
  (w- (var val)
    body ...
    var))

(define-syntax-rule (accum var body ...)
  (ret result null
    (w- (var (fn (elem) (push! elem result)))
      body ...)))

(define-syntax (namedlet stx)
  (: as-non-syntax stx stx : delisting binds-and-body stx
          (error)
    _     (error)
    next  (letfirst binds body (parse-magic-withlike binds-and-body)
            #`(let #,next #,binds #,@body))))

(define-syntax (magic-withlike stx)
  (: as-non-syntax stx stx : delisting binds-and-body stx
        (error)
    _   (error)
    op  (letfirst binds body (parse-magic-withlike binds-and-body)
          #`(#,op #,binds
              voidval
              #,@body))))

(define-syntax-rule (=fn var parms body ...)
  (set! var (fn parms body ...)))

(define-syntax-rule (named name body ...)
  (letrec ([name (du body ...)])
    name))

(define-syntax-rule (rfn name parms body ...)
  (named name (fn parms body ...)))

(define-syntax-rule (letr binds-and-body ...)
  (magic-withlike letrec binds-and-body ...))


; This is inspired by <http://blog.racket-lang.org/2008/02/
; dirty-looking-hygiene.html>.

(define-syntax-rule (w-stxparam binds-and-body ...)
  (magic-withlike syntax-parameterize binds-and-body ...))

(define-syntax-rule (longhand-w-anaphor
                      ([name transformer] ...) body ...)
  (syntax-parameterize
      ([name (make-rename-transformer #'transformer)] ...)
    body ...))

(define-syntax-rule (w-anaphor binds-and-body ...)
  (magic-withlike longhand-w-anaphor binds-and-body ...))

(define-syntax-parameter a idfn)
(define-syntax-parameter b idfn)

(define-syntax-rule (abfn body ...)
  (lambda (our-a our-b)
    (w-anaphor (a our-a b our-b)
      body ...)))

(define-syntax-parameter it idfn)

(define-syntax-rule (zapit! place body ...)
  (w- our-it place
    (w-anaphor it our-it
      (set! place (begin body ...)))))

(define-syntax-parameter next idfn)

(define-syntax-rule (longhand-nextlet binds body ...)
  (let our-next binds
    (w-anaphor (next our-next)
      body ...)))

(define-syntax-rule (nextlet binds-and-body ...)
  (magic-withlike longhand-nextlet binds-and-body ...))

(define-syntax-rule (nextfn parms body ...)
  (named our-next
    (w-anaphor next our-next
      (fn parms body ...))))


(define-syntax-rule (push! elem seq)
  (zapit! seq (cons elem it)))
