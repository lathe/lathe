#lang parendown racket

; qq.rkt
;
; Implementations of familiar quasiquotation operators in terms of
; higher quasiquotation.

(require #/for-meta 1 "../../main.rkt")
(require #/for-meta 1 "trees.rkt")
(require #/for-meta 1 "expanders.rkt")

(provide #/all-defined-out)



; TODO: Implement `-quasiquote`. The code here is copied from
; ../../macros2.rkt. Our Implementation details may be much different
; here than they were there, and we may or may not need to
; implement `quasiquote-q`.
;
; We should see if we need to use `call-stx` from ./trees.rkt as well.
; If not, we probably won't miss it if we delete it.
;
#|

(define-syntax -quasiquote #/initiate-bracket-syntax #/lambda (stx)
  (syntax-case stx () #/ (_ body)
  #/w- g-body (gensym "body")
  #/careful-q-expr-layer
    (lambda (fills) #`#/quasiquote-q #,#/holes-ref fills 0 g-body)
  #/list
  #/hasheq g-body
  #/careful-q-expr-layer (lambda (fills) #/initiating-open 1 #'body)
  #/list))

; TODO: Implement this for real. This currently doesn't have splicing.
(define-syntax quasiquote-q #/lambda (stx)
  (syntax-case stx () #/ (_ body)
  #/dissect (syntax-e #'body) (q-expr-layer body rests)
  #/dissect (fill-out-holes 1 rests) (list rests)
    (struct foreign (val) #:prefab)
    (define (expand-qq s-expr)
      ; TODO: Implement splicing.
      (if (syntax? s-expr)
        (expand-qq #/syntax-e s-expr)
      #/match s-expr
        [(foreign s-expr) s-expr]
        [(cons first rest)
        #`#/cons #,(expand-qq first) #,(expand-qq rest)]
        [(list) #'#/list]
        [_ #`'#,s-expr]))
    (expand-qq #/body
    #/list
    #/hasheq-fmap rests #/dissectfn (q-expr-layer make-rest sub-rests)
      (careful-q-expr-layer
        (lambda (fills) #/foreign #/make-rest fills)
        sub-rests))))
|#

(define-syntax -unquote #/bracket-syntax #/lambda (stx)
  (syntax-case stx () #/ (_ body)
  #/w- g-body (gensym "body")
  #/expect (bracroexpand #'body)
    (hoqq-closing-hatch (hoqq-tower #/list) closing-brackets
      partial-span-step)
    (error "Expected the bracroexpansion result to be a hoqq-closing-hatch without holes")
  #/hoqq-closing-hatch (careful-hoqq-tower #/list)
    (careful-hoqq-tower #/list #/hasheq g-body
    #/hoqq-closing-hatch (careful-hoqq-tower #/list) closing-brackets
      partial-span-step)
  #/lambda (span-steps)
    (expect
      (hoqq-span-step-instantiate
      #/hoqq-tower-ref span-steps 0 g-body)
      (escapable-expression literal expr)
      (error "Expected the a span step's inputs to create escapable-expression values")
    #/escapable-expression
      #`(-unquote #,literal)
      expr)))
