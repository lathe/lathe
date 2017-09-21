#lang parendown racket

; qq.rkt
;
; Implementations of familiar quasiquotation operators in terms of
; higher quasiquotation.

(require #/for-meta 1 "../../main.rkt")
(require #/for-meta 1 "util.rkt")
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

(define-syntax -quasiquote
  #/w- impl
    (lambda (stx is-bracket)
      (syntax-case stx () #/ (_ body)
      #/w- g-body (gensym "body")
      #/expect (bracroexpand #'body)
        (hoqq-closing-hatch (hoqq-tower #/list) body-closing-brackets
        #/hoqq-span-step sig func)
        (error "Expected the bracroexpansion result to be a hoqq-closing-hatch without holes")
      #/dissect
        (hoqq-tower-dkv-split-by body-closing-brackets
        #/lambda (d k v)
          (< 0 d))
        (list lower-brackets upper-brackets)
      ; We compute `closing-brackets` as a combination of the
      ; higher-degree closing brackets of `body-closing-brackets` and
      ; all the closing brackets under the lowest-degree closing
      ; brackets.
      #/w- closing-brackets
        (foldl hoqq-tower-merge-force upper-brackets
        #/list-fmap (hoqq-tower-values lower-brackets) #/expectfn
          (hoqq-closing-bracket data liner
          #/hoqq-closing-hatch
            lower-spansig closing-brackets partial-span-step)
          (error "Expected each of the lowest-order closing brackets to be a hoqq-closing-bracket")
          closing-brackets)
      #/careful-hoqq-closing-hatch (careful-hoqq-tower #/list)
        closing-brackets
      #/careful-hoqq-span-step
        (hoqq-tower-fmap closing-brackets #/expectfn
          (hoqq-closing-bracket data liner
          #/hoqq-closing-hatch
            lower-spansig closing-brackets partial-span-step)
          (error "Expected each of the overall closing brackets to be a hoqq-closing-bracket")
          lower-spansig)
      #/lambda (span-steps)
        ; We instantiate all the low-degree closing brackets and call
        ; `func` with that.
        (w- result
          (func #/hoqq-tower-fmap lower-brackets #/expectfn
            (hoqq-closing-bracket data liner
            #/hoqq-closing-hatch (hoqq-tower #/list) closing-brackets
            #/hoqq-span-step sig func)
            (error "Expected each of the lowest-order closing brackets to be a hoqq-closing-bracket with no holes")
            (w- composed-span-step
              (careful-hoqq-span-step (careful-hoqq-tower #/list)
              #/dissectfn (hoqq-tower #/list)
                (func
                #/hoqq-tower-restrict span-steps closing-brackets))
#|
            (expect
              (if is-bracket
                (liner partial-span-step)
                partial-span-step)
              (hoqq-span-step (hoqq-tower #/list) func)
              (error "Expected partial-span-step to be a hoqq-span-step")
            #/func #/hoqq-tower #/list))
|#
            #/if is-bracket
              (liner composed-span-step)
              composed-span-step))
        ; We modify it to add the `-quasiquote` call if appropriate.
        #/if is-bracket
          #`#`(-quasiquote #,#,result)
          result)))
  #/syntax-and-bracket-syntax
    (lambda (stx)
      (expect (impl stx #f)
        (hoqq-closing-hatch (hoqq-tower #/list) (hoqq-tower #/list)
        #/hoqq-span-step (hoqq-tower #/list) func)
        (error "Expected a -quasiquote result that had no closing brackets or holes")
      #/func #/careful-hoqq-tower #/list))
    (lambda (stx) #/impl stx #t))

(define-syntax -unquote #/bracket-syntax #/lambda (stx)
  (syntax-case stx () #/ (_ body)
  #/w- g-body (gensym "body")
  #/expect (bracroexpand #'body)
    (hoqq-closing-hatch (hoqq-tower #/list) closing-brackets
;      partial-span-step)
    #/hoqq-span-step sig func)
    (error "Expected the bracroexpansion result to be a hoqq-closing-hatch without holes")
  #/begin
    (hoqq-tower-each closing-brackets #/expectfn
      (hoqq-closing-bracket data liner
      #/hoqq-closing-hatch (hoqq-tower #/list)
        closing-brackets partial-span-step)
      (error "Expected the bracroexpansion result's closing brackets to have no holes beyond them"))
  ; TODO: come up with a better value for `bracket-data`.
  #/w- bracket-data (hasheq)
  #/careful-hoqq-closing-hatch (careful-hoqq-tower #/list)
    (careful-hoqq-tower #/list #/hasheq g-body
    #/careful-hoqq-closing-bracket bracket-data
      (expectfn (hoqq-span-step (hoqq-tower #/list) func)
        (error "Expected a liner input that was a hoqq-span-step")
        (careful-hoqq-span-step (careful-hoqq-tower #/list)
        #/lambda (span-steps)
          #`#`(-unquote #,#,#/func span-steps)))
    #/careful-hoqq-closing-hatch (careful-hoqq-tower #/list)
;      closing-brackets partial-span-step)
      closing-brackets
    #/careful-hoqq-span-step sig #/lambda (span-steps)
      (func span-steps))
  #/careful-hoqq-span-step
    (careful-hoqq-tower #/list #/hasheq g-body
    #/careful-hoqq-tower #/list)
  #/lambda (span-steps)
    (hoqq-span-step-instantiate
    #/hoqq-tower-ref span-steps 0 g-body)))
