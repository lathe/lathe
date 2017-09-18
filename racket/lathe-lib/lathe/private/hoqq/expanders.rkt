#lang parendown racket

; expanders.rkt
;
; Syntax expanders for q-expression-building macros (aka bracros) as
; well as an implementation of bracroexpansion.

(require "../../main.rkt")
(require "util.rkt")
(require "trees.rkt")

(provide #/all-defined-out)




; This struct property indicates a syntax's behavior as a
; q-expression-building macro (aka a bracro).
(define-values (prop:q-expr-syntax q-expr-syntax? q-expr-syntax-ref)
  (make-struct-type-property 'q-expr-syntax))

(define (q-expr-syntax-maybe x)
  (if (q-expr-syntax? x)
    (list #/q-expr-syntax-ref x)
    (list)))

(struct bracket-syntax (impl)
  #:property prop:q-expr-syntax
  (lambda (this stx)
    (expect this (bracket-syntax impl)
      (error "Expected this to be a bracket-syntax")
    #/impl stx)))

(struct initiate-bracket-syntax (impl)
  
  ; Calling an `initiate-bracket-syntax` as a q-expression-building
  ; macro (aka a bracro) makes it call its implementation function
  ; directly.
  #:property prop:q-expr-syntax
  (lambda (this stx)
    (expect this (initiate-bracket-syntax impl)
      (error "Expected this to be an initiate-bracket-syntax")
    #/impl stx))
  
  ; Calling an `initiate-bracket-syntax` as a Racket macro makes it
  ; call its implementation function and then instantiate the
  ; resulting hole-free `hoqq-hatch` to create a post-bracroexpansion
  ; Racket s-expression. If the `hoqq-hatch` has any holes, there's an
  ; error.
  #:property prop:procedure
  (lambda (this stx)
    (expect this (initiate-bracket-syntax impl)
      (error "Expected this to be an initiate-bracket-syntax")
    #/expect (impl stx) (hoqq-hatch producer closing-brackets)
      (error "Expected an initiate-bracket-syntax result that was a hoqq-hatch")
    #/if (hoqq-spanlike-has-degree? closing-brackets 0)
      (error "Expected an initiate-bracket-syntax result with no higher quasiquotatoin holes")
    #/hoqq-producer-instantiate producer))
)



(define (bracroexpand-list stx lst)
  (if (syntax? lst)
    (bracroexpand-list lst #/syntax-e lst)
  #/match lst
    [(cons first rest)
    ; TODO: Support splicing.
    #/expect (bracroexpand first)
      (hoqq-hatch (hoqq-producer first-sig first-func)
        first-closing-brackets)
      (error "Expected a bracroexpansion result that was a hoqq-hatch")
    #/dissect (bracroexpand-list stx rest)
      (hoqq-hatch (hoqq-producer rest-sig rest-func)
        rest-closing-brackets)
    ; TODO: Instead of using `hoqq-spanlike-merge-force`, rename the
    ; keys so that they don't have conflicts.
    #/hoqq-hatch
      (hoqq-producer (hoqq-spanlike-merge-force first-sig rest-sig)
      #/lambda (producers)
        (expect
          (first-func
          #/hoqq-spanlike-restrict producers first-closing-brackets)
          (escapable-expression first-escaped first-expr)
          (error "Expected the hoqq-producer result to be an escapable-expression")
        #/expect
          (rest-func
          #/hoqq-spanlike-restrict producers rest-closing-brackets)
          (escapable-expression rest-escaped rest-expr)
          (error "Expected the hoqq-producer result to be an escapable-expression")
        #/escapable-expression
          #`(cons #,first-escaped #,rest-escaped)
        #/datum->syntax stx #/cons first-expr rest-expr))
    #/hoqq-spanlike-merge-force
      first-closing-brackets rest-closing-brackets]
    [(list) #/hoqq-hatch-simple #/datum->syntax stx lst]
    [_ #/error "Expected a list"]))

(define (bracroexpand stx)
  (match (syntax-e stx)
    [(cons first rest)
    #/expect (syntax-local-maybe first) (list local)
      (bracroexpand-list stx stx)
    #/expect (q-expr-syntax-maybe local) (list q-expr-syntax)
      (bracroexpand-list stx stx)
    #/q-expr-syntax local stx]
    ; TODO: We support lists, but let's also support vectors and
    ; prefabricated structs, like Racket's `quasiquote` and
    ; `quasisyntax` do.
    [_ #/hoqq-hatch-simple stx]))
