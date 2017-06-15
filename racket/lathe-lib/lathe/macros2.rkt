#lang parendown racket

(require "main.rkt")

(provide (all-defined-out))

(require (for-meta 1 (only-in racket/list append-map)))
(require (for-meta 1 (only-in racket/match match)))


; TODO: Finish defining Racket s-expression syntaxes that initiate
; q-expressions: quasiquote, quasisyntax.
;
; TODO: Finish defining Racket s-expression syntaxes that consume
; s-expression-encoded q-expressions and imitate Racket's existing
; syntaxes:
;   (quasiquote-q
;     #s(qexpr-layer q-expr-as-s-expr
;         (#hasheq( (var1 . #s(splicing s-expr))
;                   (var2 . #s(non-splicing s-expr))
;                   ...))))
;   (quasisyntax-q #s(qexpr-layer q-expr-as-s-expr (#hasheq(...))))
;
; TODO: Define q-expression-building macros that loosely imitate
; Racket's existing syntaxes: quasiquote, unquote, unquote-splicing,
; quasisyntax, unsyntax, unsyntax-splicing.
;
; TODO: Define q-expression-building macros that help with expressing
; s-expressions:
;   (escape-s-expr s-expr)
;   (splice-s-expr s-exprs)
; (If the syntax monad were (Writer String) rather than s-expressions,
; we'd also want friendly character escapes for Unicode characters,
; friendly character escapes for brackets, whitespace normalization
; control, and an escaped version of the escape character, and one or
; more weak parens that ensure their strong counterparts are inserted
; at the other side of the string if they're unmatched.)
;
; TODO: Define q-expression-building macros that permit the full
; range of q-expressions:
;   (bracket open/close degree s-expr)
;
; TODO: Define new q-expression-building macros that provide the
; ability to unwind all parens which don't match a specified name:
;   (define-bracket-label degree new-var existing-var s-expr)
;   (define-current-bracket-label degree new-var s-expr)
;   (unwind-up-to-bracket-label open/close degree existing-var s-expr)
;   (unwind-past-bracket-label open/close degree existing-var s-expr)
; Most of these are brackets except for `define-bracket-label` and
; `define-current-bracket-level`, which define variables whose lexical
; scope begins with the nearest opening bracket of the given degree
; and ends with all the nearest closing brackets of strictly lesser
; degree. All variable bindings defined within a single bracket family
; must be mutually consistent.
;
;
; Almost all of these features mentioned have examples already in
; Cene, although some things are different in Cene:
;
;   - Cene doesn't have higher quasiquotation or reader macros yet.
;   - It doesn't have bracket labels that are bound at a deeper
;     s-expression location than where they're used
;   - It has some inconsistent quirks around labels' lexical scope
;     boundaries and whether to unwind "up to" or "past" a label's
;     introduction.
;
; Part of the goal of this Racket library is to work through all of
; those design topics as a proof of concept for improving Cene.



(begin-for-syntax
  ; TODO: Use `qexpr-hole`.
  (struct qexpr-hole (name fills) #:prefab)
  (struct qexpr-layer (qexpr fills) #:prefab)
  
  ; TODO: See if we'll use this.
  (define (qexpr? x)
    (or (qexpr-hole? x) (qexpr-layer? x)))
  
  ; TODO: Implement this for real.
  (define (bracroexpand stx)
    (datum->syntax stx
    #/qexpr-layer (qexpr-hole 'a #/list) #/list #/hasheq 'a 1))
  
  (define (hasheq-fmap hash func)
    (apply hasheq #/append-map
      (lambda (pair) #/match pair #/(cons k v) (list k #/func v))
      (hash->list hash)))
  
  (struct initiate-bracket-syntax (impl)
    ; TODO: Add a new struct property indicating this syntax's
    ; behavior as a q-expression-building macro.
    
    ; Calling an `initiate-bracket-syntax` as a Racket macro makes it
    ; run the bracroexpander.
    #:property prop:procedure
    (lambda (this stx)
      (match this
        [ (initiate-bracket-syntax impl)
          (match (impl stx)
            [ (qexpr-layer make-qexpr (list fills))
              (make-qexpr #/hasheq-fmap fills bracroexpand)]
            [_ (error "Expected an initiate-bracket-syntax result that was a qexpr-layer")])]
        [_ (error "Expected this to be an initiate-bracket-syntax")])))
  )

(define-syntax -quasiquote #/initiate-bracket-syntax #/lambda (stx)
  (syntax-case stx () #/ (_ body)
  #/qexpr-layer
    (lambda (fills) #`#/quasiquote-q #,#/hash-ref fills 'body)
  #/list #/hasheq 'body #'body))

; TODO: Implement this for real.
(define-syntax quasiquote-q #/lambda (stx)
  (syntax-case stx () #/ (_ #s(qexpr-layer body rests))
    #''(body rests)))

(define-syntax -quasisyntax #/initiate-bracket-syntax #/lambda (stx)
  (syntax-case stx () #/ (_ body)
  #/qexpr-layer
    (lambda (fills) #`#/quasisyntax-q #,#/hash-ref fills 'body)
  #/list #/hasheq 'body #'body))

; TODO: Implement this for real.
(define-syntax quasisyntax-q #/lambda (stx)
  (syntax-case stx () #/ (_ #s(qexpr-layer body rests))
    #''(body rests)))

; Test
(-quasiquote woo)
