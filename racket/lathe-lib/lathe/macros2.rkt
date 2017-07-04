#lang parendown racket

(require "main.rkt")

(provide (all-defined-out))

(require (for-meta 1 (only-in racket/hash hash-union)))
(require (for-meta 1 (only-in racket/match match match-lambda)))


; TODO: Finish defining Racket s-expression syntaxes that initiate
; q-expressions: quasiquote, quasisyntax.
;
; TODO: Finish defining Racket s-expression syntaxes that consume
; s-expression-encoded q-expressions and imitate Racket's existing
; syntaxes:
;   (quasiquote-q
;     #s(qexpr-layer q-expr-as-s-expr
;         (
;           #hasheq(
;                    (var1 .
;                      #s(qexpr-layer
;                          #<lambda (fills) #s(splicing s-expr)>
;                          ()))
;                    (var2 .
;                      #s(qexpr-layer
;                          #<lambda (fills) #s(non-splicing s-expr)>
;                          ()))
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
  (struct qexpr-layer (make-qexpr fills) #:prefab)
  
  (define (syntax-local-maybe identifier)
    (if (identifier? identifier)
      (let ()
        (define dummy #/list #/list)
        (define local
          (syntax-local-value identifier #/lambda () dummy))
        (if (eq? local dummy)
          (list)
          (list local)))
      (list)))
  
  ; This struct property indicates a syntax's behavior as a
  ; q-expression-building macro.
  (define-values (prop:qexpr-syntax qexpr-syntax? qexpr-syntax-ref)
    (make-struct-type-property 'qexpr-syntax))
  
  (define (qexpr-syntax-maybe x)
    (if (qexpr-syntax? x)
      (list #/qexpr-syntax-ref x)
      (list)))
  
  (define (simplify-holes holes)
    (match holes
      [(cons first rest)
      #/match (simplify-holes rest)
        [(list) #/if (hash-empty? first) (list) (list first)]]
      [_ holes]))
  
  (define (simplify-layer layer err)
    (match layer
      [(qexpr-layer qexpr fills)
      #/qexpr-layer qexpr #/simplify-holes fills]
      [_ #/err]))
  
  (define (fill-out-holes n holes)
    (if (= 0 n)
      holes
    #/match holes
      [(cons first rest) #/cons first #/fill-out-holes (- n 1) rest]
      [_ #/cons (hasheq) #/fill-out-holes (- n 1) holes]))
  
  (define (fill-out-layer n layer err)
    (match (simplify-layer layer err)
      [(qexpr-layer qexpr fills)
      #/qexpr-layer qexpr #/fill-out-holes n fills]
      [_ #/error "Internal error"]))
  
  (define (merge-holes as bs)
    (match as
      [(cons a as)
      #/match bs
        [(cons b bs)
        #/cons
          (hash-union a b #:combine #/lambda (a b)
            (error "Expected the hole names of multiple bracroexpand calls to be mutually exclusive"))
        #/merge-holes as bs]
        [_ as]]
      [_ bs]))
  
  (define (bracroexpand-list stx lst)
    (if (syntax? lst)
      (bracroexpand-list lst #/syntax-e lst)
    #/match lst
      [(cons first rest)
      ; TODO: Support splicing.
      #/match (bracroexpand first)
        [(qexpr-layer make-first first-holes)
        #/match (bracroexpand-list stx rest)
          [(qexpr-layer make-rest rest-holes)
          #/qexpr-layer
            (lambda (fills)
              (datum->syntax stx
              #/cons (make-first fills) (make-rest fills)))
          #/merge-holes first-holes rest-holes]
          [_ #/error "Internal error"]]
        [_ #/error "Expected a bracroexpand result that was a qexpr-layer"]]
      [(list)
      #/qexpr-layer (lambda (fills) #/datum->syntax stx lst) #/list]
      [_ #/error "Expected a list"]))
  
  (define (bracroexpand stx)
    (match (syntax-e stx)
      [(cons first rest)
      #/match (syntax-local-maybe first)
        [(list local)
        #/match (qexpr-syntax-maybe local)
          [(list qexpr-syntax) #/qexpr-syntax local stx]
          [_ #/bracroexpand-list stx stx]]
        [_ #/bracroexpand-list stx stx]]
      ; TODO: We support lists, but let's also support vectors and
      ; prefabricated structs, like Racket's `quasiquote` and
      ; `quasisyntax` do.
      [_ #/qexpr-layer (lambda (fills) stx) #/list]))
  
  (define (hasheq-kv-map hash func)
    (make-immutable-hasheq #/map
      (match-lambda #/ (cons k v) #/cons k #/func k v)
    #/hash->list hash))
  
  (define (hasheq-fmap hash func)
    (hasheq-kv-map hash #/lambda (k v) #/func v))
  
  (struct initiating-open #/degree sexpr)
  
  (struct initiate-bracket-syntax (impl)
    
    ; Calling an `initiate-bracket-syntax` as a q-expression-building
    ; macro makes it (TODO: Finish this sentence.).
    #:property prop:qexpr-syntax
    (lambda (this stx)
      (match this
        [(initiate-bracket-syntax impl)
        #/match
          (fill-out-layer 1 (impl stx) #/lambda ()
            (error "Expected an initiate-bracket-syntax result that was a qexpr-layer"))
          [ (qexpr-layer make-qexpr #/list fills)
            (define bracroexpanded-fills
              (hasheq-fmap fills #/match-lambda
                [(qexpr-layer make-fill #/list)
                #/match (make-fill #/list)
                  [(initiating-open degree sexpr)
                  ; TODO: Figure out if we should be using `degree`
                  ; for something additional here, like verifying that
                  ; the result has no more than `degree` degrees of
                  ; holes.
                  #/fill-out-layer degree (bracroexpand sexpr)
                  #/lambda ()
                    (error "Expected a bracroexpand result that was a qexpr-layer")]
                  [_ #/error "Expected an initiate-bracket-syntax result where each fill was an initiating-open"]]
                [_ #/error "Expected an initiate-bracket-syntax result where each fill was a qexpr-layer with no holes"]))
            (define bracroexpanded-makers
              (hasheq-fmap bracroexpanded-fills #/match-lambda
                [(qexpr-layer make-qexpr sub-fills) make-qexpr]
                [_ #/error "Expected a bracroexpand result that was a qexpr-layer"]))
            (define bracroexpanded-sub-fills
              (hasheq-fmap bracroexpanded-fills #/match-lambda
                [(qexpr-layer make-qexpr sub-fills) sub-fills]
                [_ #/error "Expected a bracroexpand result that was a qexpr-layer"]))
            (qexpr-layer
              (lambda (fills)
                ; TODO: See if we should transform some of the
                ; low-degree fills in some way, rather than just
                ; passing them all through like this. After all, there
                ; should be some way we account for the increased
                ; depth in these `initate-open` sections.
                (make-qexpr #/list #/hasheq-fmap bracroexpanded-makers
                #/lambda (make-qexpr) #/make-qexpr fills))
            #/foldl merge-holes (list)
            #/hash-values bracroexpanded-sub-fills)]
          [_ #/error "Expected an initiate-bracket-syntax result with no more than one degree of fills"]]
        [_ #/error "Expected this to be an initiate-bracket-syntax"]))
    
    ; Calling an `initiate-bracket-syntax` as a Racket macro makes it
    ; run the bracroexpander.
    #:property prop:procedure
    (lambda (this stx)
      (match this
        [(initiate-bracket-syntax impl)
        #/match
          (fill-out-layer 1 (impl stx) #/lambda ()
            (error "Expected an initiate-bracket-syntax result that was a qexpr-layer"))
          [(qexpr-layer make-qexpr #/list fills)
          #/make-qexpr #/list #/hasheq-fmap fills #/match-lambda
            [(qexpr-layer make-fill #/list)
            #/match (make-fill #/list)
              [(initiating-open degree sexpr)
              ; TODO: Figure out if we should be using `degree` for
              ; something additional here, like verifying that the
              ; result has no more than `degree` degrees of holes.
              #/fill-out-layer degree (bracroexpand sexpr) #/lambda ()
                (error "Expected a bracroexpand result that was a qexpr-layer")]
              [_ #/error "Expected an initiate-bracket-syntax result where each fill was an initiating-open"]]
            [_ #/error "Expected an initiate-bracket-syntax result where each fill was a qexpr-layer with no holes"]]
          [_ #/error "Expected an initiate-bracket-syntax result with no more than one degree of fills"]]
        [_ #/error "Expected this to be an initiate-bracket-syntax"]))
    
    )
  )

(define-syntax -quasiquote #/initiate-bracket-syntax #/lambda (stx)
  (syntax-case stx () #/ (_ body)
  #/qexpr-layer
    (lambda (fills)
      #`(quasiquote-q #,#/hash-ref (list-ref fills 0) 'body))
  #/list
  #/hasheq 'body
  #/qexpr-layer (lambda (fills) #/initiating-open 1 #'body) #/list))

; TODO: Implement this for real. This currently doesn't have unquotes,
; let alone splicing.
(define-syntax quasiquote-q #/lambda (stx)
;  (syntax-case stx () #/ (_ #s(qexpr-layer body rests))
;    #''(body rests)))
  (syntax-case stx () #/ (_ body)
  #/match (syntax-e #'body) #/ (qexpr-layer body rests)
  #/match (fill-out-holes 1 #/syntax-e rests) #/ (list rests)
    (struct foreign #/val)
    (define (expand-qq sexpr)
      ; TODO: Implement splicing.
      (if (syntax? sexpr)
        (expand-qq #/syntax-e sexpr)
      #/match sexpr
        [(foreign sexpr) sexpr]
        [(cons first rest)
        #`#/cons #,(expand-qq first) #,(expand-qq rest)]
        [(list) #'#/list]
        [_ #`'#,sexpr]))
    (expand-qq #/ (syntax-e body)
    #/list #/hasheq-fmap (syntax-e rests) foreign)))

(define-syntax -quasisyntax #/initiate-bracket-syntax #/lambda (stx)
  (syntax-case stx () #/ (_ body)
  #/qexpr-layer
    (lambda (fills)
      #`#(quasisyntax-q #,#/hash-ref (list-ref fills 0) 'body))
  #/list
  #/hasheq 'body
  #/qexpr-layer (lambda (fills) #/initiating-open 1 #'body) #/list))

; TODO: Implement this for real.
(define-syntax quasisyntax-q #/lambda (stx)
  (syntax-case stx () #/ (_ #s(qexpr-layer body rests))
    #''(body rests)))


; Tests

(-quasiquote (foo (bar baz) () qux))

; This test currently fails: The result of bracroexpanding
; (-quasiquote ()) is (quasiquote-q ()), but it should be of the form
; (quasiquote-q #s(qexpr-layer ...)).
(-quasiquote (foo (bar baz) (-quasiquote ()) qux))
