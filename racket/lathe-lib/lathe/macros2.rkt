#lang parendown racket

(require "main.rkt")

(provide (all-defined-out))

(require
  (for-meta 1
    (only-in racket/hash hash-union)
    (only-in racket/list append-map make-list range split-at)
    (only-in racket/match match match-lambda)))


; TODO: Finish defining Racket s-expression syntaxes that initiate
; q-expressions: quasiquote, quasisyntax.
;
; TODO: Finish defining Racket s-expression syntaxes that consume
; s-expression-encoded q-expressions and imitate Racket's existing
; syntaxes:
;   (quasiquote-q
;     #<q-expr-layer q-expr-as-s-expr
;         (
;           #hasheq(
;                    (var1 .
;                      #<q-expr-layer
;                          #<lambda (fills) #s(splicing s-expr)>
;                          ()>)
;                    (var2 .
;                      #<q-expr-layer
;                          #<lambda (fills) #s(non-splicing s-expr)>
;                          ()>)
;                   ...))>)
;   (quasisyntax-q #<q-expr-layer q-expr-as-s-expr (#hasheq(...))>)
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
  
  (define (list-kv-map lst func)
    (map func (range #/length lst) lst))
  
  (define (list-fmap lst func)
    (map func lst))
  
  (define (list-bind lst func)
    (append-map func lst))
  
  (define (hasheq-kv-map hash func)
    (make-immutable-hasheq #/list-fmap (hash->list hash)
    #/match-lambda #/ (cons k v) #/cons k #/func k v))
  
  (define (hasheq-fmap hash func)
    (hasheq-kv-map hash #/lambda (k v) #/func v))
  
  (define (holes-values holes)
    (append-map (lambda (holes) #/hash-values holes) holes))
  
  (define (holes-dkv-map holes func)
    (list-kv-map holes #/lambda (degree holes)
    #/hasheq-kv-map holes #/lambda (key value)
    #/func degree key value))
  
  (define (holes-fmap holes func)
    (list-fmap holes #/lambda (holes) #/hasheq-fmap holes func))
  
  (define (holes-ref holes degree k)
    (hash-ref (list-ref holes degree) k))
  
  (define (print-for-custom value port mode)
    (if (eq? #t mode) (write value port)
    #/if (eq? #f mode) (display value port)
    #/print value port mode))
  
  (struct q-expr-layer (make-q-expr fills)
    #:methods gen:custom-write
  #/ #/define (write-proc this port mode)
    ; TODO: Remove this branch. It's kinda useful if we need to debug
    ; this write behavior itself.
    (if #f (write-string "#<q-expr-layer ?>" port)
    #/match this
      [ (q-expr-layer make-q-expr fills)
        
        (define (print-holes holes port mode)
          (for-each
            (lambda (holes)
              (write-string " " port)
              (define holes-list
                (append-map (match-lambda #/ (cons k v) #/list k v)
                #/sort (hash->list holes) symbol<? #:key car))
              (print-for-custom holes-list port mode))
            holes))
        
        (struct hole (degree key fills)
          #:methods gen:custom-write
        #/ #/define (write-proc this port mode)
          (match this
            [ (hole degree key fills)
              (write-string "#<hole " port)
              (print-for-custom key port mode)
              (write-string " " port)
              (print-for-custom degree port mode)
              (print-holes fills port mode)
              (write-string ">" port)]
            [_ #/error "Expected this to be a hole"]))
        
        (write-string "#<q-expr-layer" port)
        (print-holes fills port mode)
        (write-string " " port)
        (define body
          (make-q-expr #/holes-dkv-map fills
          #/lambda (degree key fill)
            (match fill
              [(q-expr-layer make-fill sub-fills)
              #/careful-q-expr-layer
                (lambda (fills) #/hole degree key fills)
                sub-fills]
              [_ #/error "Expected a fill that was a q-expr-layer"])))
        (print-for-custom body port mode)
        (write-string ">" port)]
      [_ #/error "Expected this to be a q-expr-layer"]))
  
  (define (hash-keys-eq? a b)
    (and (= (hash-count a) (hash-count b)) #/hash-keys-subset? a b))
  
  (define (holes-match? as bs)
    (define (verify-all-empty as)
      (match as
        [(cons a a-rest)
        #/and (hash-empty? a) #/verify-all-empty a-rest]
        [_ #t]))
    (match as
      [(cons a a-rest)
      #/match bs
        [(cons b b-rest)
        #/and (hash-keys-eq? a b) #/holes-match? a-rest b-rest]
        [_ #/verify-all-empty as]]
      [_ #/verify-all-empty bs]))
  
  (define (careful-q-expr-layer make-q-expr fills)
    (q-expr-layer
      (lambda (holes)
        (unless (holes-match? holes fills)
          (error "Expected holes and fills to match"))
        (make-q-expr holes))
      fills))
  
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
  (define-values (prop:q-expr-syntax q-expr-syntax? q-expr-syntax-ref)
    (make-struct-type-property 'q-expr-syntax))
  
  (define (q-expr-syntax-maybe x)
    (if (q-expr-syntax? x)
      (list #/q-expr-syntax-ref x)
      (list)))
  
  (define (simplify-holes holes)
    (match holes
      [(cons first rest)
      #/match (simplify-holes rest)
        [(list) #/if (hash-empty? first) (list) (list first)]]
      [_ holes]))
  
  (define (simplify-layer layer err)
    (match layer
      [(q-expr-layer make-q-expr fills)
      #/careful-q-expr-layer make-q-expr #/simplify-holes fills]
      [_ #/err]))
  
  (define (fill-out-holes n holes)
    (if (= 0 n)
      holes
    #/match holes
      [(cons first rest) #/cons first #/fill-out-holes (sub1 n) rest]
      [_ #/cons (hasheq) #/fill-out-holes (sub1 n) holes]))
  
  (define (fill-out-layer n layer err)
    (match (simplify-layer layer err)
      [(q-expr-layer make-q-expr fills)
      #/careful-q-expr-layer make-q-expr #/fill-out-holes n fills]
      [_ #/error "Internal error"]))
  
  (define (length-lte lst n)
    (if (< n 0)
      #f
    #/match lst
      [(cons first rest) #/length-lte rest #/sub1 n]
      [_ #t]))
  
  (define (fill-out-restrict-layer n layer err)
    (define result (fill-out-layer n layer err))
    (match result #/ (q-expr-layer make-q-expr fills)
    #/if (length-lte fills n)
      result
      (err)))
  
  (define (merge-holes as bs)
    (match as
      [(cons a a-rest)
      #/match bs
        [(cons b b-rest)
        #/cons
          (hash-union a b #:combine #/lambda (a b)
            (error "Expected the hole names of multiple bracroexpand calls to be mutually exclusive"))
        #/merge-holes a-rest b-rest]
        [_ as]]
      [_ bs]))
  
  (define (filter-hash hash example-hash)
    (make-immutable-hasheq #/list-bind (hash->list hash)
    #/match-lambda #/ (cons k v)
      (if (hash-has-key? example-hash k)
        (list #/cons k v)
        (list))))
  
  (define (filter-holes holes example-holes)
    (define (loop holes example-holes)
      (match holes
        [(cons hole hole-rest)
        #/match example-holes
          [(cons example-hole example-hole-rest)
          #/cons (filter-hash hole example-hole)
          #/filter-holes hole-rest example-hole-rest]
          [_ #/list]]
        [_ #/list]))
    (simplify-holes #/loop holes example-holes))
  
  (define (bracroexpand-list stx lst)
    (if (syntax? lst)
      (bracroexpand-list lst #/syntax-e lst)
    #/match lst
      [(cons first rest)
      ; TODO: Support splicing.
      #/match (bracroexpand first)
        [(q-expr-layer make-first first-holes)
        #/match (bracroexpand-list stx rest)
          [(q-expr-layer make-rest rest-holes)
          #/careful-q-expr-layer
            (lambda (fills)
              (datum->syntax stx
              #/cons
                (make-first #/filter-holes fills first-holes)
                (make-rest #/filter-holes fills rest-holes)))
          #/merge-holes first-holes rest-holes]
          [_ #/error "Internal error"]]
        [_ #/error "Expected a bracroexpand result that was a q-expr-layer"]]
      [(list)
      #/careful-q-expr-layer (lambda (fills) #/datum->syntax stx lst)
      #/list]
      [_ #/error "Expected a list"]))
  
  (define (bracroexpand stx)
    (match (syntax-e stx)
      [(cons first rest)
      #/match (syntax-local-maybe first)
        [(list local)
        #/match (q-expr-syntax-maybe local)
          [(list q-expr-syntax) #/q-expr-syntax local stx]
          [_ #/bracroexpand-list stx stx]]
        [_ #/bracroexpand-list stx stx]]
      ; TODO: We support lists, but let's also support vectors and
      ; prefabricated structs, like Racket's `quasiquote` and
      ; `quasisyntax` do.
      [_ #/careful-q-expr-layer (lambda (fills) stx) #/list]))
  
  (struct bracket-syntax (impl)
    #:property prop:q-expr-syntax
    (lambda (this stx)
      (match this
        [(bracket-syntax impl) (impl stx)]
        [_ #/error "Expected this to be a bracket-syntax"])))
  
  (struct initiating-open #/degree s-expr)
  
  (struct initiate-bracket-syntax (impl)
    
    ; Calling an `initiate-bracket-syntax` as a q-expression-building
    ; macro (aka a bracro) makes it run the bracroexpander
    ; recursively. If the intermediate bracroexpansion result has any
    ; holes, high-degree holes are propagated as holes in the return
    ; value, but low-degree holes are turned into
    ; `#<q-expr-layer ...>` nodes in the s-expression.
    #:property prop:q-expr-syntax
    (lambda (this stx)
      (match this
        [(initiate-bracket-syntax impl)
        #/match
          (fill-out-restrict-layer 1 (impl stx) #/lambda ()
            (error "Expected an initiate-bracket-syntax result that was a q-expr-layer with no more than one degree of fills"))
        #/ (q-expr-layer make-q-expr #/list fills)
           (struct bracroexpanded-fill #/
             degree
             make-q-expr
             low-degree-fills
             remaining-fills)
           (define bracroexpanded-fills
             (hasheq-fmap fills #/match-lambda
               [(q-expr-layer make-fill #/list)
               #/match (make-fill #/list)
                 [(initiating-open degree s-expr)
                 #/match
                   (fill-out-layer degree (bracroexpand s-expr)
                   #/lambda ()
                   #/error "Expected a bracroexpand result that was a q-expr-layer")
                 #/ (q-expr-layer make-q-expr fills)
                    (define-values
                      (low-degree-fills high-degree-fills)
                      (split-at fills degree))
                    (define remaining-fills
                      ; We merge the high-degree fills with the holes
                      ; that occur in the low-degree fills.
                      (foldl merge-holes
                        (append (make-list degree #/hasheq)
                          high-degree-fills)
                      #/list-fmap (holes-values low-degree-fills)
                      #/match-lambda #/
                        (q-expr-layer make-fill sub-fills)
                        sub-fills))
                    (bracroexpanded-fill
                      degree
                      make-q-expr
                      low-degree-fills
                      remaining-fills)]
                 [_ #/error "Expected an initiate-bracket-syntax result where each fill was an initiating-open"]]
               [_ #/error "Expected an initiate-bracket-syntax result where each fill was a q-expr-layer with no holes"]))
           (careful-q-expr-layer
             (lambda (fills)
               (make-q-expr #/list #/hasheq-fmap bracroexpanded-fills
               #/match-lambda #/
                 (bracroexpanded-fill
                   degree
                   make-q-expr
                   low-degree-fills
                   remaining-fills)
               #/careful-q-expr-layer
                 (lambda (sub-fills)
                   ; TODO: We used to merge `fills` in like this. See why.
;                   (make-q-expr #/merge-holes fills sub-fills))
                   (make-q-expr sub-fills))
               #/holes-fmap low-degree-fills #/match-lambda #/
                 (q-expr-layer make-fill sub-fills)
                 ; TODO: Let's make sure that every implementation of
                 ; `make-fill` returns another `q-expr-layer` rather
                 ; than an something like an s-expression. This seems
                 ; tricky. Will we want all the degrees of fills to
                 ; return `q-expr-layer` values or just the low
                 ; degrees? Once we begin to implement closing
                 ; brackets of higher degree than 0, we'll start to
                 ; see whether this is wrong. Right now, we only have
                 ; degree-0 closing brackets (unquotes).
                 (make-fill fills)))
           #/foldl merge-holes (list)
           #/hash-values #/hasheq-fmap bracroexpanded-fills
           #/match-lambda #/
             (bracroexpanded-fill
               degree
               make-q-expr
               low-degree-fills
               remaining-fills)
             remaining-fills)]
        [_ #/error "Expected this to be an initiate-bracket-syntax"]))
    
    ; Calling an `initiate-bracket-syntax` as a Racket macro makes it
    ; run the bracroexpander from start to finish on the body. If this
    ; overall bracroexpansion result has any holes, there's an error.
    #:property prop:procedure
    (lambda (this stx)
      (match this
        [(initiate-bracket-syntax impl)
        #/match
          (fill-out-restrict-layer 1 (impl stx) #/lambda ()
            (error "Expected an initiate-bracket-syntax result that was a q-expr-layer with no more than one degree of fills"))
        #/ (q-expr-layer make-q-expr #/list fills)
        #/make-q-expr #/list #/hasheq-fmap fills #/match-lambda
          [(q-expr-layer make-fill #/list)
          #/match (make-fill #/list)
            [(initiating-open degree s-expr)
            #/fill-out-restrict-layer degree (bracroexpand s-expr)
            #/lambda ()
              (error "Expected a bracroexpand result that was a q-expr-layer with no more than the specified number of degrees of holes")]
            [_ #/error "Expected an initiate-bracket-syntax result where each fill was an initiating-open"]]
          [_ #/error "Expected an initiate-bracket-syntax result where each fill was a q-expr-layer with no holes"]]
        [_ #/error "Expected this to be an initiate-bracket-syntax"]))
    
    )
  )

(define-syntax -quasiquote #/initiate-bracket-syntax #/lambda (stx)
  (syntax-case stx () #/ (_ body)
  #/let [#/g-body #/gensym "body"]
  #/careful-q-expr-layer
    (lambda (fills) #`#/quasiquote-q #,#/holes-ref fills 0 g-body)
  #/list
  #/hasheq g-body
  #/careful-q-expr-layer (lambda (fills) #/initiating-open 1 #'body)
  #/list))

; TODO: Implement this for real. This currently doesn't have splicing.
(define-syntax quasiquote-q #/lambda (stx)
  (syntax-case stx () #/ (_ body)
  #/match (syntax-e #'body) #/ (q-expr-layer body rests)
  #/match (fill-out-holes 1 rests) #/ (list rests)
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
    #/hasheq-fmap rests #/match-lambda #/
      (q-expr-layer make-rest sub-rests)
      (careful-q-expr-layer
        (lambda (fills) #/foreign #/make-rest fills)
        sub-rests))))

(define-syntax -unquote #/bracket-syntax #/lambda (stx)
  (syntax-case stx () #/ (_ body)
  #/let [#/g-body #/gensym "body"]
  #/careful-q-expr-layer
    (lambda (fills)
      (match (holes-ref fills 0 g-body)
        [(q-expr-layer make-fill #/list) #/make-fill #/list]
        [_ #/error "Expected a fill that was a q-expr-layer with no sub-fills"]))
  #/list
  #/hasheq g-body
  #/match (bracroexpand #'body) #/ (q-expr-layer make-q-expr fills)
  #/careful-q-expr-layer
    (lambda (fills)
      (let ([result (make-q-expr fills)])
      #/careful-q-expr-layer (lambda (fills-2) result) #/list))
    fills))

(define-syntax -quasisyntax #/initiate-bracket-syntax #/lambda (stx)
  (syntax-case stx () #/ (_ body)
  #/let [#/g-body #/gensym "body"]
  #/careful-q-expr-layer
    (lambda (fills) #`#/quasisyntax-q #,#/holes-ref fills 0 g-body)
  #/list
  #/hasheq g-body
  #/careful-q-expr-layer (lambda (fills) #/initiating-open 1 #'body)
  #/list))

; TODO: Implement this for real.
(define-syntax quasisyntax-q #/lambda (stx)
  (syntax-case stx () #/ (_ body)
  #/match (syntax-e #'body) #/ (q-expr-layer body rests)
    #`'(#,body #,rests)))


; Tests

(print-syntax-width 10000)

(-quasiquote (foo (bar baz) () qux))
(-quasiquote (foo (bar baz) (-quasiquote ()) qux))
(-quasiquote (foo (bar baz) (-unquote (* 1 123456)) qux))

; TODO: Fix this test. Right now, it has the result shown. The problem
; is that the implementation of `expand-qq` doesn't descend into
; `q-expr-layer` structs at all, which leaves behind an unprocessed
; `foreign` struct.
(-quasiquote
  (foo (-quasiquote (bar (-unquote (baz (-unquote (* 1 123456))))))))
#|

Result:

'(foo
   (quasiquote-q
     #<q-expr-layer
        (body1016
          #<q-expr-layer
             #<syntax
                (baz
                  #s(foreign
                      #<q-expr-layer #<syntax (* 1 123456)>>))>>)
        #<syntax (bar #<hole body1016 0>)>>))

Expected (roughly):

'(foo
   (quasiquote-q
     #<q-expr-layer (body1016 #<q-expr-layer #<syntax (baz 123456)>>)
        #<syntax (bar #<hole body1016 0>)>>))
|#
