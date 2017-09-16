#lang parendown racket

; holes.rkt
;
; Data structures and syntaxes for encoding the kind of higher-order
; holes that occur in higher quasiquotation.

(require "../../main.rkt")
(require "util.rkt")

(require #/only-in racket/hash hash-union)

(provide #/all-defined-out)


; ===== Representing higher-order holes in Racket syntax =============

; TODO: This is part of the way we intend to encode higher
; quasiquotation in Racket s-expressions once our higher
; quasiquotation macros have expanded. See if we'll actually use it
; that way. If we do, see if we should move this to another file.
(define-syntax call-stx #/lambda (stx)
  (syntax-case stx () #/ (_ func arg)
  ; TODO LATER: Disarm `arg`.
  ; TODO LATER: Remove any `'taint-mode` and `'certify-mode` syntax
  ; properties from `arg`.
  ; TODO LATER: Rearm the result.
  ; TODO LATER: Apply syntax properties to the result that correspond
  ; to the syntax properties of `arg`.
  ; TODO LATER: See if the above steps really are sufficient to
  ; simulate calling `func` as a syntax transformer.
  #/#'func #'arg))


; ===== Collections corresponding to higher-order holes ==============

(struct hoqq-siglike (tables)
  #:methods gen:equal+hash
  [
    (define (equal-proc a b recursive-equal?)
      (expect a (hoqq-siglike a-tables)
        (error "Expected a to be a hoqq-siglike")
      #/expect b (hoqq-siglike b-tables)
        (error "Expected b to be a hoqq-siglike")
      #/recursive-equal? a-tables b-tables))
    (define (hash-proc this recursive-equal-hash-code)
      (expect this (hoqq-siglike tables)
        (error "Expected this to be a hoqq-siglike")
      #/recursive-equal-hash-code tables))
    (define (hash2-proc this recursive-equal-secondary-hash-code)
      (expect this (hoqq-siglike tables)
        (error "Expected this to be a hoqq-siglike")
      #/recursive-equal-secondary-hash-code tables))]
  #:methods gen:custom-write
  [
    (define (write-proc this port mode)
      (expect this (hoqq-siglike tables)
        (error "Expected this to be a hoqq-siglike")
        
        (write-string "#<hoqq-siglike" port)
        (print-hoqq-siglike port mode this #/lambda (v)
          (print-for-custom port mode v))
        (write-string ">" port)))])

(define (print-hoqq-siglike port mode siglike print-v)
  (expect siglike (hoqq-siglike tables)
    (error "Expected siglike to be a hoqq-siglike")
  #/list-each tables #/lambda (table)
    (write-string " (" port)
    (hash-kv-each-sorted symbol<? table #/lambda (k v)
      (write-string "[" port)
      (print-for-custom port mode k)
      (print-v v)
      (write-string "]" port))
    (write-string ")" port)))

(define (careful-hoqq-siglike tables)
  (unless (list? tables)
    (error "Expected tables to be a list"))
  (list-each tables #/lambda (table)
    (unless (hasheq-immutable? table)
      (error "Expected table to be an immutable eq? hash"))
    (hash-kv-each table #/lambda (k v)
      (unless (symbol? k)
        (error "Expected k to be a symbol"))))
  (define (simplify tables)
    (expect tables (cons table tables) tables
    #/w- tables (simplify tables)
    #/mat tables (cons _ _) (cons table tables)
    #/if (hash-empty? table) (list) (list tables)))
  (hoqq-siglike #/simplify tables))

(define (hoqq-siglike-dkv-all siglike func)
  (expect siglike (hoqq-siglike tables)
    (error "Expected siglike to be a hoqq-siglike")
  #/list-kv-all tables #/lambda (degree table)
    (hash-kv-all table #/lambda (key value)
      (func degree key value))))

(define (hoqq-siglike-dkv-each siglike body)
  (hoqq-siglike-dkv-all #/lambda (d k v)
    (body d k v)
    #t)
  (void))

(define (hoqq-siglike-keys-eq? a b)
  (expect a (hoqq-siglike a-tables)
    (error "Expected a to be a hoqq-siglike")
  #/expect b (hoqq-siglike b-tables)
    (error "Expected b to be a hoqq-siglike")
  #/and (= (length a-tables) (length b-tables))
  #/list-all (map list a-tables b-tables)
  #/dissectfn (list a-table b-table)
    (hash-keys-eq? a-table b-table)))

(define (hoqq-siglike-zip-each a b body)
  (expect a (hoqq-siglike a-tables)
    (error "Expected a to be a hoqq-siglike")
  #/expect b (hoqq-siglike b-tables)
    (error "Expected b to be a hoqq-siglike")
  #/list-each (map list a-tables b-tables)
  #/dissectfn (list a-table b-table)
    (hash-kv-each a-table #/lambda (k a-v)
      (body a-v #/hash-ref b-table k))))

(define (hoqq-siglike-dkv-map-maybe siglike func)
  (expect siglike (hoqq-siglike tables)
    (error "Expected siglike to be a siglike")
  #/careful-hoqq-siglike
  #/list-kv-map tables #/lambda (degree table)
    (hasheq-kv-map-maybe table #/lambda (key value)
      (func degree key value))))

(define (hoqq-siglike-dkv-map siglike func)
  (expect siglike (hoqq-siglike tables)
    (error "Expected siglike to be a siglike")
  #/hoqq-siglike
  #/list-kv-map tables #/lambda (degree table)
    (hasheq-kv-map table #/lambda (key value)
      (func degree key value))))

(define (hoqq-siglike-fmap siglike func)
  (expect siglike (hoqq-siglike tables)
    (error "Expected siglike to be a siglike")
  #/hoqq-siglike
  #/list-fmap tables #/lambda (table) #/hasheq-fmap table func))

(define (hoqq-siglike-restrict original example)
  (hoqq-siglike-dkv-map-maybe original #/lambda (degree k v)
    (if (hoqq-siglike-has-key? example degree k)
      (list v)
      (list))))

(define (hoqq-siglike-merge as bs merge-v)
  (expect as (hoqq-siglike a-tables)
    (error "Expected as to be a siglike")
  #/expect bs (hoqq-siglike b-tables)
    (error "Expected bs to be a siglike")
  #/expect a-tables (cons a a-rest) bs
  #/expect b-tables (cons b b-rest) as
  #/hoqq-siglike
  #/cons (hash-union a b #:combine #/lambda (a b) #/merge-v a b)
  #/hoqq-siglike-merge a-rest b-rest))

(define (hoqq-siglike-merge-force as bs)
  (hoqq-siglike-merge as bs #/lambda (a b)
    (error "Expected the hole names of multiple bracroexpand calls to be mutually exclusive")))

(define (hoqq-siglike-has-degree? siglike degree)
  (expect siglike (hoqq-siglike tables)
    (error "Expected siglike to be a siglike")
  #/lt-length degree tables))

(define (hoqq-siglike-has-key? siglike degree key)
  (expect siglike (hoqq-siglike tables)
    (error "Expected siglike to be a siglike")
  #/and (lt-length degree tables)
  #/hash-has-key? (list-ref tables degree) key))

(define (hoqq-siglike-ref siglike degree k)
  (expect siglike (hoqq-siglike tables)
    (error "Expected siglike to be a siglike")
  #/hash-ref (list-ref tables degree) k))

(define (hoqq-siglike-values siglike)
  (expect siglike (hoqq-siglike tables)
    (error "Expected siglike to be a siglike")
  #/list-bind tables hash-values))


; ===== Signatures of expressions' higher-order holes ================

(define (hoqq-sig? x)
  (and (hoqq-siglike? x)
  #/hoqq-siglike-dkv-all x #/lambda (degree _ subsig)
    (and
      (hoqq-siglike? subsig)
      (not #/hoqq-siglike-has-degree? subsig degree)
      (hoqq-sig? subsig))))

(define (print-hoqq-sig port mode sig)
  (print-hoqq-siglike port mode sig #/lambda (subsig)
    (print-hoqq-sig port mode subsig)))

(define (hoqq-sig-eq? a b)
  (equal? a b))


; ===== Fake nodes for printing things with higher-order holes =======

(struct example (val)
  #:methods gen:custom-write
#/ #/define (write-proc this port mode)
  (expect this (example val)
    (error "Expected this to be an example")
    (write-string "#<example " port)
    (print-for-custom port mode val)
    (write-string ">" port)))


; ===== Computations parameterized by higher-order holes =============

(struct hoqq-producer (sig func)
  #:methods gen:custom-write
#/ #/define (write-proc this port mode)
  (expect this (hoqq-producer sig func)
    (error "Expected this to be a hoqq-producer")
    
    (write-string "#<hoqq-producer" port)
    (print-hoqq-sig port mode sig)
    (print-hoqq-producer-example port mode this)
    (write-string ">" port)))

(define (print-hoqq-producer-example port mode producer)
  (expect producer (hoqq-producer sig func)
    (error "Expected producer to be a hoqq-producer")
    
    (write-string " " port)
    (print-for-custom #/func #/careful-hoqq-carrier sig
    #/list-fmap sig #/lambda (table)
      (hasheq-fmap table #/lambda  (subsig)
        (careful-hoqq-producer subsig
        #/dissectfn (hoqq-carrier subsig producers)
          (example producers))))))

(define (careful-hoqq-producer sig func)
  (unless (hoqq-sig? sig)
    (error "Expected sig to be a well-formed hoqq sig"))
  (hoqq-producer sig #/lambda (carrier)
    (dissect carrier (hoqq-carrier carrier-sig producers)
    #/expect (hoqq-sig-eq? sig carrier-sig) #t
      (error "Expected a careful-hoqq-producer and the carrier it was given to have the same sig")
    #/func carrier)))

; TODO: See if we'll use this.
(define (hoqq-producer-instantiate producer)
  (expect producer (hoqq-producer sig func)
    (error "Expected producer to be a hoqq-producer")
  #/if (hoqq-siglike-has-degree? sig 0)
    (error "Tried to instantiate a hoqq-producer which still had holes in it")
  #/func #/hoqq-carrier (hoqq-siglike #/list) #/hoqq-siglike #/list))

; TODO: See if we should write some kind of `hoqq-producer-compose`
; that combines two hole-having data structures seamlessly. We could
; use this followed by `hoqq-producer-instantiate` to make calls in a
; certain sense.


; ===== Collections which can fill in higher-order holes =============

; TODO: See if we'll use this section.

(struct hoqq-carrier (sig producers)
  #:methods gen:custom-write
#/ #/define (write-proc this port mode)
  (expect this (hoqq-carrier sig producers)
    (error "Expected this to be a hoqq-carrier")
    
    (write-string "#<hoqq-carrier" port)
    (print-hoqq-sig port mode sig)
    (write-string " :" port)
    (print-hoqq-siglike port mode producers #/lambda (producer)
      (print-hoqq-producer-example port mode producer))
    (write-string ">" port)))

(define (careful-hoqq-carrier sig producers)
  (unless (hoqq-sig? sig)
    (error "Expected sig to be a well-formed hoqq sig"))
  (unless (hoqq-siglike? producers)
    (error "Expected producers to be a hoqq-siglike"))
  (unless (hoqq-siglike-keys-eq? sig producers)
    (error "Expected sig and producers to have the same keys"))
  (hoqq-siglike-zip-each sig producers #/lambda (subsig producer)
    (expect producer (hoqq-producer producer-subsig func)
      (error "Expected producer to be a hoqq-producer")
    #/unless (hoqq-sig-eq? subsig producer-subsig)
      (error "Expected the producers contained in a careful-hoqq-carrier to have sigs matching the overall sig")))
  (hoqq-carrier sig producers))


; ===== Bracroexpansion results ======================================


; A bracro takes a pre-bracroexpanded Racket s-expression as input,
; and it returns a `hoqq-producer-with-closing-brackets`. Most bracros
; will introduce zero closing brackets in their results, with the
; exception being operations that are dedicated to being closing
; brackets, such as unquote operations.
;
; Many bracros will call the bracroexpander again as they do their
; processing, in order to process subexpressions of their input that
; should be passed through almost directly into the output. When they
; call the bracroexpander this way, there is always the chance that
; the intermediate result contains closing brackets, due to closing
; bracket operations occurring deep within that subexpression.


; A pre-bracroexpanded Racket s-expression is the kind of s-expression
; a user maintains, where higher quasiquotation operations are still
; represented as a bunch of disjointed opening bracket and closing
; bracket operators like `-quasiquote` and `-unquote`.
;
; A post-bracroexpanded Racket s-expression is an s-expression where
; the nested structure of higher quasiquotation has been deduced from
; the bracket operators and re-encoded as a nested tree. This is the
; proper form to pass the rest of the work through Racket's
; macroexpander, since the encapsulation strategy Racket pursues in
; the orm of its syntax taint system assumes that the nesting of
; s-expression syntax corresponds to the lexical nesting of macro
; calls.


; An escapable expression is a data structure that can create one of
; two things:
;
;   - A post-bracroexpanded s-expression.
;
;   - An unencapsulated, pre-bracroexpanded Racket s-expression.
;
; The point is that these represent operations that come in handy as
; delimiters or formatters for syntax literals, such as an operator
; that acts as a string delimiter or a string escape sequence
; initiator. When they occur in a context that properly suppresses
; them, these operators can appear inside a string with the same
; syntax they have outside it but without being construed as anything
; other than chunks of syntax. This is handy for code generation,
; since it means we can write syntax literals that mention these
; operators without escaping each operator each time it occurs.


; The fields of `hoqq-producer-with-closing-brackets` are as follows:
;
;   `producer`: A `hoqq-producer` generating an escapable expression.
;
;   `closing-brackets`: A `hoqq-siglike` of `hoqq-closing-bracket`
;     values.
;
; The sigs of the `closing-brackets` values put together must match
; the sig of `producer`.
;
(struct hoqq-producer-with-closing-brackets
  (producer closing-brackets)
  #:methods gen:custom-write
#/ #/define (write-proc this port mode)
  (expect this
    (hoqq-producer-with-closing-brackets producer closing-brackets)
    (error "Expected this to be a hoqq-producer-with-closing-brackets")
    
    (write-string "#<hoqq-producer-with-closing-brackets" port)
    (print-all-for-custom port mode #/list producer closing-brackets)
    (write-string ">" port)))

(define
  (careful-hoqq-producer-with-closing-brackets
    producer closing-brackets)
  (expect producer (hoqq-producer sig func)
    (error "Expected producer to be a hoqq-producer")
  #/expect (hoqq-siglike? closing-brackets) #t
    (error "Expected closing-brackets to be a hoqq-siglike")
  #/expect (hoqq-siglike-keys-eq? sig closing-brackets) #t
    (error "Expected sig and closing-brackets to have compatible keys")
    
    (hoqq-siglike-zip-each sig closing-brackets
    #/lambda (subsig closing-bracket)
      (expect closing-bracket
        (hoqq-closing-bracket
          data degree outer-section inner-sections)
        (error "Expected closing-bracket to be a hoqq-closing-bracket")
      #/expect outer-section (hoqq-producer sig func)
        (error "Expected outer-section to be a hoqq-producer")
      #/expect (hoqq-sig-eq? subsig sig) #t
        (error "Expected producer and closing-brackets to have matching sigs")))
  #/hoqq-producer-with-closing-brackets producer closing-brackets))

; The fields of `hoqq-closing-bracket` are as follows:
;
;   `data`: Miscellaneous information about the bracket, potentially
;     including things like this:
;
;     ; TODO: Implement each part of this. Consult other design notes
;     ; first because this list was a rough reconstruction from
;     ; memory.
;
;     - A bracket strength, determining which of the brackets are
;       consumed when two brackets match and which of them continue on
;       to match with other brackets beyond.
;
;     - A label, to identify corresponding brackets by.
;
;     - Let bindings, allowing labels to be renamed as their lookups
;       cross this bracket.
;
;     - A macro to call when processing this set of matched brackets.
;
;   `degree`: The quasiquotation degree of the section bounded by this
;     closing bracket. A closing bracket can only match an opening
;     bracket of the same degree, and its label bindings would only
;     affect matchings between labels of that degree as well.
;
;   `outer-section`: A `hoqq-producer` generating an escapable
;     expression, corresponding to all content that could be enclosed
;     by this closing bracket's opening bracket if not for this
;     closing bracket being where it is.
;
;   `inner-sections`: A `hoqq-siglike` of
;     `hoqq-producer-with-closing-brackets` values. These inner
;     sections represent the parts that were nested inside of the
;     closing bracket's opening brackets in the pre-bracroexpanded
;     Racket s-expression, but which should ultimately be nested
;     outside of the closing bracket's enclosed section in the overall
;     post-bracroexpanded Racket s-expression.
;
; The sigs of the `inner-sections` values put together must match the
; sig of `outer-section`, and that sig they have in common must not
; have any hole with degree equal to or greater than `degree`.
;
; Although it seems to be an arbitrary choice (TODO: Is it?), we allow
; closing brackets to have no holes. This effectively lets the region
; outside the closing bracket continue all the way to the end of a
; document. If we instead required there to be at least one hole of
; the highest possible degree, then we could compute `degree` from the
; sig.
;
(struct hoqq-closing-bracket
  (data degree outer-section inner-sections)
  #:methods gen:custom-write
#/ #/define (write-proc this port mode)
  (expect this
    (hoqq-closing-bracket data degree outer-section inner-sections)
    (error "Expected this to be a hoqq-closing-bracket")
    
    (write-string "#<hoqq-closing-bracket" port)
    (print-all-for-custom port mode
    #/list data degree outer-section inner-sections)
    (write-string ">" port)))

(define
  (careful-hoqq-closing-bracket
    data degree outer-section inner-sections)
  (expect (exact-nonnegative-integer? degree) #t
    (error "Expected degree to be an exact nonnegative integer")
  #/expect outer-section (hoqq-producer sig func)
    (error "Expected outer-section to be a hoqq-producer")
  #/expect inner-sections (hoqq-siglike tables)
    (error "Expected inner-sections to be a hoqq-siglike")
  #/if (hoqq-siglike-has-degree? inner-sections degree)
    (error "Expected inner-sections to have sections only of degree less than the inner section's degree")
  #/expect (hoqq-siglike-keys-eq? sig inner-sections) #t
    (error "Expected outer-section and inner-sections to have corresponding keys")
    
    (hoqq-siglike-zip-each sig inner-sections
    #/lambda (subsig inner-section)
      (expect inner-section
        (hoqq-producer-with-closing-brackets producer closing-brackets)
        (error "Expected inner-section to be a hoqq-producer-with-closing-brackets")
      #/expect producer (hoqq-producer sig func)
        (error "Expected producer to be a hoqq-producer")
      #/expect (hoqq-sig-eq? subsig sig) #t
        (error "Expected outer-section and inner-sections to have matching sigs")))
  #/hoqq-closing-bracket data degree outer-section inner-sections))
