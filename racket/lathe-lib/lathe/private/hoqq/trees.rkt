#lang parendown racket

; trees.rkt
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


; ===== Fake nodes for printing things with higher-order holes =======

(struct example (val)
  #:methods gen:custom-write
#/ #/define (write-proc this port mode)
  (expect this (example val)
    (error "Expected this to be an example")
    (write-string "#<example " port)
    (print-for-custom port mode val)
    (write-string ">" port)))


; ===== Low-order building block for higher qq spans and hatches =====

(struct hoqq-tower (tables)
  #:methods gen:equal+hash
  [
    (define (equal-proc a b recursive-equal?)
      (expect a (hoqq-tower a-tables)
        (error "Expected a to be a hoqq-tower")
      #/expect b (hoqq-tower b-tables)
        (error "Expected b to be a hoqq-tower")
      #/recursive-equal? a-tables b-tables))
    (define (hash-proc this recursive-equal-hash-code)
      (expect this (hoqq-tower tables)
        (error "Expected this to be a hoqq-tower")
      #/recursive-equal-hash-code tables))
    (define (hash2-proc this recursive-equal-secondary-hash-code)
      (expect this (hoqq-tower tables)
        (error "Expected this to be a hoqq-tower")
      #/recursive-equal-secondary-hash-code tables))]
  #:methods gen:custom-write
  [
    (define (write-proc this port mode)
      (expect this (hoqq-tower tables)
        (error "Expected this to be a hoqq-tower")
        
        (write-string "#<hoqq-tower" port)
        (hoqq-tower-print port mode this #/lambda (v)
          (print-for-custom port mode v))
        (write-string ">" port)))])

(define (hoqq-tower-print port mode tower print-v)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a hoqq-tower")
  #/list-each tables #/lambda (table)
    (write-string " (" port)
    (hash-kv-each-sorted symbol<? table #/lambda (k v)
      (write-string "[" port)
      (print-for-custom port mode k)
      (print-v v)
      (write-string "]" port))
    (write-string ")" port)))

(define (careful-hoqq-tower tables)
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
  (hoqq-tower #/simplify tables))

(define (hoqq-tower-dkv-all tower func)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a hoqq-tower")
  #/list-kv-all tables #/lambda (degree table)
    (hash-kv-all table #/lambda (key value)
      (func degree key value))))

(define (hoqq-tower-dkv-each tower body)
  (hoqq-tower-dkv-all #/lambda (d k v)
    (body d k v)
    #t)
  (void))

(define (hoqq-tower-keys-eq? a b)
  (expect a (hoqq-tower a-tables)
    (error "Expected a to be a hoqq-tower")
  #/expect b (hoqq-tower b-tables)
    (error "Expected b to be a hoqq-tower")
  #/and (= (length a-tables) (length b-tables))
  #/list-all (map list a-tables b-tables)
  #/dissectfn (list a-table b-table)
    (hash-keys-eq? a-table b-table)))

(define (hoqq-tower-zip-each a b body)
  (expect a (hoqq-tower a-tables)
    (error "Expected a to be a hoqq-tower")
  #/expect b (hoqq-tower b-tables)
    (error "Expected b to be a hoqq-tower")
  #/list-each (map list a-tables b-tables)
  #/dissectfn (list a-table b-table)
    (hash-kv-each a-table #/lambda (k a-v)
      (body a-v #/hash-ref b-table k))))

(define (hoqq-tower-dkv-map-maybe tower func)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a tower")
  #/careful-hoqq-tower
  #/list-kv-map tables #/lambda (degree table)
    (hasheq-kv-map-maybe table #/lambda (key value)
      (func degree key value))))

(define (hoqq-tower-dkv-map tower func)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a tower")
  #/hoqq-tower
  #/list-kv-map tables #/lambda (degree table)
    (hasheq-kv-map table #/lambda (key value)
      (func degree key value))))

(define (hoqq-tower-fmap tower func)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a tower")
  #/hoqq-tower
  #/list-fmap tables #/lambda (table) #/hasheq-fmap table func))

(define (hoqq-tower-restrict original example)
  (hoqq-tower-dkv-map-maybe original #/lambda (degree k v)
    (if (hoqq-tower-has-key? example degree k)
      (list v)
      (list))))

(define (hoqq-tower-merge as bs merge-v)
  (expect as (hoqq-tower a-tables)
    (error "Expected as to be a tower")
  #/expect bs (hoqq-tower b-tables)
    (error "Expected bs to be a tower")
  #/expect a-tables (cons a a-rest) bs
  #/expect b-tables (cons b b-rest) as
  #/hoqq-tower
  #/cons (hash-union a b #:combine #/lambda (a b) #/merge-v a b)
  #/hoqq-tower-merge a-rest b-rest))

(define (hoqq-tower-merge-force as bs)
  (hoqq-tower-merge as bs #/lambda (a b)
    (error "Expected the hole names of multiple bracroexpand calls to be mutually exclusive")))

(define (hoqq-tower-has-any-of-at-least-degree? tower degree)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a tower")
  #/lt-length degree tables))

(define (hoqq-tower-has-any-of-less-than-degree? tower degree)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a tower")
  #/let loop ([tables tables] [degree degree])
    (expect (nat-pred-maybe degree) (list degree) #f
    #/expect tables (cons table tables) #f
    #/expect (hash-empty? table) #t #t
    #/loop tables degree)))

(define (hoqq-tower-has-any? tower)
  (hoqq-tower-has-any-of-at-least-degree? tower 0))

(define (hoqq-tower-has-key? tower degree key)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a tower")
  #/and (lt-length degree tables)
  #/hash-has-key? (list-ref tables degree) key))

(define (hoqq-tower-ref tower degree k)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a tower")
  #/hash-ref (list-ref tables degree) k))

(define (hoqq-tower-values tower)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a tower")
  #/list-bind tables hash-values))


; ===== Signatures of higher quasiquotation spans and hatches ========

(define (hoqq-spansig? x)
  (and (hoqq-tower? x)
  #/hoqq-tower-dkv-all x #/lambda (degree _ subsig)
    (and
      (hoqq-tower? subsig)
      (not #/hoqq-tower-has-any-of-at-least-degree? subsig degree)
      (hoqq-spansig? subsig))))

(define (hoqq-hatchsig? x)
  (and (hoqq-tower? x)
  #/hoqq-tower-dkv-all x #/lambda (degree _ subsig)
    (and
      (hoqq-tower? subsig)
      (not #/hoqq-tower-has-any-of-less-than-degree? subsig degree)
      (hoqq-hatchsig? subsig))))

(define (hoqq-sig-print port mode sig)
  (hoqq-tower-print port mode sig #/lambda (subsig)
    (hoqq-sig-print port mode subsig)))

(define (hoqq-sig-eq? a b)
  (equal? a b))


; ===== Suspended computations over higher quasiquotation spans ======

(struct hoqq-span-step (sig func)
  #:methods gen:custom-write
#/ #/define (write-proc this port mode)
  (expect this (hoqq-span-step sig func)
    (error "Expected this to be a hoqq-span-step")
    
    (write-string "#<hoqq-span-step" port)
    (hoqq-sig-print port mode sig)
    (hoqq-span-step-print-example port mode this)
    (write-string ">" port)))

(define (hoqq-span-step-print-example port mode span)
  (expect span (hoqq-span-step sig func)
    (error "Expected span to be a hoqq-span-step")
    
    (write-string " " port)
    (print-for-custom #/func
    #/hoqq-tower-fmap sig #/lambda (subsig)
      (careful-hoqq-span-step subsig #/lambda (span-steps)
        (example span-steps)))))

(define (careful-hoqq-span-step sig func)
  (unless (hoqq-spansig? sig)
    (error "Expected sig to be a well-formed hoqq sig"))
  (hoqq-span-step sig #/lambda (span-steps)
    (unless (hoqq-tower? span-steps)
      (error "Expected span-steps to be a hoqq-tower"))
    (hoqq-tower-zip-each sig span-steps #/lambda (subsig span-step)
      (expect span-step (hoqq-span-step span-step-subsig func)
        (error "Expected span-step to be a span-step")
      #/expect (hoqq-sig-eq? subsig span-step-subsig) #t
        (error "Expected a careful-hoqq-span-step and the tower of span-steps it was given to have the same sig")))
    (func span-steps)))

(define (hoqq-span-step-instantiate span)
  (expect span (hoqq-span-step sig func)
    (error "Expected span to be a hoqq-span-step")
  #/if (hoqq-tower-has-any? sig)
    (error "Tried to instantiate a hoqq-span-step which still had holes in it")
  #/func #/hoqq-tower #/list))

; TODO: See if we should write some kind of `hoqq-span-step-compose`
; that combines two hole-having data structures seamlessly. We could
; use this followed by `hoqq-span-step-instantiate` to make calls in a
; certain sense.


; ===== Bracroexpansion results ======================================


; TODO: This data structure is actually rather wrong. As a guiding
; example, consider a pattern of closing brackets like this:
;
;    ,( ,( ) )  )  ,( )  )
;
; Currently it would be illegal to have ,() occur in the outer-section
; of ) like that. In fact, it would also be illegal for ) to appear in
; the outer-section of ) like that as well.
;
; Something we should notice about this is that if we prepend ,( to
; that pattern, it completes another closing bracket out of ). Because
; of this, we can't find all the nearest closing brackets of a certain
; degree (to match an opening bracket of that degree) without first
; having eliminated all the closing brackets of a lesser degree.
;
; So, the locality structure of that syntax, from root to tip of the
; representation, should be something like this for representing only
; the brackets themselves:
;
;    ,( ,( ) )  )  ,( )  )
;               A
;     .--------' '------.
;    C                   B
;     '.            .---'
;       D          E
;
; Before we get too carried away, we also have to represent the
; syntactic data in between those brackets. We don't necessarily have
; any full segments here, because they're all bordered by a closing
; bracket that might be closing a higher-degree hole with more of the
; segment on the other side. But we can see specific sections with
; specific degrees of closing bracket which could continue them:
;
;    ,( ,( ) )  )  ,( )  )
;   0  1_____ 0_ 0_  1 0_ 0
;         1
;
; Or perhaps...
;
;    ,( ,( ) )  )  ,( )  )
;   0___________ 0_______ 0
;      1_____        1
;         1
;
; The latter lets us associate each one of these partial segments
; one-for-one with the closing bracket it's outside of, except for one
; partial segment at the start with no closing bracket before it.
;
; Note that a partial segment of a certain degree can't have any
; unmatched closing brackets of a lesser degree.
;
; It seems this follows the opposite rule that completed higher
; quasiquotation structures do: In those, a hole can only have holes
; of its own if they're of strictly lower degree. In this case, a
; closing bracket can only have closing brackets after it if they're
; of equal or greater degree.
;
; Let's go with that rule. It seems to classify what's going on here
; very well.
;
; When we encode the data between the brackets, what we want is
; something we can use to *build* a `hoqq-span-step`, much like we use
; the `func` of a `hoqq-span-step` to build a post-bracroexpansion
; Racket s-expression. Let's encode it simply by using a very similar
; `func`.
;
; We will have no opening brackets in this format. When we
; bracroexpand a call to an opening bracket, we'll do so by
; eliminating it on the spot. We bracroexpand the body of the opening
; bracket call. If the lowest degree of closing bracket in the
; bracroexpanded body is of lower degree than the opening bracket,
; there's an error. Otherwise, we create a new bracroexpansion
; result that has the expanded body's closing brackets of higher
; degree than the opening bracket, as well as all the closing brackets
; of the partial segments *behind* the equal-degree closing brackets,
; because those are the brackets we've matched.
;
; The `func` of the new bracroexpansion result is a combination of the
; `func` of the bracroexpanded body (invoked to create the interior of
; a hole) and the `func` of each partial segment behind the
; equal-degree closing brackets (invoked to create the partial
; sections around the hole's holes). Some opening brackets may
; alternatively become closing brackets of a higher degree when they
; match up (such as ,( as an opening bracket), in which case that
; higher-degree closing bracket is one of the closing brackets in the
; result. And some may become opening brackets of a higher degree when
; they match up, in which case we repeat this process.


; A bracro takes a pre-bracroexpanded Racket s-expression as input,
; and it returns a `hoqq-closing-hatch`. Most bracros will introduce
; zero closing brackets in their results, with the exception being
; operations that are dedicated to being closing brackets, such as
; unquote operations.
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
;   `literal`: An unencapsulated, pre-bracroexpanded Racket
;     s-expression.
;
;   `expr`: A post-bracroexpanded s-expression.
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
;
(struct escapable-expression #/literal expr)


; A `hoqq-closing-hatch` represents a partial, inside-out portion of a
; higher quasiquotation data structure, seen as a branching collection
; of closing brackets and the ever-higher-order closing brackets
; beyond.
;
; The fields of `hoqq-closing-hatch` are as follows:
;
;   `span-step`: A `hoqq-span-step` generating an escapable
;     expression.
;
;   `closing-brackets`: A `hoqq-tower` of `hoqq-closing-bracket`
;     values. The section enclosed by each of these closing brackets
;     will be of degree one greater than its own degree, and its own
;     degree is its position in this `hoqq-seqlike`. Brackets
;     only interact with other brackets of the same enclosed section
;     degree.
;
; The sigs of the `closing-brackets` values put together must match
; the sig of `span`.
;
(struct hoqq-closing-hatch (span-step closing-brackets)
  #:methods gen:custom-write
#/ #/define (write-proc this port mode)
  (expect this (hoqq-closing-hatch span-step closing-brackets)
    (error "Expected this to be a hoqq-closing-hatch")
    
    (write-string "#<hoqq-closing-hatch" port)
    (print-all-for-custom port mode #/list span-step closing-brackets)
    (write-string ">" port)))

(define (careful-hoqq-closing-hatch span-step closing-brackets)
  (expect span-step (hoqq-span-step sig func)
    (error "Expected span-step to be a hoqq-span-step")
  #/expect (hoqq-tower? closing-brackets) #t
    (error "Expected closing-brackets to be a hoqq-tower")
  #/expect (hoqq-tower-keys-eq? sig closing-brackets) #t
    (error "Expected sig and closing-brackets to have compatible keys")
    
    (hoqq-tower-zip-each sig closing-brackets
    #/lambda (subsig closing-bracket)
      (expect closing-bracket
        (hoqq-closing-bracket data outer-section inner-sections)
        (error "Expected closing-bracket to be a hoqq-closing-bracket")
      #/expect outer-section (hoqq-span-step sig func)
        (error "Expected outer-section to be a hoqq-span-step")
      #/expect (hoqq-sig-eq? subsig sig) #t
        (error "Expected span-step and closing-brackets to have matching sigs")))
  #/hoqq-closing-hatch span-step closing-brackets))

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
;   `outer-section`: A `hoqq-span-step` generating an escapable
;     expression, corresponding to all content that could be enclosed
;     by this closing bracket's opening bracket if not for this
;     closing bracket being where it is.
;
;   `inner-sections`: A `hoqq-tower` of `hoqq-closing-hatch` values.
;     These inner sections represent the parts that were nested inside
;     of the closing bracket's opening brackets in the
;     pre-bracroexpanded Racket s-expression, but which should
;     ultimately be nested outside of the closing bracket's enclosed
;     section in the overall post-bracroexpanded Racket s-expression.
;
; The sigs of the `inner-sections` values put together must match the
; sig of `outer-section`.
;
; If a closing bracket has no holes, the enclosed region continues all
; the way to the end of the document.
;
(struct hoqq-closing-bracket (data outer-section inner-sections)
  #:methods gen:custom-write
#/ #/define (write-proc this port mode)
  (expect this (hoqq-closing-bracket data outer-section inner-sections)
    (error "Expected this to be a hoqq-closing-bracket")
    
    (write-string "#<hoqq-closing-bracket" port)
    (print-all-for-custom port mode
    #/list data outer-section inner-sections)
    (write-string ">" port)))

(define
  (careful-hoqq-closing-bracket data outer-section inner-sections)
  (expect outer-section (hoqq-span-step sig func)
    (error "Expected outer-section to be a hoqq-span-step")
  #/expect inner-sections (hoqq-tower tables)
    (error "Expected inner-sections to be a hoqq-tower")
  #/expect (hoqq-tower-keys-eq? sig inner-sections) #t
    (error "Expected outer-section and inner-sections to have corresponding keys")
    
    (hoqq-tower-zip-each sig inner-sections
    #/lambda (subsig inner-section)
      ; TODO: Combine these two `expect` patterns.
      (expect inner-section
        (hoqq-closing-hatch span-step closing-brackets)
        (error "Expected inner-section to be a hoqq-closing-hatch")
      #/expect span-step (hoqq-span-step sig func)
        (error "Expected span to be a hoqq-span-step")
      #/expect (hoqq-sig-eq? subsig sig) #t
        (error "Expected outer-section and inner-sections to have matching sigs")))
  #/hoqq-closing-bracket data outer-section inner-sections))

(define (hoqq-closing-hatch-simple val)
  (hoqq-closing-hatch
    (hoqq-span-step (hoqq-tower #/list) #/lambda (span-steps)
      (escapable-expression #`#'#,val val))
  #/hoqq-tower #/list))
