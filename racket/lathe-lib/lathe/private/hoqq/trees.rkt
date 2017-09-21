#lang parendown racket

; trees.rkt
;
; Data structures and syntaxes for encoding the kind of higher-order
; holes that occur in higher quasiquotation.

; NOTE: Just in case we want to switch back to `eq?` hashes, we refer
; to `equal?` hashes more explicitly.
(require #/only-in racket [hash hashequal])

(require #/for-meta -1 racket)
(require #/only-in racket/hash hash-union)

(require "../../main.rkt")
(require "util.rkt")

(provide #/all-defined-out)


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

(struct hoqq-tower-key-derived (a b)
  #:methods gen:equal+hash
  [
    (define (equal-proc a b recursive-equal?)
      (expect a (hoqq-tower-key-derived aa ab)
        (error "Expected a to be a hoqq-tower-key-derived")
      #/expect b (hoqq-tower-key-derived ba bb)
        (error "Expected b to be a hoqq-tower-key-derived")
      #/and
        (recursive-equal? aa ba)
        (recursive-equal? ab bb)))
    (define (hash-proc this recursive-equal-hash-code)
      (expect this (hoqq-tower-key-derived a b)
        (error "Expected this to be a hoqq-tower-key-derived")
      #/recursive-equal-hash-code #/list a b))
    (define (hash2-proc this recursive-equal-secondary-hash-code)
      (expect this (hoqq-tower-key-derived a b)
        (error "Expected this to be a hoqq-tower-key-derived")
      #/recursive-equal-secondary-hash-code #/list a b))]
  #:methods gen:custom-write
  [
    (define (write-proc this port mode)
      (expect this (hoqq-tower-key-derived a b)
        (error "Expected this to be a hoqq-tower-key-derived")
        
        (write-string "#<hoqq-tower-key-derived" port)
        (print-all-for-custom port mode #/list a b)
        (write-string ">" port)))])

(define (careful-hoqq-tower-key-derived a b)
  (unless (hoqq-tower-key? a)
    (error "Expected a to be a valid hoqq tower key"))
  (unless (hoqq-tower-key? b)
    (error "Expected b to be a valid hoqq tower key"))
  (hoqq-tower-key-derived a b))

(define (hoqq-tower-key? x)
  (or (symbol? x) (hoqq-tower-key-derived? x)))

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
          (write-string " " port)
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
    (unless (hashequal-immutable? table)
      (error "Expected table to be an immutable equal? hash"))
    (hash-kv-each table #/lambda (k v)
      (unless (hoqq-tower-key? k)
        (error "Expected k to be a valid tower key"))))
  (define (simplify tables)
    (expect tables (cons table tables) tables
    #/w- tables (simplify tables)
    #/mat tables (cons _ _) (cons table tables)
    #/if (hash-empty? table) (list) (list table)))
  (hoqq-tower #/simplify tables))

(define (hoqq-tower-dkv-all tower func)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a hoqq-tower")
  #/list-kv-all tables #/lambda (degree table)
    (hash-kv-all table #/lambda (key value)
      (func degree key value))))

(define (hoqq-tower-dkv-each tower body)
  (hoqq-tower-dkv-all tower #/lambda (d k v)
    (body d k v)
    #t)
  (void))

(define (hoqq-tower-each tower body)
  (hoqq-tower-dkv-each tower #/lambda (d k v)
    (body v)))

(define (hoqq-tower-keys-same? a b)
  (expect a (hoqq-tower a-tables)
    (error "Expected a to be a hoqq-tower")
  #/expect b (hoqq-tower b-tables)
    (error "Expected b to be a hoqq-tower")
  #/and (= (length a-tables) (length b-tables))
  #/list-zip-all a-tables b-tables #/lambda (a-table b-table)
    (hash-keys-same? a-table b-table)))

(define (hoqq-tower-zip-each a b body)
  (expect a (hoqq-tower a-tables)
    (error "Expected a to be a hoqq-tower")
  #/expect b (hoqq-tower b-tables)
    (error "Expected b to be a hoqq-tower")
  #/list-zip-each a-tables b-tables #/lambda (a-table b-table)
    (hash-kv-each a-table #/lambda (k a-v)
      (body a-v #/hash-ref b-table k))))

(define (hoqq-tower-zip-map a b func)
  (expect a (hoqq-tower a-tables)
    (error "Expected a to be a hoqq-tower")
  #/expect b (hoqq-tower b-tables)
    (error "Expected b to be a hoqq-tower")
  #/hoqq-tower
  #/list-zip-map a-tables b-tables #/lambda (a-table b-table)
    (hashequal-kv-map a-table #/lambda (k a-v)
      (func a-v #/hash-ref b-table k))))

(define (hoqq-tower-dkv-map-maybe tower func)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a tower")
  #/careful-hoqq-tower
  #/list-kv-map tables #/lambda (degree table)
    (hashequal-kv-map-maybe table #/lambda (key value)
      (func degree key value))))

(define (hoqq-tower-dkv-map tower func)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a tower")
  #/hoqq-tower
  #/list-kv-map tables #/lambda (degree table)
    (hashequal-kv-map table #/lambda (key value)
      (func degree key value))))

(define (hoqq-tower-fmap tower func)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a tower")
  #/hoqq-tower
  #/list-fmap tables #/lambda (table) #/hashequal-fmap table func))

(define (hoqq-tower-map-keys tower func)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a tower")
  #/hoqq-tower
  #/list-fmap tables #/lambda (table)
    (hashequal-kv-map-kv table #/lambda (k v)
      (list (func k) v))))

(define (hoqq-tower-dkv-split-by tower func)
  (w- tower
    (hoqq-tower-dkv-map tower #/lambda (d k v)
      (if (func d k v)
        (list (list) (list v))
        (list (list v) (list))))
  #/list
    (hoqq-tower-dkv-map-maybe tower #/lambda (d k v)
      (dissect v (list vf vt) vf))
    (hoqq-tower-dkv-map-maybe tower #/lambda (d k v)
      (dissect v (list vf vt) vt))))

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
  #/hoqq-tower-merge a-rest b-rest merge-v))

(define (hoqq-tower-prefix prefix tower)
  (unless (hoqq-tower-key? prefix)
    (error "Expected prefix to be a valid tower key"))
  (hoqq-tower-map-keys tower #/lambda (k)
    (careful-hoqq-tower-key-derived prefix k)))

(define (hoqq-tower-merge-prefix a-prefix as b-prefix bs)
  (unless (hoqq-tower-key? a-prefix)
    (error "Expected a-prefix to be a valid tower key"))
  (unless (hoqq-tower-key? b-prefix)
    (error "Expected b-prefix to be a valid tower key"))
  (unless (hoqq-tower? as)
    (error "Expected as to be a tower"))
  (unless (hoqq-tower? bs)
    (error "Expected bs to be a tower"))
  (hoqq-tower-merge
    (hoqq-tower-prefix a-prefix as)
    (hoqq-tower-prefix b-prefix bs)
  #/lambda (a b)
    (error "Internal error")))

(define (hoqq-tower-deprefix prefix tower)
  (unless (hoqq-tower-key? prefix)
    (error "Expected prefix to be a valid tower key"))
  (hoqq-tower-map-keys tower #/expectfn
    (hoqq-tower-key-derived found-prefix k)
    (error "Expected each key of tower to be a hoqq-tower-key-derived")
    (unless (equal? prefix found-prefix)
      (error "Expected each key of tower to have a certain prefix"))
    k))

(define (hoqq-tower-merge-ab as bs)
  (hoqq-tower-merge-prefix 'a as 'b bs))

(define (hoqq-tower-table table-of-towers)
  (unless (hashequal-immutable? table-of-towers)
    (error "Expected table-of-towers to be an immutable equal? hash"))
  (w- table-of-prefixed-towers
    (hashequal-kv-map table-of-towers #/lambda (k v)
      (unless (hoqq-tower-key? k)
        (error "Expected each key of table-of-towers to be a valid tower key"))
      (unless (hoqq-tower? v)
        (error "Expected each value of table-of-towers to be a hoqq-tower"))
      (hoqq-tower-prefix k v))
  #/w- merged
    (foldl
      (lambda (a b)
        (hoqq-tower-merge a b #/lambda (a b)
          (error "Internal error")))
      (careful-hoqq-tower #/list)
    #/hash-values table-of-prefixed-towers)
  #/list merged #/lambda (corresponding)
    (unless (hoqq-tower? corresponding)
      (error "Expected corresponding to be a hoqq-tower"))
    (unless (hoqq-tower-keys-same? merged corresponding)
      (error "Expected corresponding to be a hoqq-tower with the same keys as the hoqq-tower-pair-prefix merged result"))
    (hashequal-kv-map table-of-prefixed-towers #/lambda (k v)
      (hoqq-tower-deprefix k #/hoqq-tower-restrict corresponding v))))

(define (hoqq-tower-pair-ab as bs)
  (dissect (hoqq-tower-table #/hashequal 'a as 'b bs)
    (list merged de-table)
  #/list merged #/lambda (corresponding)
    (unless (hoqq-tower? corresponding)
      (error "Expected corresponding to be a hoqq-tower"))
    (unless (hoqq-tower-keys-same? merged corresponding)
      (error "Expected corresponding to be a hoqq-tower with the same keys as the hoqq-tower-pair-prefix merged result"))
    (w- table (de-table corresponding)
    #/list (hash-ref table 'a) (hash-ref table 'b))))

(define (hoqq-tower-has-any-of-at-least-degree? tower degree)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a tower")
  #/lt-length degree tables))

(define (hoqq-tower-has-any-of-less-than-degree? tower degree)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a tower")
  #/nextlet tables tables degree degree
    (expect (nat-pred-maybe degree) (list degree) #f
    #/expect tables (cons table tables) #f
    #/expect (hash-empty? table) #t #t
    #/next tables degree)))

(define (hoqq-tower-merge-by-degrees-maybe lower upper)
  (expect lower (hoqq-tower tables)
    (error "Expected lower to be a tower")
  #/if (hoqq-tower-has-any-of-less-than-degree? upper #/length tables)
    (list)
  #/list #/hoqq-tower-merge lower upper #/lambda (a b)
    (error "Internal error")))

(define (hoqq-tower-has-any? tower)
  (hoqq-tower-has-any-of-at-least-degree? tower 0))

(define (hoqq-tower-has-key? tower degree key)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a tower")
  #/and (lt-length degree tables)
  #/hash-has-key? (list-ref tables degree) key))

(define (hoqq-tower-ref-degree tower degree)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a tower")
  #/expect (exact-nonnegative-integer? degree) #t
    (error "Expected degree to be an exact nonnegative integer")
  #/expect (lt-length degree tables) #t (hashequal)
  #/list-ref tables degree))

(define (hoqq-tower-ref tower degree k)
  (w- table (hoqq-tower-ref-degree tower degree)
  #/expect (hoqq-tower-key? k) #t
    (error "Expected k to be a valid tower key")
  #/expect (hash-has-key? table k) #t
    (error "Expected k to be a key existing in the tower")
  #/hash-ref table k))

(define (hoqq-tower-values tower)
  (expect tower (hoqq-tower tables)
    (error "Expected tower to be a tower")
  #/list-bind tables hash-values))


; ===== Signatures of higher quasiquotation spans ====================

; NOTE: We call this "spansig" in particular to leave open the
; possibility that we'll want to represent sigs that have a different
; restriction in place of
; `(not #/hoqq-tower-has-any-of-at-least-degree? subsig degree)`.
;
; At one point, hatches looked like they might need their own kind of
; sig to express the way they become higher-degree (rather than
; lower-degree) toward the leaves, but this didn't turn out to be
; needed.
;
(define (hoqq-spansig? x)
  (and (hoqq-tower? x)
  #/hoqq-tower-dkv-all x #/lambda (degree _ subsig)
    (and
      (hoqq-tower? subsig)
      (not #/hoqq-tower-has-any-of-at-least-degree? subsig degree)
      (hoqq-spansig? subsig))))

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
    (print-all-for-custom port mode #/list #/func
    #/hoqq-tower-fmap sig #/lambda (subsig)
      (careful-hoqq-span-step subsig #/lambda (span-steps)
        ; TODO: Hmm, this seems to be a mess. Shouldn't we be
        ; instantiating the span-steps or something?
        (w- result (example span-steps)
        #/escapable-expression result result)))
    (write-string ">" port)))

(define (careful-hoqq-span-step sig func)
  (unless (hoqq-spansig? sig)
    (error "Expected sig to be a well-formed span sig"))
  (hoqq-span-step sig #/lambda (span-steps)
    (unless (hoqq-tower? span-steps)
      (error "Expected span-steps to be a hoqq-tower"))
    (hoqq-tower-zip-each sig span-steps #/lambda (subsig span-step)
      (expect span-step (hoqq-span-step span-step-subsig func)
        (error "Expected span-step to be a hoqq-span-step")
      #/expect (hoqq-sig-eq? subsig span-step-subsig) #t
        (error "Expected a careful-hoqq-span-step and the tower of span-steps it was given to have the same sig")))
  #/func span-steps))

(define (hoqq-span-step-instantiate span-step)
  (expect span-step (hoqq-span-step sig func)
    (error "Expected span-step to be a hoqq-span-step")
  #/if (hoqq-tower-has-any? sig)
    (error "Tried to instantiate a hoqq-span-step which still had holes in it")
  #/func #/hoqq-tower #/list))

; TODO: See if we should write some kind of `hoqq-span-step-compose`
; that combines two hole-having data structures seamlessly. We could
; use this followed by `hoqq-span-step-instantiate` to make calls in a
; certain sense.


; ===== Bracroexpansion results ======================================


; TODO: We've already completed the redesign described in the below
; TODO, so it's out of date. Rewrite its examples to be documentation
; now.
;
;
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


; A bracro is a new kind of syntax transformer which may discover
; unmatched closing brackets as it expands. It's useful for
; representing operators like `unquote` which themselves represent
; closing brackets. Opening brackets such as `quasiquote` may be
; bracros that process the closing brackets they find. Other bracros
; may simply forward along whatever closing brackets exist in their
; bodies without either adding to them or subtracting from them.
;
; Intervals beget intervals. When we match up an opening parenthesis
; with a closing parenthesis in a stream of text, we get a sequence of
; content in between, and that content may contain its own concentric
; parentheses that form a nesting of s-expressions. When we match up
; `quasiquote` and `unquote` in an s-expression, we get a particular
; area of syntax that gets quoted, while everything "outside" the
; `quasiquote` and "inside" the `unquote` is unaffected. In the Scheme
; family (including Racket, but not Common Lisp), one matching set of
; quasiquote/unquote may appear entirely within the quoted section of
; another, nesting much like s-expressions.
;
;   Nested s-expressions:    (  (  )  )
;
;   Nested quasiquotation:  `( `( ,( ,( ) ) ) )
;
; In general, intervals are ordered by containment, and an ordering is
; enough to define another kind of interval by specifying a collection
; of upper and lower bounds. So, wherever we have a notion that
; lexical syntax has an ordering to it (such as the ordering of text
; or imperative commands), we might be inspired to make it more
; structured using intervals (such as parentheses, indentation levels,
; loops, expressions, and string literals) and intervals of those
; intervals (such as lambdas, interpolated strings, and
; quasiquotations).
;
; Macro systems in Lisp dialects (including Common Lisp as well as
; many Scheme dialects such as Racket) typically dedicate to being
; reader macros which process text streams, or regular old macros
; which process s-expressions. Both exist in a single language,
; duplicating effort and leading to imparities between features
; supported. Racket has a highly developed hygienic systems in support
; of defining custom syntaxes over s-expressions, but Racket's reader
; extensions are still very unhygienic; the hygiene information Racket
; bundles with each node of its s-expressions can't be similarly
; bundled into each node of its text streams. I'm not aware of any
; existing language where `unquote` can be defined as a macro at all,
; and Racket's interactions between `quasiquote` and `quasisyntax`
; reveal that those operations' respective support for nested
; quasiquotation has been unhygienically hardcoded for their own
; purposes, as neither of those operations respect the other's nesting
; structure.
;
;   Racket's `quasiqyntax` doesn't let `quasiquote` nest: #`'#,0
;
;   Racket's `quasiquote` doesn't let `quasisyntax` nest: `#',0
;
; If even `unquote` isn't user-definable or hygienic, there's little
; hope for user-defined syntaxes over higher degrees of lexical
; structure.
;
; Bracros generalize all those degrees of lexical structure into a
; single system. To support each degree, we need to let users
; specify closing brackets of a degree one less, and indeed bracros
; can expand into closing brackets of any degree.
;
; Our implementation of bracros for Racket takes a Racket syntax value
; as input, so the minimum degree a bracro can discover is that of
; `unquote`. (We are intentionally not supporting the lower degree of
; closing parentheses just yet, because we're trying to demonstrate
; bracros and experiment with them in the context of Racket's hygiene
; features.) A bracro returns a `hoqq-closing-hatch` value to
; represent the structure of all the closing brackets it has found.
;
; We call all those intervals "spans" in this system. When we
; represent a span as data, it has a well-defined root node, but it
; span-shaped holes at the leaves. (A single span can represent an
; interval of any degree higher than the span's highest-degree hole.)
; In order to double-check that the shape of those holes interlocks
; properly with other data we're using, we tag our spans with "sigs,"
; which are signatures that are comprised of a list of hashes of
; progressively higher-degree sigs. In a span, a hole of degree 0
; can't have any holes of its own, a hole of degree 1 can have only
; degree-0 holes, and so on, so a sig gets more complex toward its
; higher degrees.
;
; A sig is recursively specified as a list of hashes of
; degree-constrained sigs, but one layer of that recursion is also
; useful. We refer to a list of hashes as a "tower" and manipulate it
; with the `hoqq-tower` struct. A sig is represented directly as a
; `hoqq-tower` struct containing other `hoqq-tower` structs, ad
; infinitum.
;
; To recover traditional tree-shaped data from a span, the holes must
; be filled in using a tower of other spans of compatible shapes. The
; original span and the tower of spans passed to it automatically
; interleave, each filling in the other, until what remains is a
; well-founded tree that data can be constructed by. All we use
; span-shaped data for, so far, is constructing tree-shaped data this
; way, so we have a `hoqq-span-step` structure that's simply comprised
; of a sig and a function. We instantiate a `hoqq-span-step` into a
; tree-shaped result by calling its function with a tower of other
; `hoqq-span-step` structures, at which point the function can
; instantiatte those arguments itself and construct its result from
; theirs.
;
; We've now described enough to explain the way we've implemented the
; structure of a `hoqq-closing-hatch`. A hatch is a sig describing
; the shape of its holes (of low degree), a tower of
; `hoqq-closing-bracket` structs representing the closing brackets
; it's found (which must be of higher degree than any of the holes),
; and a `hoqq-span-step` representing all the syntactic data that
; comes in between the root and those holes and brackets. Each
; `hoqq-closing-bracket` contains some arbitrary information
; describing what kind of bracket it is, along with another hatch
; representing the area beyond the closing bracket (such as an
; unquoted expression).

; TODO: Change `hoqq-closing-hatch` to leak less information to the
; caller and to do less overeager traversal into the syntax:
;
;   - Be shy. After a closing bracket of degree 0 (yes, specifically
;     0), do no bracroexpansion. This way, bracro definitions
;     occurring before that closing bracket can be invoked after it,
;     much like defining a reader macro at a REPL.
;
;   - In a similar spirit, don't expand further-away matchups of
;     degree N until the nearer ones are expanded. This means we'll
;     have to represent degree-N matchups using opening brackets after
;     all. (It's up to each bracro that processes degree-N closing
;     brackets to decide in what order it should then bracroexpand the
;     next-further brackets of degree N and the nearby brackets of
;     degree N+1. That said, bracros should usually expand all the
;     degree-N brackets before the degree-N+1 brackets to be sure the
;     degree-N+1 operator names are user-overridable using degree-N+1
;     lexical bindings.)
;
;   - Only show the lowest degree of closing brackets in the initial
;     result. This is just a byproduct of the last point, since we
;     won't know that any bracro call of a higher degree is a closing
;     bracket if we don't interpret it yet.
;
; TODO: Make `liner` more like a span sig or a tower of span sigs, and
; write a function that composes a span sig with a compatible liner.
;
; TODO: Separate `partial-span-step` into `span-step-of-literal` and
; `span-step-of-expr`. That way, we can throw errors instead of
; putting error sentinel values in `escapable-expression`.
;
;
; Altogether, the structs would probably look like this:
;
;   (struct hoqq-closing-hatch #/
;     lower-spansig degree table-of-closing-brackets
;     table-of-opening-brackets span-step-of-literal
;     span-step-of-expr)
;   (struct hoqq-closing-bracket #/data sig liner pre-b-expr)
;   (struct hoqq-opening-bracket #/
;     macro-name data sig degree table-of-opening-brackets liner
;     span-step-of-literal span-step-of-expr)
;
; The `pre-b-expr` at degree 0 is a Racket expression. At other
; degrees, it's this `hoqq-closed-hatch` tree derived from a previous
; `hoqq-opening-bracket` tree, and the sig must match the sig expected
; by the bracro caller:
;
;   (struct hoqq-closed-hatch #/
;     macro-name data sig degree table-of-closed-hatches
;     juxtaposition-arena-over-table-of-closed-hatches)
;   (struct hoqq-juxtaposition-arena-zero-or-one #/)
;   (struct hoqq-juxtaposition-arena-two-or-more #/
;     
;     ; By tracking the degree and making sure it's never accepted
;     ; below a certain number, we can make sure the juxtapositions
;     ; are representable in concrete syntax, rather than going down
;     ; to the -1000th degree to finally give an ordering to an
;     ; until-then unordered pair.
;     ;
;     ; TODO: Right now these struct designs make it impossible to
;     ; represent a truly unordered pair. See if we want that. This
;     ; way is more faithful to Racket syntax, but a more serious
;     ; advantage it has is that we don't have to think about states
;     ; in between ordered and unordered -- particularly, a state of
;     ; unknown ordering which can only be resolved by discovering
;     ; knowledge in an unforeseeable place.
;     ;
;     degree
;     
;     free-vars
;     table-of-juxtaposition-seats
;     juxtaposition-arena-over-table-of-juxtaposition-seats)
;   (struct hoqq-juxtaposition-seat #/
;     table-of-juxtaposition-arenas
;     var)
;
;
; TODO: Perhaps we'll want to use the `hoqq-juxtaposition-` operations
; to change span steps into something more traversable. On the other
; hand, this added structure could leak information to the caller.


; The `escapable-expression` struct is the final layer of complexity
; in the bracroexpansion process. It contains two Racket syntax
; values, either of which could be useful depending on whether the
; caller is an operation that quotes or does not quote its body:
;
;   `literal`: A Racket expression that when executed would return
;     another Racket syntax value which was similar to the bracro call
;     itself. Callers that quote their argument will pay attention to
;     this result.
;
;   `expr`: A Racket syntax value that is the end result of full
;     bracroexpansion. Callers that do not quote their argument will
;     pay attention to this result.
;
; Like quotation operators, quasiquotation operators will also use the
; result in the `literal` slot. In order to see to it that certain
; places in that result interpolate unquoted expressions, a
; quasiquotation operator will instantiate its root span step using
; carefully constructed span steps as arguments. The custom arguments'
; `escapable-expression` results will have `literal` slots that
; contain Racket expressions taken from the `expr` slots of the
; unquoted expressions.
;
; It's because of such interpolation that we need this double result
; in the first place. The quasiquotation operator must use
; bracroexpansion to detect where the unquotes appear in its body
; because unquote operations are bracros themselves, and users may
; define custom synonyms and abstractions of them. Hence, the bracros
; in the quasiquotation's body are expanded -- they're just expanded
; to an alternative literal representation that almost always
; resembles the original bracro calls. The `escapable-expression`
; struct carries that alternative along with the regular result.
;
(struct escapable-expression (literal expr)
  #:methods gen:custom-write
#/ #/define (write-proc this port mode)
  (expect this (escapable-expression literal expr)
    (error "Expected this to be an escapable-expression")
    
    (write-string "#<escapable-expression" port)
    (print-all-for-custom port mode #/list literal expr)
    (write-string ">" port)))


; A `hoqq-closing-hatch` represents a partial, inside-out portion of a
; higher quasiquotation data structure, seen as a branching collection
; of closing brackets and the potentially ever-higher-order closing
; brackets beyond.
;
; The fields of `hoqq-closing-hatch` are as follows:
;
;   `lower-spansig`: The signature of the low-degree holes in this
;     hatch. In a sense these indicate the shape of the closing
;     brackets that close this hatch itself, rather than the closing
;     brackets that occur inside it.
;
;   `closing-brackets`: A `hoqq-tower` of `hoqq-closing-bracket`
;     values, representing the nearest set of closing brackets in this
;     hatch. Note that only the lowest occurring degree of closing
;     brackets is *necessarily* nearest; the higher degrees of closing
;     brackets are only *possibly* nearest if the lowest-degree ones
;     don't match up to become their own higher-degree closing
;     bracket.
;
;   `partial-span-step`: A `hoqq-span-step` which has holes for all
;     the `lower-spansig` entries as well as holes for all the
;     `closing-brackets` entries.
;
; The minimum degree occupied in `closing-brackets` must exceed the
; maximum degree occupied in `lower-spansig`.
;
; In the `closing-brackets` tower, the degree of each closing bracket
; must be in between greater than closing bracket's own maximum
; degree of `lower-spansig` and less than or equal to its minimum
; degree of `closing-brackets`.
;
; The sigs of the `lower-spansig` and `closing-brackets` put together
; must match the sig of `partial-span-step`.
;
(struct hoqq-closing-hatch
  (lower-spansig closing-brackets partial-span-step)
  #:methods gen:custom-write
#/ #/define (write-proc this port mode)
  (expect this
    (hoqq-closing-hatch
      lower-spansig closing-brackets partial-span-step)
    (error "Expected this to be a hoqq-closing-hatch")
    
    (write-string "#<hoqq-closing-hatch" port)
    (print-all-for-custom port mode
    #/list closing-brackets partial-span-step)
    (write-string ">" port)))

(define
  (careful-hoqq-closing-hatch
    lower-spansig closing-brackets partial-span-step)
  (unless (hoqq-spansig? lower-spansig)
    (error "Expected lower-spansig to be a well-formed span sig"))
  (expect partial-span-step (hoqq-span-step span-step-sig func)
    (error "Expected partial-span-step to be a hoqq-span-step")
  #/expect (hoqq-tower? closing-brackets) #t
    (error "Expected closing-brackets to be a hoqq-tower")
  #/w- upper-spansig
    (hoqq-tower-dkv-map closing-brackets
    #/lambda (degree key closing-bracket)
      (expect closing-bracket
        (hoqq-closing-bracket data liner
        #/hoqq-closing-hatch
          subsig closing-brackets partial-span-step)
        (error "Expected closing-bracket to be a hoqq-closing-bracket")
      #/if (hoqq-tower-has-any-of-at-least-degree? subsig degree)
        (error "Expected closing-bracket to have no holes of its own degree or greater")
      #/if
        (hoqq-tower-has-any-of-less-than-degree?
          closing-brackets degree)
        (error "Expected closing-bracket to lead only to more closing brackets of its own degree or greater")
        subsig))
  #/expect
    (hoqq-tower-merge-by-degrees-maybe lower-spansig upper-spansig)
    (list spansig)
    (error "Expected lower-spansig to have a maximum degree less than the minimum degree of closing-brackets")
  #/expect (hoqq-sig-eq? spansig span-step-sig) #t
    (error "Expected lower-spansig and closing-brackets to be compatible with the sig of partial-span-step")
  #/hoqq-closing-hatch
    lower-spansig closing-brackets partial-span-step))

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
;   `liner`: A function which takes a `hoqq-span-step` value and
;     returns it augmented with the syntax for this closing bracket.
;     The sig of the input should be equal to the `lower-spansig` of
;     the `closing-hatch`, and the sig of the output should be the
;     same.
;
;   `closing-hatch`: A `hoqq-closing-hatch` corresponding to all
;     content that could be part of this closing bracket's enclosed
;     region if not for this closing bracket being where it is.
;
(struct hoqq-closing-bracket (data liner closing-hatch)
  #:methods gen:custom-write
#/ #/define (write-proc this port mode)
  (expect this (hoqq-closing-bracket data liner closing-hatch)
    (error "Expected this to be a hoqq-closing-bracket")
    
    (write-string "#<hoqq-closing-bracket" port)
    (print-all-for-custom port mode #/list data liner closing-hatch)
    (write-string ">" port)))

(define (careful-hoqq-closing-bracket data liner closing-hatch)
  (expect closing-hatch
    (hoqq-closing-hatch
      lower-spansig closing-brackets partial-span-step)
    (error "Expected closing-hatch to be a hoqq-closing-hatch")
  #/hoqq-closing-bracket data
    (lambda (span-step)
      (expect span-step (hoqq-span-step given-sig func)
        (error "Expected span-step to be a span-step")
      #/expect (hoqq-sig-eq? lower-spansig given-sig) #t
        (error "Expected span-step to have the same sig as closing-hatch")
      #/liner span-step))
    closing-hatch))

(define (hoqq-closing-hatch-simple val)
  (careful-hoqq-closing-hatch
    (careful-hoqq-tower #/list)
    (careful-hoqq-tower #/list)
  #/careful-hoqq-span-step (careful-hoqq-tower #/list)
  #/lambda (span-steps)
    (escapable-expression #`#'#,val val)))
