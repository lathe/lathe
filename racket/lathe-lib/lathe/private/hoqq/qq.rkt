#lang parendown racket

; qq.rkt
;
; Implementations of familiar quasiquotation operators in terms of
; higher quasiquotation. These aren't quite like their Racket versions
; in every respect, such as how many cons cells they generate.
; Instead, they err on the simpler side.

; NOTE: Just in case we want to switch back to `eq?` hashes, we refer
; to `equal?` hashes more explicitly.
(require #/for-meta 1 #/only-in racket [hash hashequal])

(require #/for-meta 1 "../../main.rkt")
(require #/for-meta 1 "util.rkt")
(require #/for-meta 1 "trees.rkt")
(require #/for-meta 1 "expanders.rkt")

(provide #/all-defined-out)


(define-syntax -quasiquote
  #/w- impl
    (lambda (stx is-bracket)
      (syntax-case stx () #/ (_ body)
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
      #/dissect
        (hoqq-tower-table #/hoqq-tower-ref-degree
          (hoqq-tower-fmap lower-brackets #/expectfn
            (hoqq-closing-bracket data liner
            #/hoqq-closing-hatch
              lower-spansig closing-brackets partial-span-step)
            (error "Expected each of the lowest-order closing brackets to be a hoqq-closing-bracket")
            closing-brackets)
          0)
        (list tabled-lower-brackets de-table)
      #/dissect
        (hoqq-tower-pair-ab tabled-lower-brackets upper-brackets)
        (list paired-brackets de-pair)
      #/careful-hoqq-closing-hatch (careful-hoqq-tower #/list)
        paired-brackets
      #/careful-hoqq-span-step
        (hoqq-tower-fmap paired-brackets #/expectfn
          (hoqq-closing-bracket data liner
          #/hoqq-closing-hatch
            lower-spansig closing-brackets partial-span-step)
          (error "Expected each of the overall closing brackets to be a hoqq-closing-bracket")
          lower-spansig)
      #/lambda (span-steps)
        (dissect (de-pair span-steps)
          (list tabled-lower-span-steps upper-span-steps)
        #/w- lower-span-steps (de-table tabled-lower-span-steps)
        ; We compose all the low-degree closing brackets.
        #/w- composed-lowest
          (hoqq-tower-zip-map
            lower-brackets
            (careful-hoqq-tower #/list lower-span-steps)
          #/lambda (lower-bracket span-steps)
            (expect lower-bracket
              (hoqq-closing-bracket data liner
              #/hoqq-closing-hatch (hoqq-tower #/list)
                closing-brackets
              #/hoqq-span-step sig func)
              (error "Expected each of the lowest-order closing brackets to be a hoqq-closing-bracket with no holes")
            #/w- result (func span-steps)
            #/careful-hoqq-span-step (careful-hoqq-tower #/list)
            #/dissectfn (hoqq-tower #/list)
              result))
        #/w- add-upper
          (lambda (lower-brackets)
            (hoqq-tower-merge lower-brackets upper-brackets
            #/lambda (a b)
              (error "Internal error")))
        #/escapable-expression
          ; We call their liners on them, call `func` with that, take
          ; its literal version, and modify it to add the
          ; `-quasiquote` call.
          (expect
            (func #/add-upper
            #/hoqq-tower-zip-map lower-brackets composed-lowest
            #/lambda (lower-bracket composed-lowest)
              (dissect lower-bracket
                (hoqq-closing-bracket data liner closing-hatch)
              #/liner composed-lowest))
            (escapable-expression literal expr)
            (error "Expected the instantiation of the partial span step to be an escapable-expression")
            #`#`(-quasiquote #,#,literal))
          ; We alter them so that their literal version is now equal
          ; to their expr version, we call `func` with that, and we
          ; take its literal version.
          (expect
            (func #/add-upper #/hoqq-tower-fmap composed-lowest
            #/dissectfn (hoqq-span-step (hoqq-tower #/list) func)
              (careful-hoqq-span-step (careful-hoqq-tower #/list)
              #/lambda (span-steps)
                (expect (func span-steps)
                  (escapable-expression literal expr)
                  (error "Expected the instantiation of each of the lowest-order closing brackets' partial span steps to be an escapable-expression")
                #/escapable-expression
                  expr
                  ; TODO: Signal an error or something instead of just
                  ; using an error sentinel value like this.
                  'SHOULD-NOT-BE-USED)))
            (escapable-expression literal expr)
            (error "Expected the instantiation of the partial span step to be an escapable-expression")
            literal))))
  #/syntax-and-bracket-syntax
    (lambda (stx)
      (expect (impl stx #f)
        (hoqq-closing-hatch (hoqq-tower #/list) (hoqq-tower #/list)
        #/hoqq-span-step (hoqq-tower #/list) func)
        (error "Expected a -quasiquote result that had no closing brackets or holes")
      #/expect (func #/careful-hoqq-tower #/list)
        (escapable-expression literal expr)
        (error "Expected a -quasiquote result that instantiated to an escapable-expression")
        expr))
    (lambda (stx) #/impl stx #t))

(define-syntax -unquote #/bracket-syntax #/lambda (stx)
  (syntax-case stx () #/ (_ body)
  #/expect (bracroexpand #'body)
    (hoqq-closing-hatch (hoqq-tower #/list)
      closing-brackets partial-span-step)
    (error "Expected the bracroexpansion result to be a hoqq-closing-hatch without holes")
  #/begin
    (hoqq-tower-each closing-brackets #/expectfn
      (hoqq-closing-bracket data liner
      #/hoqq-closing-hatch (hoqq-tower #/list)
        closing-brackets partial-span-step)
      (error "Expected the bracroexpansion result's closing brackets to have no holes beyond them"))
  #/w- key-body 'body
  ; TODO: come up with a better value for `bracket-data`.
  #/w- bracket-data (hashequal)
  #/careful-hoqq-closing-hatch (careful-hoqq-tower #/list)
    (careful-hoqq-tower #/list #/hashequal key-body
    #/careful-hoqq-closing-bracket bracket-data
      (expectfn (hoqq-span-step (hoqq-tower #/list) func)
        (error "Expected a liner input that was a hoqq-span-step")
        (careful-hoqq-span-step (careful-hoqq-tower #/list)
        #/lambda (span-steps)
          (expect (func span-steps)
            (escapable-expression literal expr)
            (error "Expected a liner input that instantiated to an escapable-expression")
          #/escapable-expression
            #`#`(-unquote #,#,literal)
            ; TODO: Signal an error or something instead of just using
            ; an error sentinel value like this.
            'SHOULD-NOT-BE-USED)))
    #/careful-hoqq-closing-hatch (careful-hoqq-tower #/list)
      closing-brackets partial-span-step)
  #/careful-hoqq-span-step
    (careful-hoqq-tower #/list #/hashequal key-body
    #/careful-hoqq-tower #/list)
  #/lambda (span-steps)
    (hoqq-span-step-instantiate
    #/hoqq-tower-ref span-steps 0 key-body)))

; TODO: Define `-unquote-splicing`.

; TODO: Define `-quasisyntax`, `-unsyntax`, and `-unsyntax-splicing`.
; Actually, we're already doing something like those with
; `-quasiquote`, so we should update `-quasiquote` so that it
; generates non-syntax data instead. Although `-unquote` and
; `-unsyntax` may be basically equivalent, let's consider treating
; them as different brackets that are arbitrarily incompatible with
; each other.


; TODO: Perhaps begin a new file for the rest of these.

; TODO: Define q-expression-building macros that help with expressing
; s-expressions:
;
;   (escape-s-expr s-expr)
;   (splice-s-expr s-exprs)
;
; (If the syntax monad were (Writer String) rather than s-expressions,
; we'd also want friendly character escapes for Unicode characters,
; friendly character escapes for brackets, whitespace normalization
; control, an escaped version of the escape character, and one or more
; weak parens that ensure their strong counterparts are inserted at
; the other side of the string if they're unmatched.)

; TODO: Define q-expression-building macros that permit the full
; range of q-expressions:
;
;   (bracket open/close degree s-expr)

; TODO: Define new q-expression-building macros that provide the
; ability to unwind all parens which don't match a specified name:
;
;   (define-bracket-label degree new-var existing-var s-expr)
;   (define-current-bracket-label degree new-var s-expr)
;   (unwind-up-to-bracket-label open/close degree existing-var s-expr)
;   (unwind-past-bracket-label open/close degree existing-var s-expr)
;
; Most of these are brackets except for `define-bracket-label` and
; `define-current-bracket-level`, which define variables whose lexical
; scope begins with the nearest opening bracket of the given degree
; and ends with all the nearest closing brackets of strictly lesser
; degree. All variable bindings defined within a single bracket family
; must be mutually consistent.

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
