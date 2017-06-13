#lang parendown racket

(require "main.rkt")

(provide (all-defined-out))

(require (for-meta 1 (only-in racket/match match)))


; TODO: Define Racket s-expression syntaxes that initiate
; q-expressions: quasiquote, quasisyntax.
;
; TODO: Define Racket s-expression syntaxes that consume
; s-expression-encoded q-expressions and imitate Racket's existing
; syntaxes:
;   (quasiquote-q q-expr-as-s-expr
;     #hasheq(
;       (var1 . #(splicing s-expr))
;       (var1 . #(non-splicing s-expr))
;       ...))
;   (quasisyntax-q q-expr-as-s-expr #hasheq(...))
;
; TODO: Define q-expression-building macros that loosely imitate
; Racket's existing syntaxes: quasiquote, unquote, unquote-splicing,
; quasisyntax, unsyntax, unsyntax-splicing.
;
; TODO: Define q-expression-building macros that help with expressing
; s-expressions:
;   (escape-s-expr s-expr)
;   (escape-s-expr-splicing s-exprs)
; (If the syntax monad were (Writer String) rather than s-expressions,
; we'd also want friendly character escapes for Unicode characters,
; friendly character escapes for brackets, whitespace normalization
; control, and an escaped version of the escape character, and one or
; more weak parens that ensure their strong counterparts are inserted
; at the other side of the string if they're unmatched.)
;
; TODO: Define q-expression-building macros that permit the full
; range of q-expressions:
;   (bracket open/close degree optional-new-var s-expr)
;
; TODO: Define new q-expression-building macros that provide the
; ability to unwind all parens which don't match a specified name:
;   (define-bracket-label degree new-var existing-var s-expr)
;   (unwind-up-to-bracket-label
;     open/close degree optional-new-var existing-var s-expr)
;   (unwind-past-bracket-label
;     open/close degree optional-new-var existing-var s-expr)
; Most of these are brackets except for `define-bracket-label`, which
; is associated with the nearest opening bracket of the given degree.
; The presence of an `optional-new-var` parameter associated with a
; bracket means the variable is associated with that family of
; matching brackets for purposes of variables used inside that
; boundary. Brackets which bind different names will not match each
; other, but a bracket which defines no name may match as though its
; name matches. The variables defined by `define-bracket-label` have
; their lexical scope determined by the nearest family of matching
; brackets of the given higher quasiquotation degree, and all variable
; bindings created by a single bracket family must be mutually
; consistent.
;
; Almost all of these features mentioned have examples already in
; Cene, although Cene doesn't have higher quasiquotation or reader
; macros yet, it doesn't have bracket labels that are bound at a
; deeper s-expression location than where they're used, and it has
; some inconsistent quirks around labels' lexical scope boundaries and
; whether to unwind "up to" or "past" a label's introduction. Part of
; the goal of this Racket library is to work through all of those
; design topics as a proof of concept for improving Cene.
