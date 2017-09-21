#lang parendown racket

; test.rkt
;
; Unit tests of the higher quasiquotation system, particularly the
; `-quasiquote` macro.

(require rackunit)
(require "qq.rkt")

; (We provide nothing from this module.)


; This takes something that might or might not be syntax, and it
; "de-syntaxes" it recursively.
(define (destx x)
  (syntax->datum #/datum->syntax #'foo x))


(begin-for-syntax #/print-syntax-width 10000)


(check-equal? (destx #/-quasiquote 1) 1
  "Quasiquoting a self-quoting literal")
(check-equal? (destx #/-quasiquote a) 'a "Quasiquoting a symbol")
(check-equal? (destx #/-quasiquote (a b c)) '(a b c)
  "Quasiquoting a list")
(check-equal? (destx #/-quasiquote (a (b c) z)) '(a (b c) z)
  "Quasiquoting a nested list")
(check-equal?
  (destx #/-quasiquote (a (b (-unquote 1)) z))
  '(a (b 1) z)
  "Unquoting a self-quoting literal")
(check-equal?
  (destx #/-quasiquote (a (b (-unquote 'c)) z))
  '(a (b c) z)
  "Unquoting a quoted symbol")
(check-equal?
  (destx #/-quasiquote (a (b (-unquote list)) z))
  `(a (b ,list) z)
  "Unquoting a variable")
(check-equal?
  (destx #/-quasiquote (a (b (-unquote (+ 1 2 3))) z))
  '(a (b 6) z)
  "Unquoting an expression")
(check-equal?
  (destx
  #/-quasiquote
    (a (b (-unquote #/-quasiquote #/1 2 #/-unquote #/+ 1 2 3)) z))
  '(a (b (1 2 6)) z)
  "Unquoting another quasiquotation")

(check-equal?
  (destx
  #/-quasiquote
    (a (b (-quasiquote #/1 #/-unquote #/+ 2 #/-unquote #/+ 1 2 3)) z))
  '(a (b (-quasiquote #/1 #/-unquote #/+ 2 6)) z)
  "Nesting quasiquotations")
