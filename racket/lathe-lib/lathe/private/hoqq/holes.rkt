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
  #/list-all (map list a-tables b-tables)
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
        #/dissectfn (hoqq-carrier subsig contents)
          (example contents))))))

(define (careful-hoqq-producer sig func)
  (unless (hoqq-sig? sig)
    (error "Expected sig to be a well-formed hoqq sig"))
  (hoqq-producer sig #/lambda (carrier)
    (dissect carrier (hoqq-carrier carrier-sig contents)
    #/expect (hoqq-sig-eq? sig carrier-sig) #t
      (error "Expected a careful-hoqq-producer and the carrier it was given to have the same sig")
    #/func carrier)))


; ===== Collections which can fill in higher-order holes =============

(struct hoqq-carrier (sig contents)
  #:methods gen:custom-write
#/ #/define (write-proc this port mode)
  (expect this (hoqq-carrier sig contents)
    (error "Expected this to be a hoqq-carrier")
    
    (write-string "#<hoqq-carrier" port)
    (print-hoqq-sig port mode sig)
    (write-string " :" port)
    (print-hoqq-siglike port mode contents #/lambda (producer)
      (print-hoqq-producer-example port mode producer))
    (write-string ">" port)))

(define (careful-hoqq-carrier sig contents)
  (unless (hoqq-sig? sig)
    (error "Expected sig to be a well-formed hoqq sig"))
  (unless (hoqq-siglike? contents)
    (error "Expected contents to be a hoqq-siglike"))
  (unless (hoqq-siglike-keys-eq? sig contents)
    (error "Expected sig and contents to have the same keys"))
  (hoqq-siglike-zip-each sig contents #/lambda (subsig contents-entry)
    (expect contents-entry (hoqq-producer contents-subsig func)
      (error "Expected the careful-hoqq-carrier contents to be hoqq-producer values")
    #/unless (hoqq-sig-eq? subsig contents-subsig)
      (error "Expected the careful-hoqq-carrier contents to have sigs matching the overall sig")))
  (hoqq-carrier sig contents))
