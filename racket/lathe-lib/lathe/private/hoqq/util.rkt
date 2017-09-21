#lang parendown racket

; util.rkt
;
; Miscellaneous utilities

(require "../../main.rkt")

(provide #/all-defined-out)


(define (nat-pred-maybe n)
  (unless (exact-nonnegative-integer? n)
    (error "Expected n to be an exact nonnegative integer"))
  (if (= n 0)
    (list)
    (list #/sub1 n)))


(define (list-kv-map lst func)
  (map func (range #/length lst) lst))

(define (list-fmap lst func)
  (map func lst))

(define (list-bind lst func)
  (append-map func lst))

(define (list-each lst body)
  (for-each body lst))

(define (list-kv-all lst func)
  (andmap func (range #/length lst) lst))

(define (list-all lst func)
  (andmap func lst))

(define (list-zip-map a b func)
  (list-fmap (map list a b) #/dissectfn (list a b) #/func a b))

(define (list-zip-all a b func)
  (list-all (map list a b) #/dissectfn (list a b) #/func a b))

(define (list-zip-each a b body)
  (list-each (map list a b) #/dissectfn (list a b) #/body a b))

(define (length-lte lst n)
  (if (< n 0)
    #f
  #/expect lst (cons first rest) #t
  #/length-lte rest #/sub1 n))

(define (lt-length n lst)
  (not #/length-lte lst n))


(define (hasheq-immutable? x)
  ((and/c hash? hash-eq? immutable?) x))

(define (hasheq-kv-map-maybe hash func)
  (make-immutable-hasheq #/list-bind (hash->list hash)
  #/dissectfn (cons k v)
    (match (func k v)
      [(list) (list)]
      [(list v) (list #/cons k v)]
      [_ (error "Expected the func result to be a maybe")])))

(define (hasheq-kv-map hash func)
  (hasheq-kv-map-maybe hash #/lambda (k v) #/list #/func k v))

(define (hasheq-fmap hash func)
  (hasheq-kv-map hash #/lambda (k v) #/func v))

(define (hash-keys-eq? a b)
  (and (= (hash-count a) (hash-count b)) #/hash-keys-subset? a b))

(define (hasheq-restrict original example)
  (hasheq-kv-map-maybe original #/lambda (k v)
    (if (hash-has-key? example k)
      (list v)
      (list))))

(define (hash-kv-each hash body)
  (list-each (hash->list hash) #/dissectfn (cons k v)
    (body k v)))

(define (hash-kv-each-sorted key<? hash body)
  (list-each (sort (hash->list hash) key<? #:key car)
  #/dissectfn (cons k v)
    (body k v)))

(define (hash-kv-all hash func)
  ; TODO: See if there's a more efficient way to do this when the
  ; match occurs early in the list. We shouldn't have to construct the
  ; whole list in that case.
  (list-all (hash->list hash) #/dissectfn (cons k v) #/func k v))


(define (print-for-custom port mode value)
  (if (eq? #t mode) (write value port)
  #/if (eq? #f mode) (display value port)
  #/print value port mode))

(define (print-all-for-custom port mode vals)
  (list-each vals #/lambda (value)
    (write-string " " port)
    (print-for-custom port mode value)))


(define (syntax-local-maybe identifier)
  (if (identifier? identifier)
    (w-
      dummy (list #/list)
      local (syntax-local-value identifier #/lambda () dummy)
    #/if (eq? local dummy)
      (list)
      (list local))
    (list)))
