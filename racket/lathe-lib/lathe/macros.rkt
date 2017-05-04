#lang parendown racket

(require "main.rkt")

(provide (all-defined-out))


(define (mappend args func)
  (apply append #/map func args))

(struct some (val) #:prefab)
(struct none () #:prefab)

(define (monad-maybe-done result)
  (some result))

(define (monad-maybe-bind intermediate then)
#/match intermediate
  [(some val) (then val)]
  [(none) (monad-maybe-id)]
  [intermediate
    (error "Expected an intermediate value that was a some or a none")])

(define (monad-maybe-id)
  (none))

(define (run-monad-maybe effects)
  effects)

(define (monad-list-done result)
  (list result))

(define (monad-list-bind intermediate then)
  (mappend intermediate then))

(define (monad-list-id)
  (list))

(define (monad-list-compose a b)
  (append a b))

(define (run-monad-list effects)
  effects)

(define (monad-sexp-done result)
  (list result))

(define (monad-sexp-bind intermediate then)
  (mappend intermediate then))

(define (monad-sexp-id)
  (list))

(define (monad-sexp-compose a b)
  (append a b))

(define (monad-sexp-symbol symbol)
  (list symbol))

(define (monad-sexp-nest effects)
  (list effects))

(define (run-monad-sexp effects)
  effects)



(struct qexp-literal (result) #:prefab)
(struct qexp-call (op body) #:prefab)
(struct op-id () #:prefab)
(struct op-open-paren (op) #:prefab)
(struct op-close-paren () #:prefab)

(define (qexp-done done val)
  (done #/qexp-literal #/done val))
(define (qexp-bind done bind qexp func) #/match qexp
  [(qexp-literal val) #/bind val func]
  [(qexp-call op body)
  #/bind (qexp-bind done bind body #/lambda (leaf)
         #/qexp-bind done bind leaf func)
  #/lambda (body)
  #/done #/qexp-call op #/qexp-literal #/done body]
  [qexp (error "Expected a qexp that was a qexp-literal or a qexp-call")])
(define (qexp-map done bind qexp func) #/match qexp
  [(qexp-literal val) #/qexp-literal #/bind val func]
  [(qexp-call op body)
  #/qexp-call op
  #/qexp-map done bind body #/lambda (leaf)
    (done #/qexp-map done bind leaf func)]
  [qexp (error "Expected a qexp that was a qexp-literal or a qexp-call")])

(w- done monad-maybe-done bind monad-maybe-bind
  (writeln #/equal? (qexp-done monad-maybe-done 1)
    (done #/qexp-literal #/done 1))
  (writeln #/equal?
    (bind (qexp-done done 1) #/lambda (one)
    #/qexp-bind done bind
      (qexp-call (op-id) #/qexp-literal #/done one)
    #/lambda (x)
    #/qexp-done done #/* 2 x)
    (done #/qexp-call (op-id) #/qexp-literal #/done
    #/qexp-literal #/done 2))
  (writeln #/equal?
    (bind (qexp-done done 1) #/lambda (one)
    #/done #/qexp-map done bind
      (qexp-call (op-id) #/qexp-literal #/done one)
    #/lambda (x)
      (done #/* 2 x))
    (done #/qexp-call (op-id) #/qexp-literal #/done
    #/qexp-literal #/done 2)))

(struct esc (level body) #:prefab)
(struct ret (val) #:prefab)

(define (qexp-bind-esc done bind qexp func) #/match qexp
  [(qexp-literal val) #/bind val func]
  [(qexp-call op body)
  #/bind (qexp-bind-esc done bind body #/lambda (qexp)
         #/qexp-bind-esc done bind body func)
  #/match-lambda
    [(esc i body) #/done #/esc i #/qexp-call op body]
    [(ret body) #/done #/ret #/qexp-call op body]]
  [qexp (error "Expected a qexp that was a qexp-literal or a qexp-call")])

(define (interpret done bind env qexp) #/match qexp
  [(qexp-literal result)
  #/bind result #/lambda (result)
  #/done #/ret result]
  [(qexp-call op body) (env env op body)])

; These are essentially De Morgan indices.
(struct esc-index-delegate (index-to-delegate) #:prefab)
(struct esc-index-capture () #:prefab)

(define (core-env done bind) #/lambda (env op body) #/match op
  [(op-id)
  #/bind (interpret done bind (core-env done bind) body)
  #/match-lambda
    [(esc i body) (done #/esc i #/qexp-call (op-id) body)]
    [(ret body) (interpret done bind env body)]]
  [(op-open-paren inner-op)
  #/bind (interpret done bind (core-env done bind) body)
  #/match-lambda
    [(esc i body) (done #/esc i #/qexp-call (op-open-paren op) body)]
    [(ret body)
    #/bind (interpret done bind
             (lambda (env2 op body) #/match op
               [(op-close-paren)
               #/bind (interpret done bind (core-env done bind) body)
               #/match-lambda
                 [(esc i body)
                 #/done #/esc (esc-index-delegate i)
                 #/qexp-call (op-close-paren) body]
                 [(ret body)
                 #/done #/esc (esc-index-capture)
                 #/qexp-literal #/done body]]
               [op (env env2 op body)])
             body)
    #/match-lambda
      [ (esc i body) #/match i
        [(esc-index-capture)
        #/bind (interpret done bind env #/qexp-call inner-op body)
        #/lambda (body)
        #/done body]
        [(esc-index-delegate i) #/done #/esc i body]
        [i (error "Expected an escape index that op-open-paren could capture or delegate")]]
      [(ret body) (error "Unmatched op-open-paren")]]]
  [(op-close-paren) (error "Unmatched op-close-paren")]
  [op (error "Encountered an unrecognized op")])



(define (env-with-self-quoting done bind self-quoting-op? env)
#/lambda (env2 op body)
  (if (self-quoting-op? op)
    (bind (qexp-bind-esc done bind body #/lambda (leaf)
          #/bind (interpret done bind env2 leaf) #/match-lambda
            [(esc i leaf) #/done #/esc i #/qexp-literal #/done leaf]
            [(ret leaf) #/done #/ret #/qexp-literal #/done leaf])
    #/match-lambda
      [(esc i body) #/done #/esc i #/qexp-call op body]
      [(ret body) #/done #/ret #/qexp-call op body])
 #/env env2 op body))

(struct op-nest () #:prefab)
(define (env-with-op-nest done bind env)
  (env-with-self-quoting done bind
    (lambda (op) #/match op [(op-nest) #t] [op #f])
    env))

(struct op-double () #:prefab)

(w- done monad-maybe-done bind monad-maybe-bind
  env (env-with-op-nest done bind #/core-env done bind)
  env (lambda (env2 op body) #/match op
        
        [(op-double)
        #/bind (interpret done bind (core-env done bind) body)
        #/match-lambda
          [(esc i body) (done #/esc i #/qexp-call (op-double) body)]
          [(ret body)
          #/qexp-bind done bind body #/lambda (leaf)
          #/qexp-done done #/* 2 leaf]]
        
        [op (env env2 op body)])
  (writeln #/equal?
    (interpret done bind env
    #/qexp-call (op-double) #/qexp-literal #/done
    #/qexp-literal #/done 3)
    (done #/qexp-literal #/done 6))
  (writeln #/equal?
    (interpret done bind env
    #/qexp-call (op-id) #/qexp-literal #/done
    #/qexp-call (op-id) #/qexp-literal #/done #/qexp-literal #/done 1)
    (done #/ret 1))
  (writeln #/equal?
    (interpret done bind env
    #/qexp-call (op-nest) #/qexp-literal #/done
    #/qexp-call (op-nest) #/qexp-literal #/done #/qexp-literal #/done 1)
    (done #/ret
    #/qexp-call (op-nest) #/qexp-literal #/done
    #/qexp-call (op-nest) #/qexp-literal #/done 1))
  (writeln #/equal?
    (interpret done bind env
    #/qexp-call (op-nest)
    #/qexp-call (op-nest) #/qexp-literal #/done
    #/qexp-call (op-nest) #/qexp-literal #/done
    #/qexp-literal #/done #/qexp-literal #/done 1)
    (done #/ret
    #/qexp-call (op-nest)
    #/qexp-call (op-nest) #/qexp-literal #/done
    #/qexp-call (op-nest) #/qexp-literal #/done
    #/qexp-literal #/done 1))
  (writeln #/equal?
    (interpret done bind env
    #/qexp-call (op-open-paren (op-nest)) #/qexp-literal #/done
    #/qexp-call (op-open-paren (op-nest)) #/qexp-literal #/done
    #/qexp-call (op-close-paren) #/qexp-literal #/done
    #/qexp-call (op-open-paren (op-nest)) #/qexp-literal #/done
    #/qexp-call (op-close-paren) #/qexp-literal #/done
    #/qexp-call (op-close-paren) #/qexp-literal #/done
    #/qexp-literal #/done 1)
    (done #/ret
    #/qexp-call (op-nest)
    #/qexp-call (op-nest) #/qexp-literal #/done
    #/qexp-call (op-nest) #/qexp-literal #/done
    #/qexp-literal #/done 1))
)

(w- done monad-list-done bind monad-list-bind
  env (env-with-op-nest done bind #/core-env done bind)
  env (lambda (env2 op body) #/match op
        
        [(op-double)
        #/bind (interpret done bind (core-env done bind) body)
        #/match-lambda
          [(esc i body) (done #/esc i #/qexp-call (op-double) body)]
          [(ret body)
          #/qexp-bind done bind body #/lambda (leaf)
          #/qexp-done done #/* 2 leaf]]
        
        [op (env env2 op body)])
  (writeln #/equal?
    (interpret done bind env
    #/qexp-call (op-double) #/qexp-literal #/list
      (qexp-literal #/done 3)
      (qexp-literal #/done 4))
    (list
      (qexp-literal #/done 6)
      (qexp-literal #/done 8)))
  (writeln #/equal?
    (interpret done bind env
    #/qexp-call (op-id) #/qexp-literal #/list
      (qexp-call (op-id) #/qexp-literal #/list
        (qexp-literal #/done 1)
        (qexp-literal #/done 2))
      (qexp-call (op-id) #/qexp-literal #/list
        (qexp-literal #/done 3)
        (qexp-literal #/done 4)))
    (list
      (ret 1)
      (ret 2)
      (ret 3)
      (ret 4)))
  (writeln #/equal?
    (interpret done bind env
    #/qexp-call (op-nest) #/qexp-literal #/list
      (qexp-call (op-nest) #/qexp-literal #/list
        (qexp-literal #/done 1)
        (qexp-literal #/done 2))
      (qexp-call (op-nest) #/qexp-literal #/list
        (qexp-literal #/done 3)
        (qexp-literal #/done 4)))
    (for/list ([i (in-range 1 (+ 1 4))])
      (ret
      #/qexp-call (op-nest) #/qexp-literal #/list
      #/qexp-call (op-nest) #/qexp-literal #/list i))
    ; TODO: I think what we want is more like this:
    #;(done #/ret
    #/qexp-call (op-nest) #/qexp-literal #/list
      (qexp-call (op-nest) #/qexp-literal #/list
        1
        2)
      (qexp-call (op-nest) #/qexp-literal #/list
        3
        4))
  )
  (writeln #/equal?
    (interpret done bind env
    #/qexp-call (op-nest)
    #/qexp-call (op-nest) #/qexp-literal #/list
      ; TODO: Figure out why this list of 3 elements is turning into 9
      ; times the results (i.e. being looped over twice).
      (qexp-call (op-nest) #/qexp-literal #/list
        (qexp-literal #/list
          (qexp-literal #/done 1)
          (qexp-literal #/done 2))
        (qexp-literal #/list
          (qexp-literal #/done 3)
          (qexp-literal #/done 4)))
      (qexp-call (op-nest) #/qexp-literal #/list
        (qexp-literal #/list
          (qexp-literal #/done 5)
          (qexp-literal #/done 6))
        (qexp-literal #/list
          (qexp-literal #/done 7)
          (qexp-literal #/done 8)))
      (qexp-call (op-nest) #/qexp-literal #/list
        (qexp-literal #/list
          (qexp-literal #/done 9)
          (qexp-literal #/done 10))
        (qexp-literal #/list
          (qexp-literal #/done 11)
          (qexp-literal #/done 12))))
    (apply append #/for/list ([_ (in-range 3)])
    #/for/list ([i (in-range 1 (+ 1 12))])
      (ret
      #/qexp-call (op-nest)
      #/qexp-call (op-nest) #/qexp-literal #/done
      #/qexp-call (op-nest) #/qexp-literal #/done
      #/qexp-literal #/done i))
    ; TODO: I think what we want is more like this:
    #;(done #/ret
    #/qexp-call (op-nest)
    #/qexp-call (op-nest) #/qexp-literal #/list
      (qexp-call (op-nest) #/qexp-literal #/list
        (qexp-literal #/list
          1
          2)
        (qexp-literal #/list
          3
          4))
      (qexp-call (op-nest) #/qexp-literal #/list
        (qexp-literal #/list
          5
          6)
        (qexp-literal #/list
          7
          8))
      (qexp-call (op-nest) #/qexp-literal #/list
        (qexp-literal #/list
          9
          10)
        (qexp-literal #/list
          11
          12)))
  )
  (writeln #/equal?
    (interpret done bind env
    #/qexp-call (op-open-paren (op-nest)) #/qexp-literal #/done
    #/qexp-call (op-open-paren (op-nest)) #/qexp-literal #/done
    #/qexp-call (op-close-paren) #/qexp-literal #/list
      (qexp-call (op-open-paren (op-nest)) #/qexp-literal #/done
      #/qexp-call (op-close-paren) #/qexp-literal #/list
        (qexp-call (op-close-paren) #/qexp-literal #/list
          (qexp-literal #/done 1)
          (qexp-literal #/done 2))
        (qexp-call (op-close-paren) #/qexp-literal #/list
          (qexp-literal #/done 3)
          (qexp-literal #/done 4)))
      (qexp-call (op-open-paren (op-nest)) #/qexp-literal #/done
      #/qexp-call (op-close-paren) #/qexp-literal #/list
        (qexp-call (op-close-paren) #/qexp-literal #/list
          (qexp-literal #/done 5)
          (qexp-literal #/done 6))
        (qexp-call (op-close-paren) #/qexp-literal #/list
          (qexp-literal #/done 7)
          (qexp-literal #/done 8))))
    (for/list ([i (in-range 1 (+ 1 8))])
      (ret
      #/qexp-call (op-nest)
      #/qexp-call (op-nest) #/qexp-literal #/done
      #/qexp-call (op-nest) #/qexp-literal #/done
      #/qexp-literal #/done i))
    ; TODO: I think what we want is more like this:
    #;(done #/ret
    #/qexp-call (op-nest)
    #/qexp-call (op-nest) #/qexp-literal #/list
      (qexp-call (op-nest) #/qexp-literal #/list
        (qexp-literal #/list
          1
          2)
        (qexp-literal #/list
          3
          4))
      (qexp-call (op-nest) #/qexp-literal #/list
        (qexp-literal #/list
          5
          6)
        (qexp-literal #/list
          7
          8)))
  )
)


(struct lists-and-rest (lists rest) #:prefab)

(define (list->flat-qexp done eof lst) #/match lst
  [(list) #/qexp-literal #/done #/qexp-literal #/done eof]
  [(cons first rest)
  #/qexp-call first #/qexp-literal #/done
  #/list->flat-qexp done eof rest])

(define (flat-qexp->nested-qexp done bind flat-qexp)
  (some-val #/run-monad-maybe
  #/interpret monad-maybe-done monad-maybe-bind
    (env-with-op-nest done bind #/core-env done bind)
    flat-qexp))

(define (nested-qexp->lists-and-rest qexp) #/match qexp
  [(qexp-literal rest) (lists-and-rest (list) rest)]
  [ (qexp-call op body) #/match op
    [(op-nest)
    #/match (nested-qexp->lists-and-rest body)
      [(lists-and-rest lists1 rest)
      #/match (run-monad-maybe rest)
        [(some rest)
        #/match (nested-qexp->lists-and-rest rest)
          [ (lists-and-rest lists2 rest)
            (lists-and-rest (cons lists1 lists2) rest)]
          [lists-and-rest (error "Internal error: Expected a nested-qexp->lists-and-rest result that was a lists-and-rest")]]
        [rest (error "Expected a rest that was a some")]]
      [lists-and-rest (error "Internal error: Expected a nested-qexp->lists-and-rest result that was a lists-and-rest")]]
    ; NOTE: While it could make sense to support other operators
    ; like (op-id), that would make the representation
    ; non-canonical.
    [op (error "Expected an op that was an op-nest")]]
  [qexp
    (error "Expected a qexp that was a qexp-literal or a qexp-call")])

(struct dummy-eof () #:prefab)

(define list-example #/list
  (op-open-paren #/op-nest)
  (op-open-paren #/op-nest)
  (op-close-paren)
  (op-open-paren #/op-nest)
  (op-close-paren)
  (op-close-paren))

(define flat-example
  (qexp-call (op-open-paren #/op-nest) #/qexp-literal #/monad-maybe-done
  #/qexp-call (op-open-paren #/op-nest) #/qexp-literal #/monad-maybe-done
  #/qexp-call (op-close-paren) #/qexp-literal #/monad-maybe-done
  #/qexp-call (op-open-paren #/op-nest) #/qexp-literal #/monad-maybe-done
  #/qexp-call (op-close-paren) #/qexp-literal #/monad-maybe-done
  #/qexp-call (op-close-paren) #/qexp-literal #/monad-maybe-done
  #/qexp-literal #/monad-maybe-done
  #/qexp-literal #/monad-maybe-done #/dummy-eof))

(define nested-example
  (qexp-call (op-nest)
  #/(lambda (it)
      (qexp-call (op-nest) #/qexp-literal #/monad-maybe-done
      #/qexp-call (op-nest) #/qexp-literal #/monad-maybe-done it))
  #/qexp-literal #/monad-maybe-done
  #/qexp-literal #/monad-maybe-done #/dummy-eof))

(define sexp-example (list (list) (list)))

(equal? (list->flat-qexp monad-maybe-done (dummy-eof) list-example)
  flat-example)
(equal?
  (flat-qexp->nested-qexp monad-maybe-done monad-maybe-bind
    flat-example)
  (ret nested-example))
(equal? (nested-qexp->lists-and-rest nested-example)
  (lists-and-rest (list sexp-example) (monad-maybe-done #/dummy-eof)))

(equal?
  (nested-qexp->lists-and-rest #/ret-val
  #/flat-qexp->nested-qexp monad-maybe-done monad-maybe-bind
  #/list->flat-qexp monad-maybe-done (dummy-eof) list-example)
  (lists-and-rest (list sexp-example) (monad-maybe-done #/dummy-eof)))
