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

(struct yep (val) #:prefab)
(struct nope (val) #:prefab)


(struct qexp-literal (result) #:prefab)
(struct qexp-call (op body) #:prefab)
(struct op-id () #:prefab)
(struct op-open-paren (op) #:prefab)
(struct op-close-paren () #:prefab)
(struct op-func (func) #:prefab)
(struct op-nest () #:prefab)

(define (qexp-done done val)
  (done #/qexp-literal #/done val))
(define (qexp-bind done bind qexp func) #/match qexp
  [(qexp-literal val) #/bind val func]
  [(qexp-call op body)
  #/bind (qexp-bind done bind body #/lambda (qexp)
         #/qexp-bind done bind body func)
  #/lambda (body)
  #/done #/qexp-call op body]
  [qexp (error "Expected a qexp that was a qexp-literal or a qexp-call")])

(w- done monad-maybe-done bind monad-maybe-bind
  (writeln #/equal? (qexp-done monad-maybe-done 1)
    (done #/qexp-literal #/done 1))
  (writeln #/equal?
    (bind (qexp-done monad-maybe-done 1) #/lambda (one)
    #/qexp-bind done bind (qexp-call (op-id) one) #/lambda (x)
    #/qexp-done done #/* 2 x)
    (done #/qexp-call (op-id) #/qexp-literal #/done 2)))

(struct esc (level body) #:prefab)

(define (qexp-bind-esc done bind qexp func) #/match qexp
  [(qexp-literal val) #/bind val func]
  [(qexp-call op body)
  #/bind (qexp-bind-esc done bind body #/lambda (qexp)
         #/qexp-bind-esc done bind body func)
  #/match-lambda
    [(esc i body) #/done #/esc i #/qexp-call op body]
    [(yep body) #/done #/yep #/qexp-call op body]]
  [qexp (error "Expected a qexp that was a qexp-literal or a qexp-call")])

(define (interpret done bind env qexp) #/match qexp
  [(qexp-literal result)
  #/bind result #/lambda (result)
  #/done #/yep result]
  [(qexp-call op body) (env env op body)])

; These are essentially De Morgan indices.
(struct esc-index-delegate (index-to-delegate) #:prefab)
(struct esc-index-capture () #:prefab)

(define (core-env done bind) #/lambda (env op body) #/match op
  [(op-id)
  #/bind (interpret done bind (core-env done bind) body)
  #/match-lambda
    [(esc i body) (done #/esc i #/qexp-call (op-id) body)]
    [(yep body) (interpret done bind env body)]]
  [(op-open-paren inner-op)
  #/bind (interpret done bind (core-env done bind) body)
  #/match-lambda
    [(esc i body) (done #/esc i #/qexp-call (op-open-paren op) body)]
    [(yep body)
    #/bind (interpret done bind
             (lambda (env2 op body) #/match op
               [(op-close-paren)
               #/bind (interpret done bind (core-env done bind) body)
               #/match-lambda
                 [(esc i body)
                 #/done #/esc (esc-index-delegate i)
                 #/qexp-call (op-close-paren) body]
                 [(yep body)
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
      [(yep body) (error "Unmatched op-open-paren")]]]
  [(op-nest)
  #/bind (qexp-bind-esc done bind body #/lambda (leaf)
         #/bind (interpret done bind env leaf) #/match-lambda
           [(esc i leaf) #/done #/esc i #/qexp-literal #/done leaf]
           [(yep leaf) #/done #/yep #/qexp-literal #/done leaf])
  #/match-lambda
    [(esc i body) #/done #/esc i #/qexp-call (op-nest) body]
    [(yep body) #/done #/yep #/qexp-call (op-nest) body]]
  [(op-func func) (func func env body)]
  [(op-close-paren) (error "Unmatched op-close-paren")]
  [op (error "Encountered an unrecognized op")])



(w- done monad-maybe-done bind monad-maybe-bind
  env (core-env done bind)
;  op-nest (op-func #/lambda (env body)
;            (done #/qexp-call (op-nest) body))
  op-double (op-func #/lambda (env body)
              (bind (env env (op-id) body) #/lambda (body)
              #/qexp-bind done bind body #/lambda (leaf)
              #/qexp-done done #/* 2 leaf))
  (writeln #/equal?
    (interpret done bind env
    #/qexp-call (op-id) #/qexp-literal #/done
    #/qexp-call (op-id) #/qexp-literal #/done #/qexp-literal #/done 1)
    (done #/yep 1))
  (writeln #/equal?
    (interpret done bind env
    #/qexp-call (op-nest) #/qexp-literal #/done
    #/qexp-call (op-nest) #/qexp-literal #/done #/qexp-literal #/done 1)
    (done #/yep
    #/qexp-call (op-nest) #/qexp-literal #/done
    #/qexp-call (op-nest) #/qexp-literal #/done 1))
  (writeln #/equal?
    (interpret done bind env
    #/qexp-call (op-nest)
    #/qexp-call (op-nest) #/qexp-literal #/done
    #/qexp-call (op-nest) #/qexp-literal #/done
    #/qexp-literal #/done #/qexp-literal #/done 1)
    (done #/yep
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
    (done #/yep
    #/qexp-call (op-nest)
    #/qexp-call (op-nest) #/qexp-literal #/done
    #/qexp-call (op-nest) #/qexp-literal #/done
    #/qexp-literal #/done 1)))



#|
  
  (done #/yep #/qexp-call (op-nest) body)
        [(op-nest) (done #/qexp-call (op-nest) body)]
  
  
    #/
    #/match-lambda
      [(nope body) (nope #/qexp-call (op-open-paren op) body)]
      [ (yep body)
        (interpret done bind env body)]]
        (interpret done bind env body)]]
    (display "blah1 ") (writeln body)
  #/bind (interpret done bind (core-env done bind) body) #/lambda (body)
    (display "blah2 ") (writeln body)
  #/qexp-bind done bind body #/lambda (body)
  #/qexp-done done 2] #;[
;  #/bind (interpret done bind (core-env done bind) body) #/lambda (body)
    (display "blah2 ") (writeln body)
  #/begin (define (my-interpret env qexp) #/match qexp
            [(qexp-literal result) (error "Unmatched op-open-paren")]
            [ (qexp-call op body) #/match op
              [(op-close-paren)
;    (display "--/##  ##  blah2a.1 ") (write op) (display " ") (writeln body)
              #/bind (interpret done bind (core-env done bind) body) #/lambda (body)
;    (display "--\\##  ##  blah2a.2 ") (write op) (display " ") (writeln body)
              #/done body]
              [op
    (display "-/##  ##  blah2.1 ") (write op) (display " ") (writeln body)
              #/bind (env env op body) #/lambda (body)
    (display "blah2.2 ") (writeln body)
              #/qexp-bind done bind body #/lambda (qexp)
    (display "-##--## blah2.3 ") (writeln qexp)
              #/bind (my-interpret env qexp) #/lambda (body)
    (display "-\\##  ##  blah2.4 ") (writeln body)
              #/done body]])
;  #/bind (qexp-bind done bind body #/lambda (leaf)
;         #/my-interpret env leaf)
;  #/lambda (body)
  #/bind (my-interpret env body) #/lambda (body)
    (display "blah3 ") (writeln body)
  #/bind (interpret done bind env #/qexp-call op body) #/lambda (body)
    (display "\\##  ##  ##  blah4 ") (writeln body)
  #/done body]
  
  #;[(op-open-paren op)
    (display "/##  ##  ##  blah1 ") (writeln body)
  #/qexp-bind done bind body #/lambda (body)
;  #/bind (interpret done bind (core-env done bind) body) #/lambda (body)
    (display "blah2 ") (writeln body)
  #/begin (define (my-interpret env qexp) #/match qexp
            [(qexp-literal result) (error "Unmatched op-open-paren")]
            [ (qexp-call op body) #/match op
              [(op-close-paren)
;    (display "--/##  ##  blah2a.1 ") (write op) (display " ") (writeln body)
              #/bind (interpret done bind (core-env done bind) body) #/lambda (body)
;    (display "--\\##  ##  blah2a.2 ") (write op) (display " ") (writeln body)
              #/done body]
              [op
    (display "-/##  ##  blah2.1 ") (write op) (display " ") (writeln body)
              #/bind (env env op body) #/lambda (body)
    (display "blah2.2 ") (writeln body)
              #/qexp-bind done bind body #/lambda (qexp)
    (display "-##--## blah2.3 ") (writeln qexp)
              #/bind (my-interpret env qexp) #/lambda (body)
    (display "-\\##  ##  blah2.4 ") (writeln body)
              #/done body]])
;  #/bind (qexp-bind done bind body #/lambda (leaf)
;         #/my-interpret env leaf)
;  #/lambda (body)
  #/bind (my-interpret env body) #/lambda (body)
    (display "blah3 ") (writeln body)
  #/bind (interpret done bind env #/qexp-call op body) #/lambda (body)
    (display "\\##  ##  ##  blah4 ") (writeln body)
  #/done body]
  [(op-func func) (func env body)]
  [(op-close-paren) (error "Unmatched op-close-paren")]
  [(op-nest) (error "No behavior defined for op-nest")]
  [op (error "Encountered an unrecognized op")])

(w- done monad-maybe-done bind monad-maybe-bind
  env (lambda (env2 op body) #/match op
        [(op-nest) (done #/qexp-call (op-nest) body)]
        [op ((core-env done bind) env2 op body)])
;  op-nest (op-func #/lambda (env body)
;            (done #/qexp-call (op-nest) body))
  op-double (op-func #/lambda (env body)
              (bind (env env (op-id) body) #/lambda (body)
              #/qexp-bind done bind body #/lambda (leaf)
              #/qexp-done done #/* 2 leaf))
  (writeln #/equal?
    (interpret done bind env
    #/qexp-call (op-id) #/qexp-literal #/done
    #/qexp-call (op-id) #/qexp-literal #/done #/qexp-literal #/done 1)
    (done #/qexp-literal #/done 1))
  (writeln #/equal?
    (writeln #/interpret done bind env
;    #/qexp-call (op-nest) #/qexp-literal #/done
    #/qexp-call (op-nest) #/qexp-literal #/done #/qexp-literal #/done 1)
    #f)
#/interpret done bind env
#/qexp-call (op-open-paren (op-nest)) #/qexp-literal #/done
#/qexp-call (op-open-paren (op-nest)) #/qexp-literal #/done
#/qexp-call (op-close-paren) #/qexp-literal #/done
#/qexp-call (op-open-paren (op-nest)) #/qexp-literal #/done
#/qexp-call (op-close-paren) #/qexp-literal #/done
#/qexp-call (op-close-paren) #/qexp-literal #/done
#/qexp-literal #/done 1)
;  1)
;#/qexp-call (op-nest) #/qexp-literal #/done #/qexp-literal #/done 1)
;#/qexp-literal #/done 1)


|#
#|  
  #/interpret done bind env
  #/
  #/lambda (body)
  #/interpret done bind (core-env done bind) qexp]
  [
  #/bind (qexp-bind done bind body #/lambda (leaf)
         #/interpret done bind env leaf)
  #/interpret done bind (core-env done bind) qexp])

; TODO: See if we'll use this.
(define (qexp-monadic-done done val)
  (qexp-literal #/done val))

(define (qexp-mappend qexp done bind func) #/match qexp
  [(qexp-literal val)
  #/bind val #/lambda (val)
  #/bind (func val) #/lambda (val)
  #/done #/qexp-literal #/done val]
  [(qexp-call op body)
  #/bind (qexp-mappend body done bind #/lambda (qexp)
         #/qexp-mappend qexp done bind func)
  #/lambda (body)
  #/done #/qexp-call op body]
  ; TODO: See if we should just return this value unmodified.
  [qexp (display "qexp-mappend ") (writeln qexp)
    (error "Expected a qexp that was a qexp-literal or a qexp-call")])

; TODO: See if we'll use this.
(define (qexp-monad done bind)
  (values
    (lambda (val) #/qexp-monadic-done done val)
    (lambda (effects then) #/qexp-mappend effects done bind then)))

(define (interpret done bind env qexp)
#/match qexp
  [(qexp-literal result) result]
  [(qexp-call op body)
  #/bind (interpret done bind env op) #/lambda (op)
  #/env env op done bind body]
  ; TODO: See if there's something else we should do to interpret
  ; these. In particular, it seems like we should make a
  ; `follow-heart` call.
  [qexp (display "interpret ") (writeln qexp)
    (error "Expected a qexp that was a qexp-literal or a qexp-call")])

(define (core-env env op done bind body)
#/match op
  [(op-let-env env2)
  #/values env
  [(op-id)
  #/bind (interpret done bind core-env body) #/lambda (body)
  #/interpret done bind env body]
  [(op-open-paren)
  #/bind (interpret done bind core-env body) #/lambda (body)
  #/bind (qexp-mappend body done bind #/lambda (end)
         #/done #/yep end)
  #/lambda (body)
    (display "blah1 ") (writeln body)
  #/bind (interpret done bind 
           (lambda (env2 op done bind body) #/match op
             [(op-close-paren)
    (display "blah1.1 ") (writeln body)
             #/bind (interpret done bind core-env body)
             #/lambda (body)
             #/bind (qexp-mappend body done bind #/lambda (end)
                    #/match end
                      [(yep end) (done end)]
                      [end (error "Internal error: Expected an end that was a yep")])
             #/lambda (body)
    (display "blah1.2 ") (writeln body)
             #/bind (interpret done bind env body) #/lambda (body)
    (display "blah1.3 ") (writeln body)
             #/done #/nope #/qexp-literal #/done body]
             [op
             #/bind (env env op done bind body) #/lambda (body)
             #/])
           body)
  #/lambda (body)
    (display "blah2 ") (writeln body)
  #/match body
    [(yep body) (error "Encountered an unmatched op-open-paren")]
    [(nope body)
    (display "blah3 ") (writeln body)
    #/interpret done bind env
    #/qexp-call (qexp-literal #/monad-maybe-done #/op-nest) body]
    [end (error "Internal error: Expected a body that was a yep or a nope")]]
;  #/done #/qexp-call (qexp-literal #/monad-maybe-done #/op-nest) body]
  
  #;[(op-open-paren)
  #/bind (interpret done bind core-env body) #/lambda (body)
  #/bind (qexp-mappend body done bind #/lambda (end)
         #/done #/yep end)
  #/lambda (body)
    (display "blah1 ") (writeln body)
  #/bind (interpret done bind 
           (lambda (env2 op done bind body) #/match op
             [(op-close-paren)
             #/bind (interpret done bind core-env body)
             #/lambda (body)
             #/bind (qexp-mappend body done bind #/lambda (end)
                    #/match end
                      [(yep end) (done end)]
                      [end (error "Internal error: Expected an end that was a yep")])
             #/lambda (body)
             #/bind (interpret done bind env body) #/lambda (body)
             #/done #/qexp-literal #/done #/nope body]
             [op (env env2 op done bind body)])
           body)
  #/lambda (body)
    (display "blah2 ") (writeln body)
  #/bind (qexp-mappend body done bind #/lambda (end) #/match end
           [ (yep end)
             (error "Encountered an unmatched op-open-paren")]
           [(nope end) (done end)]
           [end (writeln end) (error "Internal error: Expected an end that was a yep or a nope")])
  #/lambda (body)
;  #/done #/qexp-call (qexp-literal #/monad-maybe-done #/op-nest) body]
    (display "blah3 ") (writeln body)
  #/interpret done bind env
  #/qexp-call (qexp-literal #/monad-maybe-done #/op-nest) body]
  [(op-close-paren) (error "Encountered an unmatched op-close-paren")]
  [(op-nest) (error "No interpretation defined for op-nest")]
  [op (error "Encountered an unrecognized op")])

(struct lists-and-rest (lists rest) #:prefab)

(define (list->flat-qexp eof lst) #/match lst
  [(list) #/qexp-literal #/monad-maybe-done eof]
  [(cons first rest)
  #/qexp-call (qexp-literal #/monad-maybe-done first)
  #/qexp-literal #/monad-maybe-done #/list->flat-qexp eof rest])

(define (flat-qexp->nested-qexp flat-qexp)
  (some-val #/run-monad-maybe
  #/interpret monad-maybe-done monad-maybe-bind
    (lambda (env op done bind body) #/match op
      [(op-nest)
      #/done #/qexp-call (qexp-literal #/done #/op-nest) #/qexp-literal
      ; TODO
      #/done body];#/qexp-mappend body done bind #/lambda (qexp)
      ;  (interpret done bind env qexp)]
      [op (core-env env op done bind body)])
    flat-qexp))

(define (nested-qexp->lists-and-rest qexp) #/match qexp
  [(qexp-literal rest) (lists-and-rest (list) rest)]
  [ (qexp-call op body) #/match op
    [ (qexp-literal op) #/match (run-monad-maybe op)
      [ (some op) #/match op
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
      [op (error "Expected an op that was a some")]]
    ; NOTE: While it could make sense to support an operator that was
    ; obtained by a call to a simple operator like (op-id), that would
    ; make the representation non-canonical.
    [op (error "Expected an op that was a qexp-literal")]]
  [qexp (display "nested-qexp->lists-and-rest ") (writeln qexp)
    (error "Expected a qexp that was a qexp-literal or a qexp-call")])



(struct dummy-eof () #:prefab)

(define list-example #/list
  (op-open-paren)
  (op-open-paren)
  (op-close-paren)
;  (op-open-paren)
;  (op-close-paren)
  (op-close-paren))

(define flat-example
  (qexp-call (qexp-literal #/monad-maybe-done #/op-open-paren) #/qexp-literal #/monad-maybe-done
  #/qexp-call (qexp-literal #/monad-maybe-done #/op-open-paren) #/qexp-literal #/monad-maybe-done
  #/qexp-call (qexp-literal #/monad-maybe-done #/op-close-paren) #/qexp-literal #/monad-maybe-done
;  #/qexp-call (qexp-literal #/monad-maybe-done #/op-open-paren) #/qexp-literal #/monad-maybe-done
;  #/qexp-call (qexp-literal #/monad-maybe-done #/op-close-paren) #/qexp-literal #/monad-maybe-done
  #/qexp-call (qexp-literal #/monad-maybe-done #/op-close-paren) #/qexp-literal #/monad-maybe-done
  #/qexp-literal #/monad-maybe-done #/dummy-eof))

(define nested-example
  (qexp-call (qexp-literal #/monad-maybe-done #/op-nest)
  #/(lambda (it)
      (qexp-call (qexp-literal #/monad-maybe-done #/op-nest)
      #/qexp-literal #/monad-maybe-done
;      #/qexp-call (qexp-literal #/monad-maybe-done #/op-nest)
;      #/qexp-literal #/monad-maybe-done
        it))
  #/qexp-literal #/monad-maybe-done
  #/qexp-literal #/monad-maybe-done #/dummy-eof))

;(define sexp-example (list (list) (list)))
(define sexp-example (list (list)))
;(define sexp-example (list))

#;(equal? (list->flat-qexp (dummy-eof) list-example) flat-example)
(equal? (flat-qexp->nested-qexp flat-example) nested-example)
#;(equal? (nested-qexp->lists-and-rest nested-example)
  (lists-and-rest (list sexp-example) (monad-maybe-done #/dummy-eof)))

#;(equal?
  (nested-qexp->lists-and-rest #/flat-qexp->nested-qexp
  #/list->flat-qexp (dummy-eof) list-example)
  (lists-and-rest (list sexp-example) (monad-maybe-done #/dummy-eof)))
|#
