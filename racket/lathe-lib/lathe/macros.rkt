#lang parendown racket

(provide (all-defined-out))


(struct qexp-done (result) #:prefab)
(struct qexp-call (op body) #:prefab)
(struct op-id () #:prefab)
(struct op-open-paren () #:prefab)
(struct op-close-paren () #:prefab)
(struct op-nest () #:prefab)


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


; TODO: See if we'll use this.
(define (qexp-monadic-done done val)
  (qexp-done #/done val))

(define (qexp-mappend qexp done bind func) #/match qexp
  [(qexp-done val)
  #/bind val #/lambda (val)
  #/bind (func val) #/lambda (val)
  #/done #/qexp-done #/done val]
  [(qexp-call op body)
  #/bind (qexp-mappend body done bind #/lambda (qexp)
         #/qexp-mappend qexp done bind func)
  #/lambda (body)
  #/done #/qexp-call op body]
  ; TODO: See if we should just return this value unmodified.
  [qexp
    (error "Expected a qexp that was a qexp-done or a qexp-call")])

; TODO: See if we'll use this.
(define (qexp-monad done bind)
  (values
    (lambda (val) #/qexp-monadic-done done val)
    (lambda (effects then) #/qexp-mappend effects done bind then)))

(define (interpret done bind env qexp)
#/match qexp
  [(qexp-done result) result]
  [(qexp-call op body)
  #/bind (interpret done bind env op) #/lambda (op)
  #/env env op done bind body]
  ; TODO: See if there's something else we should do to interpret
  ; these. In particular, it seems like we should make a
  ; `follow-heart` call.
  [_ (error "Expected a qexp that was a qexp-done or a qexp-call")])

(struct yep (val) #:prefab)
(struct nope (val) #:prefab)

(define (core-env env op done bind body)
#/match op
  [(op-id)
  #/bind (interpret done bind core-env body) #/lambda (body)
  #/interpret done bind env body]
  [(op-open-paren)
  #/bind (interpret done bind core-env body) #/lambda (body)
  #/bind (qexp-mappend body done bind #/lambda (end)
         #/done #/yep end)
  #/lambda (body)
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
             #/done #/qexp-done #/done #/nope body]
             [op (env env2 op done bind body)])
           body)
  #/lambda (body)
  #/bind (qexp-mappend body done bind #/lambda (end) #/match end
           [ (yep end)
             (error "Encountered an unmatched op-open-paren")]
           [(nope end) (done end)])
  #/lambda (body)
  #/done #/qexp-call (qexp-done #/monad-maybe-done #/op-nest) body]
  [(op-close-paren) (error "Encountered an unmatched op-close-paren")]
  [(op-nest) (error "No interpretation defined for op-nest")]
  [op (error "Encountered an unrecognized op")])

(struct lists-and-rest (lists rest) #:prefab)

(define (list->flat-qexp lst) #/match lst
  [(list)
  #/qexp-done #/monad-maybe-done #/qexp-done #/monad-maybe-id]
  [(cons first rest)
  #/qexp-call (qexp-done #/monad-maybe-done first)
  #/qexp-done #/monad-maybe-done #/list->flat-qexp rest])

(define (flat-qexp->nested-qexp flat-qexp)
  (some-val #/run-monad-maybe
  #/interpret monad-maybe-done monad-maybe-bind
    (lambda (env op done bind body) #/match op
      [(op-nest)
      #/done #/qexp-call (qexp-done #/done #/op-nest) #/qexp-done
      #/qexp-mappend body done bind #/lambda (qexpr)
        (interpret done bind env qexpr)]
      [op (core-env env op done bind body)])
    flat-qexp))

(define (nested-qexp->lists-and-rest qexp) #/match qexp
  [(qexp-done rest) (lists-and-rest (list) rest)]
  [ (qexp-call op body) #/match op
    [ (qexp-done op) #/match (run-monad-maybe op)
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
    [op (error "Expected an op that was a qexp-done")]]
  [qexp
    (error "Expected a qexp that was a qexp-done or a qexp-call")])



(define list-example #/list
  (op-open-paren)
  (op-open-paren)
  (op-close-paren)
  (op-open-paren)
  (op-close-paren)
  (op-close-paren))

(define flat-example
  (qexp-call (qexp-done #/monad-maybe-done #/op-open-paren) #/qexp-done #/monad-maybe-done
  #/qexp-call (qexp-done #/monad-maybe-done #/op-open-paren) #/qexp-done #/monad-maybe-done
  #/qexp-call (qexp-done #/monad-maybe-done #/op-close-paren) #/qexp-done #/monad-maybe-done
  #/qexp-call (qexp-done #/monad-maybe-done #/op-open-paren) #/qexp-done #/monad-maybe-done
  #/qexp-call (qexp-done #/monad-maybe-done #/op-close-paren) #/qexp-done #/monad-maybe-done
  #/qexp-call (qexp-done #/monad-maybe-done #/op-close-paren) #/qexp-done #/monad-maybe-done
  #/qexp-done #/monad-maybe-done
  #/qexp-done #/monad-maybe-id))

(define nested-example
  (qexp-call (qexp-done #/monad-maybe-done #/op-nest)
  #/(lambda (it)
      (qexp-call (qexp-done #/monad-maybe-done #/op-nest)
      #/qexp-done #/monad-maybe-done
      #/qexp-call (qexp-done #/monad-maybe-done #/op-nest)
      #/qexp-done #/monad-maybe-done
        it))
  #/qexp-done #/monad-maybe-done
  #/qexp-done #/monad-maybe-id))

(equal? (list->flat-qexp list-example) flat-example)
(equal? (flat-qexp->nested-qexp flat-example) nested-example)
(equal? (nested-qexp->lists-and-rest nested-example)
  (lists-and-rest (list #/list (list) (list)) (monad-maybe-id)))

(equal?
  (nested-qexp->lists-and-rest #/flat-qexp->nested-qexp
  #/list->flat-qexp list-example)
  (lists-and-rest (list #/list (list) (list)) (monad-maybe-id)))
