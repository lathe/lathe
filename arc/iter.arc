; iter.arc
;
; Combinator-based iteration.
;
; There are essentially two kinds of iteration object to consider when
; using this library.
;
; An iterator is a nullary function, usually with side effects, which
; is called over and over to generate each value used in the
; iteration. An iterator should return either nil, to indicate that
; there are no more values, or else a singleton list containing the
; yielded value. It should be impossible to modify which results an
; iterator traverses over once the iterator has been created, but it
; may still be possible to mutate the result values individually.
;
; An iterable is a nullary function which returns an iterator. It
; should return an iterator over the same values each time, but it
; may still be possible to mutate the iterators' result values
; individually. This requirement that the iteration be over the same
; values each time means that if 'iterify is passed a list, string, or
; table and that input is mutated later on, it will not mutate the
; iterable (except inasmuch as the result values are mutated).
;
; Sometimes it's useful to break the constraint that an iterable or
; iterator should be immutable. This library does so with 'yielder
; and 'iter-with-yield, since although those operations can still
; produce immutable iterators if given the right input, it's in
; general impossible to enforce that behavior.
;
; Most of this library is intended to be usable even when first-class
; continuations are not supported by the Arc implementation. However,
; 'yielder and 'iter-with-yield are provided, which allow for the
; usual coroutine-esque iterable syntax when continuations are
; supported. This is meant to help bridge the gap when using already
; existing continuation-avoiding code within a project that doesn't
; need to avoid continuations.

(packed:using-rels-as ut "utils.arc"


; NOTE: "Lexico" in variable names here stands for "lexicographic."
; Most everyday orderings of lists by their elements are
; lexicographic. These include alphabetic order, orderings on numbers
; observed as strings of digits, version number ordering, etc. If you
; were to reverse the letters, digits, or other components of these
; ordered objects, they would instead be in "colexicographic" order.
; One example of a common colexicographic order occurs with numbers
; represented as little-endian byte strings.

(=fn my.empty-iter ()
  (fn () (fn ())))

(=fn my.must-iterify (val)
  (or my.iterify.val
      (err:+ "An unsupported type of value was passed to "
             "'must-iterify.")))

(=fn my.iterify (val)
  (case type.val
    fn     val
    cons   (let copyval copy.val
             (fn () (let innerval copyval
                      (fn () (when innerval (list pop.innerval))))))
    table  (let copyval tablist.val
             (fn () (let innerval copyval
                      (fn () (when innerval (list pop.innerval))))))
    str    (let copyval copy.val
             (fn () (with (i 0 innerval copyval innerlen len.copyval)
                      (fn () (if (< i innerlen)
                               (list:do1 do.innerval.i ++.i)
                               (wipe innerval innerlen))))))
    sym    (if val
             (withs (innerval string.val innerlen len.innerval)
               (fn () (let i 0
                        (fn () (if (< i innerlen)
                                 (list:do1 do.innerval.i ++.i)
                                 (wipe innerval innerlen))))))
             (my.empty-iter))))

(=fn my.iter-trav (func iterable)
  (zap my.must-iterify iterable)
  (let iterator call.iterable
    (ut:dstwhilet (val) call.iterator
      do.func.val)))

(=mc my.iter-each (var iterable . body)
  `(,my!iter-trav (fn (,var) ,@body) ,iterable))

(=fn my.iter-join args
  (zap [map my.must-iterify _] args)
  (fn ()
    (let iterators (map call args)
      (afn ()
        (whenlet (first . rest) iterators
          (or call.first
              (do (= iterators rest)
                  call.self)))))))

(=fn my.cached-iter (iterable)
  (zap my.must-iterify iterable)
  (withs (cache (queue)
          iterator call.iterable
          get (fn (index)
                (ut:dstwhilet (elem) (and iterator
                                          (<= qlen.cache index)
                                          call.iterator)
                  (enq elem cache))
                (if (< index qlen.cache)
                  (list qlist.cache.index)
                  wipe.iterator)))
    (fn ()
      (let i 0
        (fn ()
          (do1 do.get.i ++.i))))))

(=fn my.mapping (func iterable)
  (zap my.must-iterify iterable)
  (fn ()
    (let iterator call.iterable
      (fn ()
        (iflet (result) call.iterator
          (list do.func.result)
          wipe.iterator)))))

(=mc my.mappinglet (var iterable . body)
  `(,my!mapping (fn (,var) ,@body) ,iterable))

(=fn my.joining (iterable)
  (zap my.must-iterify iterable)
  (fn ()
    (with (outside-iterator call.iterable
           inside-iterator (call:call my.empty-iter))
      (afn ()
        (when inside-iterator
          (or call.inside-iterator
              (iflet (next-inside) call.outside-iterator
                (do (= inside-iterator (aif my.iterify.next-inside
                                         call.it
                                         next-inside))
                    call.self)
                (wipe outside-iterator inside-iterator))))))))

(=fn my.mappending (func iterable)
  (my.joining:my.mapping func iterable))

(=mc my.mappendinglet (var iterable . body)
  `(,my!mappending (fn (,var) ,@body) ,iterable))

; Since 'iterify works on strings and even symbols, this will probably
; go deeper than intended unless a specific termination condition is
; given.
(=fn my.deepjoining (val (o condition [do t]))
  (iflet iterable (and testify.condition.val my.iterify.val)
    (my.mappending [my.deepjoining _ condition] iterable)
    val))

(=fn my.iter*colexico iterables
  (zap [map my.cached-iter _] iterables)
  (my.mapping rev
    (ut:foldlet combined-iter (my.iterify list.nil)
                next-iter iterables
      (my:mappendinglet that-elem next-iter
        (my:mappinglet these-elems combined-iter
          (cons that-elem these-elems))))))

(=fn my.nonnegs ()
  (fn ()
    (let i -1
      (fn ()
        (list ++.i)))))

(=fn my.iter-range (start finish (o step 1))
  (when (in step 0 0.0)
    (err "A step of zero was given to 'iter-range."))
  (let approaching (if (< 0 step) <= >=)
    (fn ()
      (let i start
        (fn ()
          (when (do.approaching i finish)
            (do1 list.i
                 (++ i step))))))))

(=fn my.nonneg-tuples-by-sum
      (size sum (o reversed-lexico-significance))
  (unless (<= 0 size)
    (err "A negative size was given to 'nonneg-tuples-by-sum."))
  (unless ut.an-int.size
    (err "A non-integer size was given to 'nonneg-tuples-by-sum."))
  (unless (<= 0 sum)
    (err "A negative sum was given to 'nonneg-tuples-by-sum."))
  (unless ut.an-int.sum
    (err "A non-integer sum was given to 'nonneg-tuples-by-sum."))
  (case size
    0  (do (unless (is sum 0)
             (err:+ "A size of zero and a nonzero sum were given to "
                    "'nonneg-tuples-by-sum."))
           (my.iterify list.nil))
    1  (my.iterify:list:list sum)
       (my.mapping (if reversed-lexico-significance rev idfn)
         (my:mappendinglet choice (my.iter-range 0 sum)
           (my.mapping [cons choice _]
             (my.nonneg-tuples-by-sum (- size 1) (- sum choice)))))))

(=fn my.sum-grouped-nonneg-tuples
       (size (o reversed-lexico-significance))
  (unless (<= 0 size)
    (err "A negative size was given to 'sum-grouped-nonneg-tuples."))
  (unless ut.an-int.size
    (err:+ "A non-integer size was given to "
           "'sum-grouped-nonneg-tuples."))
  (case size 0
    (my.iterify list.nil)
    (my:mappendinglet sum (my.nonnegs)
      (my.nonneg-tuples-by-sum size sum
        reversed-lexico-significance))))

(=fn my.skippingover (amount iterable)
  (unless (<= 0 amount)
    (err "A negative amount was given to 'skippingover."))
  (unless ut.an-int.amount
    (err "A non-integer amount was given to 'skippingover."))
  (fn ()
    (ut:ret iterator call.iterable
      (let i 0
        (while (and (< i amount) call.iterator)
          ++.i)))))

(=fn my.stoppingafter (amount iterable)
  (unless (<= 0 amount)
    (err "A negative amount was given to 'stoppingafter."))
  (unless ut.an-int.amount
    (err "A non-integer amount was given to 'stoppingafter."))
  (fn ()
    (with (iterator call.iterable i 0)
      (fn ()
        (or (and iterator (< i amount) (do ++.i call.iterator))
            (wipe iterator amount))))))

(=fn my.stepping (amount iterable)
  (unless (<= 0 amount)
    (err "A negative amount was given to 'stepping."))
  (unless ut.an-int.amount
    (err "A non-integer amount was given to 'stepping."))
  (fn ()
    (let iterator call.iterable
      (fn ()
        (when iterator
          (let i 0
            (while:or (when (< i amount) ++.i call.iterator)
                      wipe.iterator)))
        (or only.call.iterator
            wipe.iterator)))))

; NOTE: This should only be used on infinite iterables. It will treat
; finite iterables as though they're infinite ones that end in
; repeating nils.
(=fn my.iter*sum-grouped-w/colexico (iterables
                                     reversed-lexico-significance)
  (zap [map my.must-iterify _] iterables)
  (withs (len len.iterables range (range 0 (- len 1)))
    (my:mappinglet coordinates (my.sum-grouped-nonneg-tuples len
                                 reversed-lexico-significance)
      (map [car:call:call (my.skippingover do.coordinates._
                                           do.iterables._)]
           range))))

(=fn my.iter*sum-grouped iterables
  (my.iter*sum-grouped-w/colexico iterables nil))

(=fn my.iter*sum-grouped-colexico iterables
  (my.iter*sum-grouped-w/colexico iterables t))

(=fn my.keeping (test iterable)
  (my.mappending (andf testify.test list) iterable))

(=mc my.keepinglet (var iterable . body)
  `(,my!keeping (fn (,var) ,@body) ,iterable))

(=fn my.folding (func start iterable)
  (fn ()
    (with (iterator call.iterable intermediate start)
      (fn ()
        (when iterator
          (do1 list.intermediate
               (iflet (next) call.iterator
                 (= intermediate (do.func intermediate next))
                 (wipe iterator intermediate))))))))

(=mc my.foldinglet (a start b iterable . body)
  `(,my!folding (fn (,a ,b) ,@body) start iterable))

(=fn my.stoppingafter1 (iterable)
  (my.stoppingafter 1 iterable))

(=fn my.iter->list (iterable)
  (accum acc
    (my.iter-trav acc iterable)))

(=fn my.repeating (iterable)
  (zap my.must-iterify iterable)
  (my.joining (fn () (fn () list.iterable))))

(=fn my.zipping iterables
  (zap [map my.must-iterify _] iterables)
  (fn ()
    (let iterators (map call iterables)
      (fn ()
        (let svalues (map call iterators)
          (if (some no svalues)
            wipe.iterators
            (list:map car svalues)))))))

(=fn my.padzipping (pad . iterables)
  (zap [map my.must-iterify _] iterables)
  (fn ()
    (let iterators (map call iterables)
      (fn ()
        (withs (finished t
                values (accum acc
                         ; NOTE: Anarki doesn't support dotted lists
                         ; as syntax in [...] forms, so we're using an
                         ; explicit (fn (_) (...)).
                         (reclist (fn (_)
                                    (let (a . b) _
                                      (iflet (result) only.call.a
                                        (do do.acc.result
                                            wipe.finished)
                                        (do do.acc.pad
                                            (wipe car._)))
                                      nil))
                                  iterators)))
          (if finished
            wipe.iterators
            list.values))))))

(=fn my.iter-some (func iterable)
  (call:call:my.keeping idfn (my.mapping func iterable)))

(=mc my.iter-somelet (var iterable . body)
  `(,my!iter-some (fn (,var) ,@body) ,iterable))

(=fn my.iter-all (func iterable)
  (~my.iter-some ~func iterable))

(=mc my.iter-all-let (var iterable . body)
  `(,my!iter-all (fn (,var) ,@body) ,iterable))

(=fn my.iter-keys (reffable)
  (if reffable
    (case type.table-or-list
      table   (my.iterify keys.reffable)
      cons    (my.iter-range 0 (- len.reffable 1))
      string  (my.iter-range 0 (- len.reffable 1))
              (err:+ "An unknown type of value was given to "
                     "'iter-keys."))
    (my.empty-iter)))

; This defines an anonymous iterable-returning function with
; coroutine-esque syntax like you might see in Python or C#. The
; function this returns will take the specified parameters and return
; an iterable which will traverse values passed to an anaphoric 'yield
; function within the body. If the body has side effects or changes
; its behavior based on external side effects, this will break the
; usual contract for iterables, since it might not iterate over the
; same values each time.
;
; Note that this relies on continuations.
;
(=mc my.yielder (parms . body)
  `(fn ,parms
     (,my!iter-with-yield (fn (yield) ,@body))))

; This function takes a function which accepts a yield parameter, and
; it returns an iterable which returns the results yielded by the
; function. If 'func has side effects or changes its behavior based on
; external side effects, this will break the usual contract for
; iterables, since it might not iterate over the same values each
; time.
;
; Note that this relies on continuations.
;
(=fn my.iter-with-yield (func)
  (fn ()
    (let next nil
      (= next (fn (succeed-from-iter)
                (do.func:fn (yielded-value)
                  (point return-from-yield
                    (= next (fn (succeed-from-this-iter)
                              (= succeed-from-iter
                                 succeed-from-this-iter)
                              do.return-from-yield.nil))
                    do.succeed-from-iter.yielded-value))))
      (fn ()
        (when next
          (point return-from-iter
            (do.next return-from-iter:list)
            wipe.next))))))


)