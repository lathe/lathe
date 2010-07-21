; oiter.arc
;
; General-purpose data structure iteration and traversal utilities,
; similar to arc.arc's utilities, but extensible in a way that
; cooperates with orc.arc's inheritance system.

(packed:using-rels-as mr "../multival/multirule.arc"
                      oc "../multival/order-contribs.arc"
                      rc "orc.arc"
                      ut "../utils.arc"


; ===== Convenience utilities ========================================


(rc:ontype my.opos (test (o return-on-nil)) table my.table
  (zap rc.otestify test)
  (catch:each (k v) self
    (when do.test.v
      (throw:or k return-on-nil))))

(rc:ontype my.opos (test (o return-on-nil)) rc.list my.list
  (zap rc.otestify test)
  (ut:xloop result 0 tail self
    (whenlet (head . nexttail) (check tail acons)
      (if do.test.head
        result
        (next (+ result 1) nexttail)))))

(rc:ontype my.opos (test (o return-on-nil)) string my.string
  (pos rc.otestify.test self))

(rc:ontype my.opos (test (o return-on-nil)) my.lazylist my.lazylist
  (zap rc.otestify test)
  (ut:xloop result 0 tail self
    (whenlet (head nexttail) rep.tail
      (if do.test.head
        result
        (next (+ result 1) call.nexttail)))))


(=fn my.lazyrange< (first pastlast)
  (let len len.self
    (ut:xloop i 0
      (annotate my!lazylist
        (when (< i len)
          (list self.i (fn () (do.next:+ i 1))))))))


(rc:ontype my.olazyentries () rc.list my.list
  (ut:xloop i 0 tail self
    (annotate my!lazylist
      (whenlet (head . nexttail) (check tail acons)
        (list (list i head)
              (fn () (do.next (+ i 1) nexttail)))))))

(rc:ontype my.olazyentries () table my.table
  (my.olazylistify tablist.self))

(rc:ontype my.olazyentries () string my.string
  (let len len.self
    (ut:xloop i 0
      (annotate my!lazylist
        (when (< i len)
          (list (list i self.i) (fn () (do.next:+ i 1))))))))

(rc:ontype my.olazyentries () my.lazylist my.lazylist
  (ut:xloop i 0 tail self
    (annotate my!lazylist
      (whenlet (head nexttail) rep.tail
        (list (list i head)
              (fn () (do.next (+ i 1) call.nexttail)))))))


(rc:ontype my.olazykeys () rc.list my.list
  (my.lazyrange< 0 my.olen.self))

(rc:ontype my.olazykeys () table my.table
  (my.omap my.olazyentries.self .0))

(rc:ontype my.olazykeys () string my.string
  (my.lazyrange< 0 my.olen.self))

(rc:ontype my.olazykeys () my.lazylist my.lazylist
  (my.omap my.olazyentries.self .0))


(rc:ontype my.olazyvalues () rc.list my.list
  (ut:xloop tail self
    (annotate my!lazylist
      (whenlet (head . nexttail) (check tail acons)
        (list head (fn () do.next.nexttail))))))

(rc:ontype my.olazyvalues () table my.table
  (my.omap my.olazyentries.self .1))

(rc:ontype my.olazyvalues () string my.string
  (let len len.self
    (ut:xloop i 0
      (annotate my!lazylist
        (when (< i len)
          (list self.i (fn () (do.next:+ i 1))))))))

(rc:ontype my.olazyvalues () my.lazylist my.lazylist
  self)


(rc:ontype my.olazylistify () rc.list my.list
  my.olazyvalues.self)

(rc:ontype my.olazylistify () table my.table
  my.olazyentries.self)

(rc:ontype my.olazylistify () string my.string
  my.olazyvalues.self)

(rc:ontype my.olazylistify () my.lazylist my.lazylist
  self)


(=fn my.oallvalues (self test)
  (~my:opos self (complement rc.otestify.test) t))

(=fn my.oallvaluelet (var self . body)
  `(,my!osomevalue ,self (fn (,var) ,@body)))

(=fn my.osomevalue (self test)
  (~~my:opos self rc.otestify.test t))

(=fn my.osomevaluelet (var self . body)
  `(,my!osomevalue ,self (fn (,var) ,@body)))

(=mc my.oeachvalue (var self . body)
  `(,my!opos ,self (fn (,var) ,@body nil)))

(=fn my.oall (self test)
  (my.oallvalues my.olazylistify.self test))

(=mc my.oall-let (var self . body)
  `(,my!oall ,self (fn (,var) ,@body)))

(=fn my.osome (self test)
  (my.osomevalue my.olazylistify.self test))

(=mc my.osomelet (var self . body)
  `(,my!osome ,self (fn (,var) ,@body)))

(=mc my.oeach (var self . body)
  `(,my!oeachvalue ,var (,my!olazylistify ,self) ,@body))


(=fn my.olistify (seq)
  (accum acc
    (my:oall acc my.olazylistify.seq)))


; This corresponds to 'ontable, but it works for lists too.
(=mc my.onentries (k v self . body)
  `(,my!oeach (,k ,v) (,my!olazyentries ,self)
     ,@body))

; This corresponds to 'on, but it makes sense for tables too.
(=mc my.onvalues (var self . body)
  `(,my!onentries index ,var ,self
     ,@body))

; This corresponds to 'forlen, but it makes sense for tables too.
(=mc my.onkeys (var self . body)
  `(,my!oeach ,var (,my!olazykeys ,self)
     ,@body))


; This corresponds to Anarki's 'switchlet, which is like 'caselet but
; evaluates its tests and uses 'is. Here we use 'otestify, which in
; many cases defaults to 'oiso, which in many cases defaults to 'iso,
; which in many cases defaults to 'is. This also supports
; destructuring.
;
; TODO: Figure out whether destructuring is ever *actually* needed.
; Even 'caselet is used rather rarely, so 'oswitchlet might be
; neglected altogether.
;
(=mc my.oswitchlet (var subject . body)
  (let (real-var bindings)
         (if no.var
           (w/uniq g (list g `(,g ,subject)))
             anormalsym.var
           (list var `(,var ,subject))
           (w/uniq g (list g `(,g ,subject ,var ,g))))
    `(withs ,bindings
       ,(ut:xloop body body
          (whenlet (first . rest) body
            (iflet (then . elses) rest
              `(if ((,rc!otestify ,first) ,real-var)
                 ,then
                 ,do.next.elses)
              first))))))

(=mc my.oswitch (subject . body)
  `(,my!oswitchlet nil subject ,@body))


(=mc my.ocheck (x test (o alt))
  `(check ,x (,rc!otestify ,test) ,alt))


(rc:ontype my.olen () table my.table
  len.self)

(rc:ontype my.olen () rc.list my.list
  ut.numcars.self)

(rc:ontype my.olen () string my.string
  len.self)


(rc:ontype my.olen< (number) table my.table
  (< len.self number))

(rc:ontype my.olen< (number) rc.list my.list
  (ut:xloop count 0 tail self
    (and (< count number)
         (or atom.tail (do.next (+ count 1) cdr.tail)))))

(rc:ontype my.olen< (number) string my.string
  (< len.self number))


(=fn my.olen> (self number)
  (~my:olen< self (+ number 1)))

(=fn my.oempty (seq)
  (ut.== my.olen 0))


(rc:ontype my.omap (transform) table my.table
  (w/table result
    (each (k v) self
      (= .k.result do.transform.v))))

(rc:ontype my.omap (transform) rc.list my.list
  (let (values end) ut.split-end.self
    (ut.join-end (map transform values) end)))

(rc:ontype my.omap (transform) string my.string
  (tostring:each char self
    (disp do.transform.char)))

(rc:ontype my.omap (transform) fn my.fn
  (compose transform self))

(rc:ontype my.omap (transform) my.lazylist my.lazylist
  (ut:xloop self self
    (annotate my!lazylist
      (whenlet (head nexttail) rep.self
        (list do.transform.head (fn () (do.next call.nexttail)))))))


; Types used in Arc 3.1, for reference:
; Very common: cons fn int mac string sym table
; Less common: char exception input num output socket thread
;
; Types Lathe cares about from Rainbow: java-object
;
; Lathe tagged types: my.lazylist patmac
;
; Lathe handles other kinds of data structures, but these are the only
; type names seen using 'type. Other values are untyped ad-hoc
; structures or else under an alternate convention such as
; (obj type 'compiled-dependency ...).


)