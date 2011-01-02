; jvm.arc


(packed:using-rels-as sn "sniff.arc"


; This library should work if either my.jarcdrop* or sn.rainbowdrop*
; is true. However, it should also load without complaint if neither
; of those features is provided. That way, a library can provide a
; few pieces of extra functionality on the JVM without breaking its
; ability to be used off of the JVM. This is especially necessary for
; libraries which implement the same features in multiple ways
; depending on which platform is present (analogously to the way this
; one does for the Jarc and Rainbow platforms).
;
; Although this library should *work* on Jarc and Rainbow, it isn't
; necessarily consistent between them. A method call, a constructor
; call, or a field get or set can behave differently on Jarc and
; Rainbow thanks to the differences in the way those two platforms
; coerce the arguments, select the specific method/constructor, and
; coerce the return value. The first two concerns might be avoided if
; we did the method selection reflection ourselves, but the return
; value of our reflection call would still be coerced no matter what
; we do, so we don't bother.
;
; Instead, we allow for differences depending on whether Jarc or
; Rainbow is in use. Many applications may be lucky enough not to have
; to worry, but in some cases the jarcdrop* and rainbowdrop* flags
; will still need to be checked individually.
;
; In fact, return value coercion brings with it another problem: We
; can't set fields very well. Attempts to use reflection to set an int
; field with a java.lang.Integer will fail on Jarc because it's been
; coerced to a jarc.Int, and the same attempts will fail on Rainbow
; because it's been coerced to a java.lang.Long.
;
;
; To see if this library works, check whether my.jvm is set. That
; function, just like sniff.arc's 'plt macro, is set to nil when on an
; unsupportive platform.
;
; It's also the main entry point of this library. The 'jvm function
; provides a way to navigate among wrapper functions for packages,
; classes, methods, fields, and so forth. For instance, the class
; wrapper function for java.lang.Object can be obtained by saying
; jvm!java!lang!Object.
;
; To be easier on the fingers, a call of jvm!java-lang-Object parses
; its argument and calls ((jvm "java") "lang-Object"), which has the
; same ultimate result as jvm!java!lang!Object. Note that the syntax
; jvm!java.lang.Object would never work, since it ssexpands to
; (((jvm 'java) lang) Object). Nevertheless, the dot is supported so
; that (jvm 'java.lang.Object) and (jvm "java.lang.Object") can work.
;
; For the purposes of 'jvm, the package/class hierarchy is extended
; with a few more nodes. Beneath each class node are the nodes
; "class", "new", ">staticmethod", ">instancemethod", ">field",
; ">class", and ">instance". Also, looking for any other node of a
; class will redirect through whichever of the ">staticmethod",
; ">instancemethod", ">field", or ">class" nodes is most appropriate.
; They're in that preference order, so looking for a node that's a
; field name as well as a static method name will result in the static
; method wrapper.
;
; To access a member of a class, usually it's just as easy as
; jvm!java-awt-Rectangle-x (which returns a field wrapper function).
; You should really only need to say jvm!java-awt-Rectangle->field-x
; if there's a potential conflict.
;
; The "class" accessor is a convenience so that you can get the class
; itself by saying jvm!java-lang-Object-class, rather than having to
; type the reflection calls yourself.
;
; The "new" accessor simply returns a function that dispatches among
; the class's constructors and calls the best fit.
;
; The ">staticmethod" and ">instancemethod" accessors dispatch among
; static methods or instance methods, respectively, limited to those
; with a given name. They both behave the same way under this API; an
; instance method just takes a "this" parameter before its regular
; parameters. Java actually doesn't allow conflicts between static
; methods and instance methods, but conflicts can still come up for us
; thanks to the way "public static void X.foo( X x )" and
; "public void X.foo()" conflict under our convention but not under
; Java's. Also, Jarc and Rainbow each provide separate ways of
; dispatching among methods depending on whether a static method or
; an instance method is desired, so these accessors correspond nicely
; to the dichotomy already assumed by those platforms.
;
; The ">field" accessor is for fields, obviously. If a static field is
; accessed this way, it's returned immediately, so you can simply say
; jvm!javax-swing-JFrame-EXIT_ON_CLOSE. If you want to assign to the
; static field instead, append an = to the name of the field; that
; gets you a one-argument function. Instance fields work the same way,
; except that the getter is a function that takes one argument (the
; instance) and the setter is a function that takes two arguments (the
; instance and the value). An instance field setter wrapper can also
; be passed just the instance, in which case it will curry itself,
; returning another wrapper function that takes just the value.
;
; The ">class" accessor is for nested classes. It's debatable whether
; nested classes are intrinsic to the JVM classes or just part of the
; Java language, but either way, the java.lang.Class class provides a
; getClasses() method for looking them up, and a nested class's
; getSimpleName() method returns the unmangled name as it's seen in
; Java code, so we're taking advantage of those methods here.
;
; Then there's the ">instance" accessor. As yet another convenience,
; if jvm!foo doesn't immediately stumble upon a class named "foo"
; outside all packages, then it's not only a wrapper for the package
; "foo" but also a wrapper for any member named "foo" in any class.
; If that wrapper is called with a non-symbol, non-string Java value,
; it determines which class it's on based on the class of that value,
; then it redirects through the ">instance" accessor, then it makes
; the call again with the same arguments, including the Java instance.
;
; The ">instance" accessor mainly changes two behaviors of the path:
; It makes instance methods have higher preference than static
; methods, and it makes static fields behave as though they were
; instance fields. That way, jvm!EXIT_ON_CLOSE.main-jframe results in
; an int, even though jvm!javax-swing-JFrame-EXIT_ON_CLOSE.main-jframe
; would raise a can't-call-an-int error. On the other hand,
; ">instance" accessor also causes the "class" and "new" accessors to
; raise errors, pending a better idea of what they should do. (TODO:
; Come up with such an idea, but don't put something in that will
; cause more complication than convenience.)
;
; Some extra effort was taken to make sure inner classes (non-static
; nested classes) could be treated as instance members. In other
; words, you can write (jvm!InnerClass.outer!new) instead of
; jvm!com-example-OuterClass-InnerClass-new.outer. This is done by
; allowing the class wrapper function for an inner class to take an
; outer instance as an argument, returning a sort of curried inner
; class wrapper. The curried wrapper behaves like a normal class
; wrapper in every way except that its "new" accessor passes the outer
; instance to the constructor along with the rest of the arguments.
; This makes "(jvm!InnerClass.outer!new a b c)" the same as Java's
; "outer.new InnerClass( a, b, c )".
;
; At long last, here are some examples:
;
; getting a static field
; jvm!javax-swing-JFrame-EXIT_ON_CLOSE
; jvm!EXIT_ON_CLOSE.jframe-instance
;
; setting a static field
; jvm!com-example-SomeClass-staticField=.new-value
; jvm!staticField=.someclass-instance.new-value
; (jvm!staticField= someclass-instance new-value)
;
; calling a static method
; jvm!java-util-Arrays-asList.array
;
; getting a top-level class as a Class instance
; jvm!java-lang-Object-class
;
; getting a static nested class as a Class instance
; jvm!java-util-HashMap-Entry-class
;
; calling a constructor
; (jvm!java-util-ArrayList-new)
;
; calling an instance method (same as a static one, but shortenable)
; (jvm!java-util-HashMap-get map key)
; (jvm!get map key)
;
; getting an instance field
; jvm!java-awt-Rectangle-x.rect
; jvm!x.rect
;
; setting an instance field
; (jvm!java-awt-Rectangle-x= rect newx)
; jvm!java-awt-Rectangle-x=.rect.newx
; (jvm!x= rect newx)
; jvm!x=.rect.newx
;
; accessing an inner class (a non-static nested class) as a Class
;   instance
; jvm!com-example-OuterClass-InnerClass-class
; jvm!InnerClass.outer!class
;
; constructing an instance of an inner class
; jvm!com-example-OuterClass-InnerClass-new.outer
; (jvm!InnerClass.outer!new)
;
; some makeshift importing
; (= jutil jvm!java-util
;    weakhash jutil!WeakHashMap-new)
; (def jweak (x)
;   (if (ajava x jutil!Map-class)
;     weakhash.x
;       (isa x 'table)
;     (let result (weakhash)
;       (each (k v) x
;         (jvm!put result k v))
;       result)
;     (err "A non-map was passed to 'jweak.")))


; We'll make local copies of the jarcdrop* and rainbowdrop* flags just
; so that people who use this package don't also need to import
; sniff.arc.
(= my.jarcdrop*     sn.jarcdrop*
   my.rainbowdrop*  sn.rainbowdrop*)


; This is way more of a general-purpose utility than a JVM one, but
; utils.arc depends on this file, not the other way around.
(=fn my.niceuniq (name)
  (sym:string (uniq) '- name))


(if sn.anyjvmdrop*
  (=mc my.=jfn (name parms . body)
    `(=fn ,name ,parms ,@body))
  (=mc my.=jfn (name parms . body)
    `(=fn ,name ,parms
       (err "Dropping to Java isn't supported on this platform."))))

(w/uniq missing
  (if
    
    sn.jarcdrop*
    (=fn my.ajava (x (o type missing))
      (unless (is type missing)
        (zap my.jclass type)
        (unless type (err "The class passed to ajava wasn't found.")))
      (and (or (no ('isInstance jarc.Type.class x))
               ('isInstance jarc.AllowMethodCalls.class x))
           (or (is type missing) ('isInstance type x))))
    
    sn.rainbowdrop*
    (=fn my.ajava (x (o type missing))
      (unless (is type missing)
        (zap my.jclass type)
        (unless type (err "The class passed to ajava wasn't found.")))
      (and (isa x 'java-object)
           (is x rep.x)
           (or (is type missing) (type 'isInstance x))))
    
    (=fn my.ajava (x (o type missing))
      (unless (is type missing)
        my.jclass.type           ; Let these errors have preference.
        (err "The class passed to ajava wasn't found."))
      nil)
    ))

(if
  
  sn.jarcdrop*
  (=fn my.jclass (name)
    (when (isa name 'sym) (zap string name))
    (if (isa name 'string)
      (do (zap [subst "." "-" _] name)
          (errsafe ('java.lang.Class.forName name)))
        (my.ajava name 'java-lang-Class)
      name
      (err "The class name must be a symbol, a string, or a Class.")))
  
  sn.rainbowdrop*
  (=fn my.jclass (name)
    (when (isa name 'sym) (zap string name))
    (if (isa name 'string)
      (do (zap [subst "." "-" _] name)
          (errsafe:java-class name))
        (my.ajava name 'java-lang-Class)
      name
      (err "The class name must be a symbol, a string, or a Class.")))
  
  (=fn my.jclass (name)
    (unless (in type.name 'sym 'string)
      (err "The class name must be a symbol, a string, or a Class."))
    nil)
  )


; For my.jnew, my.jinvoke, and my.jstaticinvoke, the behavior may vary
; between Jarc and Rainbow depending on how they choose to coerce the
; arguments, select the most appropriate methods/constructors, and
; coerce the results.
;
; The same is true for my.jset an my.jget, which use reflection calls
; to achieve their results, and the humongous my.jvm entry point,
; which needs to do its invocation somehow. However, if analogues for
; these three invoker functions are defined, then it is very easy to
; define analogues for those other functions. Both my.jset and my.jget
; take an "invoker" parameter (defaulted to my.jinvoker), and my.jvm
; is based on my.jpathwrapper, which takes parameters for all three of
; these.
;
; In some cases, other functions may use these directly, even if
; they're otherwise configurable. That should only happen when the
; effect is hard to get wrong. For instance, if it's a method call
; with only one possible match, and the types involved have no obvious
; Arc counterparts (like java.lang.Class and java.lang.reflect.Field)
; or very obvious Arc counterparts (like boolean), then we rely upon
; that common sense rather than using the potentially slow replacement
; invoker.

(if
  
  sn.jarcdrop*
  (=fn my.jnew (class . args)
    (zap my.jclass class)
    (aif ('jarc.JavaConstructor.find ('getName class))
      (apply it args)
      (err "A constructor wasn't found.")))
  
  sn.rainbowdrop*
  (=fn my.jnew (class . args)
    (zap my.jclass class)
    (apply java-new class!getName args))
  
  (=fn my.jnew (class . args)
    (err "Dropping to Java isn't supported on this platform."))
  )

(if
  
  sn.jarcdrop*
  (=fn my.jinvoke (object method . args)
    (unless (in type.method 'sym 'string)
      (err "The method name must be a symbol or a string."))
    (aif ('jarc.JavaInstanceMethod.find object string.method args)
      (apply it object args)
      (err "An instance method wasn't found.")))
  
  sn.rainbowdrop*
  (=fn my.jinvoke (object method . args)
    (unless (in type.method 'sym 'string)
      (err "The method name must be a symbol or a string."))
    (java-invoke object sym.method args))
  
  (=fn my.jinvoke (object method . args)
    (err "Dropping to Java isn't supported on this platform."))
  )

(if
  
  sn.jarcdrop*
  (=fn my.jstaticinvoke (class method . args)
    (zap my.jclass class)
    (unless (in type.method 'sym 'string)
      (err "The method name must be a symbol or a string."))
    (zap string method)
    (when (pos #\. method)
      (err "The method name must not have dots in it."))
    (aif ('jarc.JavaStaticMethod.find (+ ('getName class) "." method))
      (apply it args)
      (err "A static method wasn't found.")))
  
  sn.rainbowdrop*
  (=fn my.jstaticinvoke (class method . args)
    (zap my.jclass class)
    (unless (in type.method 'sym 'string)
      (err "The method name must be a symbol or a string."))
    (apply java-static-invoke do.class!getName sym.method args))
  
  (=fn my.jstaticinvoke (class method . args)
    (err "Dropping to Java isn't supported on this platform."))
  )

(if
  
  sn.jarcdrop*
  (=fn my.jarray->list (jarray)
    (let iter ('iterator ('java.util.Arrays.asList jarray))
      (accum acc
        (while ('hasNext iter)
          (do.acc ('next iter))))))
  
  ; Rainbow does this for us.
  sn.rainbowdrop*
  (=fn my.jarray->list (jarray)
    jarray)
  
  (=fn my.jarray->list (jarray)
    (err "Dropping to Java isn't supported on this platform."))
  )


; The my.jvm-path-taker macro is a utility that helps to parse the
; hyphens in the jvm!java-util-Object convenience syntax. It defines a
; function such that all of the following are true:
;
;  - With no arguments, it returns itself.
;  - With a symbol as the first argument, it coerces the symbol to a
;      string and tries again.
;  - With multiple arguments where the first one is a string, it calls
;      itself with the string first and applies the result to the rest
;      of the arguments.
;  - With a single empty string argument, it returns itself.
;  - With a single string argument, where the string contains #\. or
;      #\-, it splits the string on the first occurrence of such a
;      character, it calls itself with the first half, and it calls
;      the result of that call with the second half. (Neither half
;      contains the character that inspired the split.)
;  - With any other arguments (which is to say, either any non-void
;      arguments without a symbol or string at the beginning, or a
;      single argument which isn't a splittable string), it
;      destructures the arguments according to the given destructuring
;      pattern, and it executes the body.
;
; If the body is more than one expression long and the first of its
; expressions is a non-nil, non-ssyntax symbol, that symbol will be
; bound to the function itself like it's the name of an 'rfn.

(=fn my.fn-jvm-path-taker (alternative)
  (afn args
    (iflet (first . rest) args
      (if (in type.first 'sym 'string)
        (caselet firstr string.first "" self
          (aif rest
            (apply self.firstr rest)
              (pos [in _ #\. #\-] firstr)
            (let (firsthead firsttail) (split firstr it)
              (self.firsthead:cut firsttail 1))
            (do.alternative list.firstr self)))
        (do.alternative args self))
      self)))

(=mc my.jvm-path-taker (args-var . alternative)
  (let g-self car.alternative
    (aif (and anormalsym.g-self cdr.alternative)
      (= alternative it)
      (= g-self my.niceuniq!self))
    `(,my!fn-jvm-path-taker (fn (,args-var ,g-self) ,@alternative))))


(my:=jfn my.jpathwrapper (path constructor invoker staticinvoker)
  (when (isa path 'sym) (zap string path))
  (unless (isa path 'string)
    (err "The path must be a symbol or a string."))
  (my:jvm-path-taker (first . rest)
    (if (isa first 'string)
      (let newpath (+ path (case path "" "" ".") first)
        (aif my.jclass.newpath
          (my.jclasswrapper it constructor invoker staticinvoker)
          (my.jpathwrapper newpath
            constructor invoker staticinvoker)))
        my.ajava.first
      (apply (my.jclasswrapper (my.jinvoke first 'getClass)
               constructor invoker staticinvoker)
        '>instance path first rest)
      (err "Unrecognized arguments to a path wrapper."))))

(= my.jvm (when sn.anyjvmdrop*
            (my.jpathwrapper "" my.jnew my.jinvoke my.jstaticinvoke)))

(with (is-static [my.jstaticinvoke
                   "java.lang.reflect.Modifier"
                   'isStatic
                   (my.jinvoke _ 'getModifiers)]
       missing (uniq))
  (my:=jfn my.jclasswrapper (class constructor invoker staticinvoker)
    (my:jvm-path-taker (name . rest1) bigself
      (if (~isa name 'string)
        (if (~and my.ajava.name
                  (my.jinvoke class 'isMemberClass)
                  ; NOTE: We're avoiding "do.is-static.class" because
                  ; Jarc warns us that it can't find the class.
                  (~do.is-static class))
          (err "Unrecognized arguments to a class wrapper.")
          ; TODO: Find or write a library that uses inner classes, and
          ; actually test this.
          (apply (my:jvm-path-taker (nextname)
                   (unless (isa nextname 'string)
                     (err:+ "Unrecognized arguments to an "
                            "instance-bound inner class wrapper."))
                   (case nextname "new"
                     (fn args
                       (apply constructor class name args))
                     do.bigself.nextname))
                 rest1))
        (case name
          "class"
            class
          "new"
            (fn args
              (apply constructor class args))
          ">staticmethod"
            (fn (name)
              (unless (in type.name 'sym 'string)
                (err "The method name must be a symbol or a string."))
              (zap string name)
              (fn args
                (apply staticinvoker class name args)))
          ">instancemethod"
            (fn (name)
              (unless (in type.name 'sym 'string)
                (err "The method name must be a symbol or a string."))
              (zap string name)
              (fn (object . args)
                ; This is a bit circuitous, in that
                ; jvm!java-util-ArrayList-get can be used in all the
                ; same places jvm!java-util-HashMap-get can. All
                ; that's used is the method's name.
                (apply invoker object name args)))
          ">field"
            (fn (name)
              (when (isa name 'sym) (zap string name))
              (unless (isa name 'string)
                (err "The field name must be a symbol or a string."))
              (let setting (endmatch "=" name)
                (when setting
                  (zap [cut _ 0 (- len._ 1)] name))
                (withs (field (my.jinvoke class 'getField name)
                        currier (if setting
                                  (fn (object (o value missing))
                                    (let (getter setter)
                                           (my.jgetset object field)
                                      (if (is value missing)
                                        [do.setter _ invoker]
                                        (do.setter value invoker))))
                                  [my.jget _ field invoker]))
                  (if do.is-static.field
                    do.currier.nil
                    currier))))
          ">class"
            ; If we didn't return a jvm-path-taker function here and
            ; returned a regular (fn ...) instead, then forms like
            ; jvm!java-util-AbstractMap->class-SimpleEntry-new
            ; wouldn't work, even though corresponding forms like
            ; jvm!java-util-AbstractMap-SimpleEntry-new would work.
            ; (The "SimpleEntry" part would be parsed out in the
            ; second case because of "SimpleEntry-new" being passed to
            ; the class wrapper directly. The class wrapper itself is
            ; already a jvm-path-taker, so it trims off the
            ; "SimpleEntry" portion.)
            (my:jvm-path-taker (name)
              (unless (isa name 'string)
                (err:+ "The nested class name must be a symbol or a "
                       "string."))
              (let nested (keep [is name
                                    (my.jinvoke _ 'getSimpleName)]
                            (my.jarray->list:my.jinvoke
                              class 'getClasses))
                (unless nested (err "A nested class wasn't found."))
                (when cdr.nested
                  (err:+ "Somehow a class provided multiple nested "
                         "classes with the desired name."))
                (my.jclasswrapper car.nested
                  constructor invoker staticinvoker)))
          ">instance"
            (my:jvm-path-taker (name . rest2) littleself
              (aif
                
                (or (~isa name 'string)
                    (in name '(">staticmethod" ">instancemethod"
                               ">field" ">class")))
                (apply bigself name rest2)
                
                (is name ">instance")
                littleself
                
                (in name "class" "new")
                (err:+ "The \"class\" and \"new\" accessors aren't "
                       "supported on instance-expecting class "
                       "wrappers.")
                
                (keep [is name (my.jinvoke _ 'getName)]
                  (my.jarray->list:my.jinvoke class 'getMethods))
                (do.bigself (if (all is-static it)
                              '>staticmethod
                              '>instancemethod)
                            name)
                
                (let realname (if (endmatch "=" name)
                                (cut name 0 (- len.name 1))
                                name)
                  (find [is realname (my.jinvoke _ 'getName)]
                    (my.jarray->list:my.jinvoke class 'getFields)))
                (let result do.bigself!>field.name
                  (if do.is-static.it
                    (fn (object (o value missing))
                      (if (is value missing)
                        result
                        do.result.value))
                    result))
                
                ; else
                do.bigself.name
                ))
          ; else
            (aif
              
              (keep [is name (my.jinvoke _ 'getName)]
                (my.jarray->list:my.jinvoke class 'getMethods))
              (do.bigself (if (some is-static it)
                            '>staticmethod
                            '>instancemethod)
                          name)
              
              (let name (if (endmatch "=" name)
                          (cut name 0 (- len.name 1))
                          name)
                (some [is name (my.jinvoke _ 'getName)]
                  (my.jarray->list:my.jinvoke class 'getFields)))
              do.bigself!>field.name
              
              (some [is name (my.jinvoke _ 'getSimpleName)]
                (my.jarray->list:my.jinvoke class 'getClasses))
              do.bigself!>class.name
              
              ; else
              (err "A field, method, or nested class wasn't found.")
              ))))))


; The Class class does not itself have any fields, so we can get by
; with a single pair of methods, my.jget and my.jset, for both static
; and instance fields, by distinguishing the cases based on whether
; the instance being accessed is a Class or not.
;
; Note that 'jset accepts a symbol, a string, or a
; java.lang.reflect.Field, whereas 'jget only accepts a symbol or a
; string.

; NOTE: Jarc doesn't treat macros in optional arguments properly, so
; we're going to bind 'my and 'jinvoke lexically to values that let us
; keep using the syntax we like.
(with (my idfn
       jinvoke my.jinvoke)

; NOTE: Rainbow's profiler doesn't like function calls in optional
; arguments.
(with (missing (uniq)
       al (when sn.anyjvmdrop*
            (map [list (my.jinvoke
                         (my.jinvoke
                           (my.jclass:+ "java.lang." _
                             (case _ Char "acter" Int "eger"))
                           'getField "TYPE")
                         'get nil)
                       (sym:+ "get" _)
                       (sym:+ "set" _)]
              '(Boolean Byte Char Double Float Int Long Short)))
       fieldclass my.jclass!java-lang-reflect-Field
       classclass my.jclass!java-lang-Class
       equals (awhen my.jvm it!java-lang-Object-equals))
  (my:=jfn my.jgetset (object field)
    (unless (my.ajava field fieldclass)
      (when (isa field 'sym) (zap string field))
      (unless (isa field 'string)
        (err:+ "The field name must be a symbol, a string, or a "
               "java.lang.reflect.Field."))
      (zap [let class (if (and my.ajava.object
                               (~my.ajava object classclass))
                        (my.jinvoke object 'getClass)
                        my.jclass.object)
             (my.jinvoke class 'getField _)]
           field))
    (unless field (err "A field wasn't found."))
    (withs (fieldtype (my.jinvoke field 'getType)
            (type get set) (or (find [equals fieldtype car._] al)
                               (list nil 'get 'set)))
      (list (fn ((o invoker missing))
              (when (is invoker missing)
                (= invoker my.jinvoke))
              (do.invoker field get object))
            (fn (value (o invoker missing))
              (when (is invoker missing)
                (= invoker my.jinvoke))
              (do.invoker field set object value)
              value)))))

; NOTE: Rainbow's profiler doesn't like function calls in optional
; arguments.
(w/uniq missing
  (my:=jfn my.jget (object field (o invoker missing))
    (when (is invoker missing)
      (= invoker my.jinvoke))
    (let (getter setter) (my.jgetset object field)
      do.getter.invoker)))

; NOTE: Rainbow's profiler doesn't like function calls in optional
; arguments.
(w/uniq missing
  (my:=jfn my.jset (object field value (o invoker missing))
    (when (is invoker missing)
      (= invoker my.jinvoke))
    (let (getter setter) (my.jgetset object field)
      (do.setter value invoker))))

)

; TODO: Make a more comprehensive plan regarding 'defset and the
; module system, particularly the part of the module system that has
; to do with 'activate. With the following setter code, if this
; package is activated, (= (jget ...) ...) doesn't work, since the
; 'setter entry for 'jget isn't set.

; To use this, write (= (my:jget object field) value), where 'my is
; the namespace being used for this package.
(=fn (setter my!jget) ((callee object field . invoker))
  (when cdr.invoker
    (err "There were more than four setter arguments to 'jget."))
  (w/uniq (g-get g-set g-invoker)
    `(((,g-get ,g-set) (,my!jgetset ,object ,field)
       ,g-invoker ,(if invoker
                     car.invoker
                     my!jinvoke))
      (,g-get ,g-invoker)
      [,g-set _ ,g-invoker])))


(let jiso2 (awhen my.jvm it!java-lang-Object-equals)
  (=fn my.jiso args
    (iflet judge (find my.ajava args)
      (all [or (is _ judge) (do.jiso2 judge _)] args)
        args
      (err "Only non-Java arguments were passed to 'jiso.")
      t)))


)
