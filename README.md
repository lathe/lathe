# Lathe

Lathe is a collection of miscellaneous personal utilities by me,
rocketnia, for use in various languages. No language offers quite what
I want it to, so Lathe smoothes the experience. The first (and one of
the most ambitious) parts of Lathe was the collection of libraries for
Arc, which started with making an Arc module system since Arc didn't
come with one of its own.

As with many utilities libraries, Lathe is rather unstable and
undocumented.


## Arc


### Features

  - A very young Arc module system based on renaming of global
      variables (arc/modules/).

  - A young multimethod system built up within that module system
      (arc/multival/).

  - A continuation-based backtracking library (arc/amb.arc).

  - An extensive continuation-free, combinator-style iterator library
      which nevertheless supports continuation-based, coroutine-style
      iterator specification as well, as long as the language
      implementation supports it (arc/iter.arc).

  - Some more general-purpose modules the multimethod system relies on
      (arc/rules.arc and arc/utils.arc).

  - An updated version of Andrew Wilcox's 'extend macro so that
      extensions can be removed and replaced (arc/extend.arc).

  - Very small examples of using the module system, the multimethod
      framework, and the iteration library in application code
      (arc/examples/), which double as the only test cases to date.

The examples (and therefore the tests) don't cover everything Lathe
has to offer. Lathe's Arc code is fairly well commented, so reading
those comments is probably one of the best ways to get a good feel for
things.


### Setup

First, get the Arc language by following the instructions at [https://arclanguage.github.io/](https://arclanguage.github.io/).
There are many versions of Arc, and Lathe is designed to work with
several:

* [Anarki and Anarki Stable](https://arclanguage.github.io/), which
  are community-maintained versions of Arc.

* [Arc 3.1](http://arclanguage.org/item?id=10254), the last official
  release of Arc from August 2009, which has several known issues.

* [ar](https://github.com/awwx/ar), Andrew Wilcox's fork of Arc 3.1
  which makes Arc use Racket's mutable cons cells instead of unsafely
  mutating the immutable ones.

* [arc-nu](https://github.com/arclanguage/arc-nu), Pauan's fork of ar
  which is heavily refactored.

* [Jarc](http://jarc.sourceforge.net/), JD Brennan's JVM
  implementation of Arc, which omits continuation support and has
  syntaxes for easy interaction with other JVM code, making it fit in
  with the JVM ecosystem.

* [Rainbow](https://github.com/conanite/rainbow), Conan Dalton's JVM
  implementation of Arc, optimized for speed.

* [Rainbow.js](https://github.com/conanite/rainbow), my port of
  Rainbow to JavaScript.

To load the core Lathe libraries, first copy the Lathe code into
lib/lathe/ or some other foler relative to your Arc directory, and
then run:

```
(= lathe-dir* "lib/lathe/")
(load:+ lathe-dir* "loadfirst.arc")
```

The loadfirst.arc code looks at the lathe-dir* global variable to
determine where to load other Lathe files from, so you must set that
variable as shown.

If you want to use a Lathe module, you can do this:

```
(use-rels-as ut (+ lathe-dir* "utils.arc"))
```

This will set 'ut to a namespace from which you can access all the
things defined in utils.arc. A Lathe namespace is just a macro that
associates friendly symbols with less friendly global names, and you
can use a namespace as follows:

```
; function calling with (ut.foo ...)
(ut.foldl (fn (a b) (+ (* 10 a) b)) 0 '(1 2 3))

; macro usage with (ut:foo ...), which also works for function calls
(ut:foldlet a 0
            b '(1 2 3)
  (+ (* 10 a) b))

; using ut.foo lookup somewhere other than function position
(iso (flat:map ut.tails (ut:tails:list 1 2 3))
     '(1 2 3  2 3  3
              2 3  3
                   3))

; using ut.foo as a settable place
(= old-tails ut.tails
   ut.tails [do (prn "DEBUG: entering utils.arc's 'tails")
                old-tails._])

; lookup of unevaluated global names with ut!foo
(mac afoldl (aval bval . body)
  `(,ut!foldlet a ,aval b ,bval ,@body))
```

If you're making an Arc application that uses Lathe libraries, that
should be enough to get started, but you may need to dig through some
code to find the utilities you actually want to import this way.

If instead you're making a library, then you can continue using
'use-rels-as like this, but as long as you're using Lathe already,
you might consider making your library into a module. Take a look at a
few of the modules included with Lathe to see how to do that.


## Racket


### Features

Lathe for Racket is just a simple utility library. It's likely to stay
pretty unexciting; if it contained anything notable, I would rather
release that part as a separate Racket library anyway. Right now,
Lathe just includes some syntaxes that reduce parentheses in simple
ways.


### Setup

Since Racket already has a module system and a package manager, you
can run `raco pkg install lathe` at a command line and then import
Lathe in your program like so:

```
(require lathe)
(require (for-syntax lathe))
```


## JavaScript


### Features

Most of Lathe's JavaScript utilities are all in one file, js/lathe.js.
These began with ports of the Arc utilities, but they soon took on a
life of their own to include additional utilities for iframe
manipulation and binary encodings.

The file js/lathe-fs.js includes some utilities for easier filesystem
access in Node.js.

The file js/chops.js contains a kernel for string-based interpreted
languages, like macro systems and templating engines.

The file js/choppascript.js is a particular Chops DSL for generating
JavaScript code. Its most useful feature is a `[str ...]` macro for
writing multi-line strings.


### Setup

Lathe for JavaScript is made up of unminified files that can be copied
and pasted into a project as needed. These files will all work in
Node.js, and everything but lathe-fs.js will work in the browser as
well.

There's currently no npm package for Lathe. It's likely I'll want to
break up Lathe's feature set into several independent npm packages, so
I don't want to publish one large package only to delete from it over
time.
