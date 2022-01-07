# Lathe

Lathe is a collection of libraries that aim to smooth out the rough edges and feature gaps of various programming languages in order to work toward smoother overall language designs.

This repository is for tracking assets, notes, and other supplementary materials that apply to the Lathe project as a whole. Currently, there isn't much here.


## What happened to the code in this repository?

Lathe started out with a module system and various libraries for Arc. Then it branched out into some JavaScript and Racket utilities as well. At one point, these were all maintained in this one repository. Presently, they're factored out into individual repositories with somewhat narrower focuses.

* [Framewarc](https://github.com/rocketnia/framewarc) is the new name for the Arc part of Lathe. This consists of a module system and a set of various libraries built for running portably on multiple Arc implementations. It's been broken off from the Lathe project to allow it to develop in new directions.

* [Lathe Comforts for JS](https://github.com/lathe/lathe-comforts-for-js) is the new name for the JavaScript part of Lathe. This consists of miscellaneous utilities, including utilities for manipulating iframes and writing text preprocessors.

* [Parendown for Racket](https://github.com/lathe/parendown-for-racket) and [Lathe Comforts for Racket](https://github.com/lathe/lathe-comforts-for-racket) have eclipsed the few rudimentary Racket utilities that were once part of this repo. At this point, the [Lathe organization](https://github.com/lathe) has several Racket libraries aside from these.

* Some initial work on [Punctaffy for Racket](https://github.com/lathe/punctaffy-for-racket) took place in this repo, but it moved to its own repo before it reached a stable state.
