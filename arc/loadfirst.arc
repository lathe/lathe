; loadfirst.arc.
;
; Use this as follows, replacing the specific path as necessary:
;
; (= lathe-dir* "lib/lathe/")
; (load:+ lathe-dir* "loadfirst.arc")
;
; The global variable is necessary; it's what this file uses to know
; where to load from.
;
; A good place to put this is at the end of libs.arc (but before
; loading any code that relies on it, naturally).

; The order things are loaded here is actually the *only* valid order.
;
;  modulemisc  provides  'tldo                       to  once
;  once        provides  'once                       to  nspace
;  nspace      provides  'nspaced                    to  import
;  import      provides  'import-sobj                to  package
;  package     provides  'compile-dependency-rules*  to  rel
;
(each file '(modulemisc once nspace import package rel)
  (load:string lathe-dir* 'modules/ file '.arc))