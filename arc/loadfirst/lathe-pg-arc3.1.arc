; lathe-pg-arc3.1.arc.
;
; Use this as follows, replacing the specific path as necessary:
;
; (= lathe-dir* "lib/lathe/")
; (load:+ lathe-dir* "loadfirst/lathe-pg-arc3.1.arc")
;
; The global variable is necessary; it's what this file uses to know
; where to load from.
;
; A good place to put this is at the end of libs.arc (but before
; loading any code that relies on it, naturally).

; The order things are loaded here is actually the *only* valid order.
;
;  modulemisc  provides  'tl                         to  once
;  once        provides  'once                       to  nspaced
;  nspaced     provides  'nspaced                    to  import
;  import      provides  'import-sobj                to  package
;  package     provides  'compile-dependency-rules*  to  rel
;
(each file '(modulemisc once nspaced import package rel)
  (load:string lathe-dir* 'modules/ file '.arc))