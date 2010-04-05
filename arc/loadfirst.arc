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

; The order things are loaded here is not the only valid order;
; modulemisc.arc provides the 'tldo macro for once.arc, and once.arc
; provides the 'once-tl macro for the other four files. However, the
; order shown here is such that the loading can be cut off at any
; point between files without any of the loaded features being
; incomplete and broken.
;
(each file '(modulemisc once nspace import package rel)
  (load:string lathe-dir* 'modules/ file '.arc))