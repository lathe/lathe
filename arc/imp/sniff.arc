; sniff.arc

(packed


; Jarc doesn't support re-invocable continuations, and we don't blame
; it. This flag indicates whether the feature is supported.
(= my.cccraziness* (errsafe:iflet c catch.throw (c nil) t))


)