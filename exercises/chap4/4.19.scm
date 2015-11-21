; Expected result of evaluating

; (let ((a 1))
;   (define (f x)
;     (define b (+ a x))
;     (define a 5)
;     (+ a b))
;   (f 10))

; Im my opinion, Eva's view makes sense. `b` and `a` should be
; simultaneous, so both reflect the expression evaluated with
; the correct enviroment bindings at any time.

; One idea to implement Eva's view is to try and order definitions
; in a order such that dependent definitions comes afterwards. It
; might not be possible if we have circular dependencies, in which
; case we could signal an error.
