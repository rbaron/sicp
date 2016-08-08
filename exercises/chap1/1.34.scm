; Perverse high order procedure application

; Load this file with
; $ cat 1.34.scm - | mit-scheme

; Given high order procedure
(define (f g)
  (g 2))

; Calling
; (f f)

; Will evaluate the body of f with g bound to f:
; (f 2)

; This, in turn, will evaluate the body of f with g bound to 2:
; (2 2)

; Since 2 is not a procedure, this will yield an error.

; Uncomment to try it out:
; (f f)
; => The object 2 is not applicable.
