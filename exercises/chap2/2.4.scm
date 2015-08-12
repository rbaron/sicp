; Let's use the substitution method for analysing the call:

; (car (cons x y))

; Where `car` and `cons` are given by the alternative implementations:

(define (cons x y)
 (lambda (m) (m x y)))

(define (car z)
   (z (lambda (p q) p)))


; What happens is:
; (car (cons x y))
; (car (lambda (m) (m x y)))
; ((lambda (m) (m x y)) (lambda (p q) p))
; (lambda (x y) x)
; x

; Implementing `cdr` is very straight-forward once we
; have the `car` procedure. It's analogous, but returns
; the second argument instead of the first one:

(define (cdr z)
   (z (lambda (p q) q)))

; Let's try this out:
(cdr (cons 1 2))
; => 2
