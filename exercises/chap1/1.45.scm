; Damping fixed point procedures for finding higher order polynomial
; roots

; Load this file with
; $ cat 1.45.scm - | mit-scheme

; Given procedures
(define (average v1 v2)
  (/ (+ v1 v2) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Solution of exercise 1.43
(define (repeated f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))))

; Solution of exercise 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (damp-n-times f n)
  ((repeated average-damp n) f))

(define (inc x) (+ x 1))

; This test procedure will either produce a result or overflow
(define (test-fixed-point-convergence poly-order n-damping)
  ; Fixed point for x^(poly-order) - 1 = 0
  (define (poly x)
    (/ 1 (expt x (- poly-order 1))))

  (define initial-guess 0.5)

  (fixed-point (damp-n-times poly n-damping) initial-guess))

; Testing

; n-damping = 1
(test-fixed-point-convergence 2 1)
(test-fixed-point-convergence 3 1)
;(test-fixed-point-convergence 4 1)
; => hangs

; n-damping = 2
(test-fixed-point-convergence 4 2)
(test-fixed-point-convergence 5 2)
(test-fixed-point-convergence 6 2)
(test-fixed-point-convergence 7 2)
; (test-fixed-point-convergence 8 2)
; => hangs

; n-damping = 3
(test-fixed-point-convergence 8 3)
(test-fixed-point-convergence 9 3)
(test-fixed-point-convergence 10 3)
(test-fixed-point-convergence 11 3)
(test-fixed-point-convergence 15 3)
; (test-fixed-point-convergence 16 3)
; => hangs

; n-damping = 4
(test-fixed-point-convergence 16 4)
(test-fixed-point-convergence 18 4)
(test-fixed-point-convergence 19 4)
(test-fixed-point-convergence 20 4)
; ...
(test-fixed-point-convergence 31 4)
; (test-fixed-point-convergence 32 4)
; => hangs

; From the tests, it seems like a n-fold-damping is able to find roots
; of polynomials up to the degree 2^(n+1) - 1.
