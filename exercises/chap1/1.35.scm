; Golden ration as a fixed point

; Load this file with
; $ cat 1.35.scm - | mit-scheme

; The golden ratio x is the solution of the equation:
; x^2 = x + 1

; Dividing both sides by x, we get
; x = 1 + 1/x = f(x)

; By solving f(x) = 1 + 1/x = x, we find the fixed point of f(x).

; Given prodecedures

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

; Solution

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
; => 1.6180327868852458
