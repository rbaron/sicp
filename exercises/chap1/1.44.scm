; Smoothing prodecedures

; Load this file with
; $ cat 1.44.scm - | mit-scheme

; We're implementing a smooth procedure that receives a procedure f
; and returns the smoothed version of f.

; Left big on purpose so we can see the smoothing effects better
(define dx 0.1)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
        3)))

; Testing
(define (cube x) (* x x x))

((smooth cube ) 2.0)
; => 8.04

; The exercise also asks us to implement a n-fold-smooth procedure
; that re-applies the smooth function sequentially n times.

; Solution of exercise 1.43
(define (repeated f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))))

; Solution of exercise 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

; Solution
(define (n-fold-smooth f n)
  (lambda (x)
    (((repeated smooth n) f) x)))

; Testing

((n-fold-smooth cube 1) 2.0)
; => 8.04

((n-fold-smooth cube 7) 2.0)
; => 8.28
