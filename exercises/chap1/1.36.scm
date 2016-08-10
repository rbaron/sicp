; Findind a solution of x^x = 1000

; Load this file with
; $ cat 1.36.scm - | mit-scheme

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess n)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          (begin
            (display "\nTook ")
            (display n)
            (display " iterations to converge")
            next)
          (try next (+ n 1)))))
  (try first-guess 0))

(define (average v1 v2) (/ (+ v1 v2) 2))

; Solution

(define (f x) (/ (log 1000) (log x)))
(define (f-damped x) (average x (/ (log 1000) (log x))))

(fixed-point f 2.)
; => Took 33 iterations to converge
; => 4.555532270803653

(fixed-point f-damped 2.)
; => Took 8 iterations to converge
; => 4.555537551999825
