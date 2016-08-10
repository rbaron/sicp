; Doubling procedures

; Load this file with
; $ cat 1.41.scm - | mit-scheme

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

; Testing

((double inc) 0)
; => 2

; What is the value of the following expression?

; (((double (double double)) inc) 5)

; The inner (double double) can be rewritten as double-2:
(define (double-2 f)
  (lambda (x) ((double (double f)) x)))

; Likewise for the outer double:
(define (double-final f)
  (lambda (x) ((double (double (double (double f)))) x)))

; Thus, the given expression applies inc 16 times.

; Testing
(((double (double double)) inc) 5)
; => 21 = 5 + 16

((double-final inc) 5)
; => 21
