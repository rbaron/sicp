; Repeating procedure applications

; Load this file with
; $ cat 1.43.scm - | mit-scheme

; We're implementing a repeated procedure that receives a procedure f
; and a natural number n and composes f with itself n times.

(define (repeated f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))))

; Where compose is the solution of exercise 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

; Testing

(define (square x) (* x x))

((repeated square 2) 5)
; => 625
