; Analyzing the `expand` procedure

; Given procedure
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

; What does it do?

; Let's trace the execution of two calls:

; 1. (expand 1 7 10)

; num  den   radix   quotient
;  1    7      10       1
;  3    7      10       4
;  2    7      10       2
;  6    7      10       8
;  4    7      10       5
;  5    7      10       7
;  1    7      10       1
;  (repeats...)


; 2. (expand 3 8 10)

; num  den   radix   quotient
;  3    8      10       3
;  6    8      10       7
;  4    8      10       5
;  0    8      10       0
;  0    8      10       0
;     (repeats zeros)


; Upon inspection of the traced calls, we can see that the
; procedure `expand` calculates the decimal places from
; integers divisions between `num` and `den` in base `radix`.
