; Estimating e using Euler's continued fractions

; Load this file with
; $ cat 1.38.scm - | mit-scheme

; Solution of exercise 1.37
(define (cont-frac-iter n d k)
  (define (inner counter accumulated)
    (if (= counter 0)
      accumulated
      (inner (- counter 1)
             (/ (n counter)
                (+ (d counter)
                    accumulated)))))

  (inner (- k 1) (/ (n k) (d k))))

; Solution

; Let's find a function that receives the sequence index
; and gives us the sequence value:

; INDEX:  1 2 3 4 5 6 7 8 9 10 11
; VALUE:  1 2 1 1 4 1 1 6 1 1  8

; Here we can see that the sequence assumes values different than
; one in indices 2, 5, 8, 11. These indices can be described as numbers
; n for which n-2 is divisible for 3.

; In such indices, the sequence takes values:

; INDEX  VALUE
;  2       2
;  5       4
;  8       6
;  11      8

; Intuitively, we can see that VALUE is a linear function of INDEX (because
; the index increases by 3 as the value increases by 2).

; This linear dependency can be modeled as f(i) = a + bx. Let's substitute
; two pairs of (INDEX, VALUE) and find a and b:

; (INDEX = 2, VALUE = 2)
; 2 = a + 2b => a = 2 - 2b

; (INDEX = 5, VALUE = 4)
; 4 = a + 5b = 2 - 2b + 5b = 2 + 3b

; => b = 2/3
; => a = 2 - 4/3 = (6 - 4)/3 = 2/3

; => f(i) = 2*(1 + i)/3

; Verifying...
; f(2) = 2
; f(5) = 2*6/3 = 4
; f(8) = 2*9/3 = 6
; f(11) = 2*12/3 = 8

; All good!

; Now we have:

(define (d i)
  (if (= (remainder (- i 2) 3) 0)
    (* (/ 2 3) (+ i 1))
    1))

; Testing

(map d (iota 20 1))
; => (1 2 1 1 4 1 1 6 1 1 8 1 1 10 1 1 12 1 1 14)

; Estimating e

(define e-2 (cont-frac-iter
  (lambda (n) 1.)
  d
  1000))

(define e (+ e-2 2))

e
; => 2.7182818284590455
