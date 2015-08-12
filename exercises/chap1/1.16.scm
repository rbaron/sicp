; Lets start with a naïve iterative solution:

(define (exp-iter b n acc)
  (if (< n 1)
    acc
    (exp-iter b (- n 1) (* acc b))))

(define (exp b n) (exp-iter b n 1))

(exp 2 3)
; => 8

(exp 3 3)
; => 27

(exp 2 10)
; => 1024

(exp 2 11)
; => 2048


; So far, so good. Now let's try and leverage the successive squaring process.
; Let's examine the following case:
;
; 2^16 = ((((2^2)^2)^2)^2) = (2^2)^(16/2)
;
; We can see that the _base_ `b` is actually changing from iteration to iteration
; when `n` is even. In that case, the next iteration will receive:
;
; b <- b*b
; n <- n/2
;
; When n is odd, we'll have:
;
; 2^17 = 2*2^16
;
; In which case the base remains the same but we decrement n by 1.

(define (even? n)
   (= (remainder n 2) 0))

(define (fast-exp-iter b n acc)
  (if (even? n)
    (fast-exp-iter (* b b) (/ n 2) acc)
    (fast-exp-iter b (- n 1) (b*acc))))

(exp 2 3)
; => 8

(exp 3 3)
; => 27

(exp 2 10)
; => 1024

(exp 2 11)
; => 2048
