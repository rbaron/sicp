; To devise the iterative version of the multiplication algorithm `m`
; from exercise 1.17, we can start by thinking about having a new state
; variable to be passed around in recursive calls.
; I like to call such variables "accumulators", or `acc`.

; Available procedures
(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (even? n)
   (= (remainder n 2) 0))

; Logarithmic linear recursive multiplication
(define (m-iter a b acc)
  (if (= b 0)
    0
    (if (even? b)
      (m-iter (double a) (halve b) acc)
      (m-iter a (- b 1) (+ acc a)))))

(define (m a b) (m-iter a b 0))

(m 4 5)
; => 20

(m 3 4)
; => 12
