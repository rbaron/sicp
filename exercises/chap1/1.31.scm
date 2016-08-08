; Abstracting product using procedures

; Load this file with
; $ cat 1.31.scm - | mit-scheme

; Recursive version
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; Iterative version
(define (product-it term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

; Testing. Finding 2^n:
(define (ratio x) 2)
(define (inc x) (+ x 1))

(product ratio 1 inc 10)
; => 1024

(product-it ratio 1 inc 10)
; => 1024

; Let's use it to estimate pi.

; Determine the nominator term procedure
(define (nominator-term k)
  (cond ((odd? k) (+ k 3))
        ((even? k) (+ k 2))))

; Determine the denominator term procedure
(define (denominator-term k)
  (cond ((even? k) (+ k 3))
        ((odd? k) (+ k 2))))

; Let's write a procedure that will give better estimates of
; pi depending on the number of terms of the products:

(define (estimate-pi n product-procedure)
  (* 4. (/ (product-procedure nominator-term 0 inc n)
           (product-procedure denominator-term 0 inc n))))

(estimate-pi 10 product)
; => 3.023170192001361

(estimate-pi 1000 product)
; => 3.140026946105016

; (estimate-pi 100000 product)
; => ;Aborting!: maximum recursion depth exceeded

; Iterative product to the rescure! (It takes a while to run)
 (estimate-pi 100000 product-it)
; => 3.1415769461370178
