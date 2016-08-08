; Abstracting monoids

; Load this file with
; $ cat 1.32.scm - | mit-scheme

; a. Recursive accumulate
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner
                          null-value
                          term
                          (next a)
                          next
                          b))))

; b. Iterative accumulate
(define (accumulate-it combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a)
            (combiner (term a) result))))
  (iter a null-value))

; We can now write sum and product in terms of accumulate:

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (sum-it term a next b)
  (accumulate-it + 0 term a next b))

; Testing. Summing natural numbers between 0 and 100

(define (id x) x)
(define (inc x) (+ x 1))

(sum id 0 inc 100)
; => 5050

;(sum id 0 inc 100000)
; => Aborting!: maximum recursion depth exceeded

; Again, iterative procedure to the rescure!

(sum-it id 0 inc 100000)
; => Value: 5000050000
