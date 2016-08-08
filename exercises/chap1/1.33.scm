; Implementing filtered-accumulate

; Load this file with
; $ cat 1.33.scm - | mit-scheme

; Recursive version
(define (filtered-accumulate combiner null-value term a next b satisfies?)
  (if (> a b)
    null-value
    (combiner (if (satisfies? a) (term a) null-value)
              (filtered-accumulate combiner
                                   null-value
                                   term
                                   (next a)
                                   next
                                   b
                                   satisfies?))))

; Iterative version
(define (filtered-accumulate-it combiner null-value term a next b satisfies?)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a)
            (combiner (if (satisfies? a) (term a) null-value)
                      result)))))

; a. Sum of the squares of the prime numbers in the interval a to b
;    (assuming that you have a prime? predicate already written)

; Given procedures
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; Solution
(define (square n) (* n n))
(define (inc n) (+ n 1))

(define (sum-of-squared-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

(sum-of-squared-primes 2 10)
; => 87

; b. Product of all the positive integers less than n that are relatively
;   prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1)

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (id n) n)

(define (product-of-relatively-prime n)
  (define (relative-prime-to-n? a)
    (= (gcd n a) 1))

  (filtered-accumulate * 1 id 2 inc (- n 1) relative-prime-to-n?))

(product-of-relatively-prime 10)
; => 189
