; Let's start by implementing the `next` procedure.

(define (next test-divisor)
  (if (= test-divisor 2)
    3
    (+ test-divisor 2)))

; Let's plug it in the given procedures:

(define (prime? n)
   (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        ; Changed here to use `(next test-divisor)` instead of `(+ test-divisor 1)`
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

; Timed procedure to test primality
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; My function to search for the smallest 3 primes within a given range.
; See exercise 1.22 for implementation details

(define (search-for-primes a n-found)
  (timed-prime-test a)
  (if (< n-found 3)
    (if (prime? a)
      (search-for-primes (+ a 2) (+ n-found 1))
      (search-for-primes (+ a 2) n-found))))

; For a 11-digit number:
(search-for-primes 10000000001 0)

; 10000000019 *** .07
; 10000000033 *** .08
; 10000000061 *** .07

; In general the speedup was from around .1s to .07s (70% of the original time)


; For a 12-digit number:
(search-for-primes 100000000001 0)

; 100000000003 *** .24
; 100000000019 *** .21999999999999997
; 100000000057 *** .21999999999999997

; Speedup was from around .34s to .22s (64% of the original time)

; For a 13-digit number:
(search-for-primes 1000000000001 0)

; 1000000000039 *** .6800000000000002
; 1000000000061 *** .7000000000000002
; 1000000000063 *** .6699999999999999

; Speedup was from around 1.09s to .69s (63% of the original time)


; These results do not confirm our original expectation that the runtime
; would be halved with the implementation of `next`.
; The probable cause is that we now have introduced a few more instructions
; on your procedure, since our `next` procedure has to evaluate an `if` predicate
; on every run.
