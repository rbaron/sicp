; The task is to write a procedure `search-for-primes`. It looks for prime
; numbers within a given range.

; `prime?` O(sqrt(n)) implementation
(define (prime? n)
   (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

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
; It tests primality for integers in the closed range `[a, b]`.
; For simplicity, it is assumed that `a` is odd.
; This is a... naïve implementation, since it checks for pimality twice,
; just so we can keep track of how many primes were found without having to
; modify the given functions

(define (search-for-primes a n-found)
  (timed-prime-test a)
  (if (< n-found 3)
    (if (prime? a)
      (search-for-primes (+ a 2) (+ n-found 1))
      (search-for-primes (+ a 2) n-found))))


; Finding the first 3 primes greater than 1000:
(search-for-primes 1001 0)

;1009 *** 0.
;1013 *** 0.
;1019 *** 0.

; I'm running this on a intel i5 processor. Apparently the given order of magnitude
; for `n` doesn't seem to be relevante for the timing function. Let's try with some
; bigger `n`:

(search-for-primes 10000000001 0)

; 10000000019 *** .12000000000000002
; 10000000033 *** .10999999999999999
; 10000000061 *** .10999999999999999

; So it takes ~ .1 s for the procedure `is-prime` to run on a 11-digit number.
; Let's try for a 12-digit `n`:

(search-for-primes 100000000001 0)

; 100000000003 *** .3599999999999999
; 100000000019 *** .33999999999999986
; 100000000057 *** .33999999999999986

; As expected, the time required is about sqrt(10) greater. For a 13-digit number:


(search-for-primes 1000000000001 0)

; 1000000000039 *** 1.08
; 1000000000061 *** 1.0999999999999996
; 1000000000063 *** 1.0900000000000007

; Again, the time requirement seems to have been multiplied by sqrt(10)! Cool!
