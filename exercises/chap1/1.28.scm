; Miller-Rabin primality test:
;
; if n is a prime number and a is any positive integer less than n,
; then a raised to the (n - 1)st power is congruent to 1 modulo n.

; That is, for some `a < n`, all primes satisfy `(a^(n-1))&n = 1&n 1` (since `a < n`).
; Non-primes _may_ also  satisfy the equation.

; Let's modify the procedure `expmod` to return `0` should it find a non-trivial
; square root of `1 mod n`


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder (square-sig (expmod base (/ exp 2) m) m)
                     m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))


(define (square-sig b n)
  (if (and (> b 1) (< b (- n 1)) (= (remainder (square b) n) 1))
    0
    (square b)))


(define (mr-test n a)
  (if (= (expmod a (- n 1) n) 0)
    false
    true))


(define (mr-test-all-it n a)
  (if (mr-test n a)
    (if (= (- n 1) a)
      true
      (mr-test-all-it n (+ a 1)))
    false))


(define (mr-test-all n) (mr-test-all-it n 1))

; Testing with composite numbers (all return #f)

(mr-test-all 561)
(mr-test-all 1105)
(mr-test-all 1729)
(mr-test-all 2465)
(mr-test-all 2821)
(mr-test-all 6601)

; Testing with prime numbers (all return #t)

(mr-test-all 599)
(mr-test-all 1723)
(mr-test-all 3499)
(mr-test-all 8179)
(mr-test-all 27763)
(mr-test-all 38891)
