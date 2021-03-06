; Fermat's Little Theorem: If n is a prime number and a is any positive
; integer less than n, then a raised to the nth power is congruent to a modulo n.

; That is, for some `a < n`, all primes satisfy `(a^n)&n = a&n = a` (since `a < n`).
; Non-primes _may_ also  satisfy the equation.


; Instead of using the given `fast-prime?` procedure, let's write one that
; runs the fermat test agains every a for 0 <= a < n.

(define (fermat-test-all-it n a)
  (if (= (expmod a n n) a)
    (if (= (- n 1) a)
      true
      (fermat-test-all-it n (+ a 1)))
    false))

(define (fermat-test-all n) (fermat-test-all-it n 1))

; Given helper procedures
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder (square (expmod base (/ exp 2) m))
                     m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (square x) (* x x))


; Since there are numbers that fool the `fermat-test` procedure, we cannot
; be 100% sure that a number that passes the test is, in fact, prime. If a
; number _doesn't_ pass it, though, we can be sure that it is a composite.

; Here are some numbers given on footnote 47 that fool the test (Charmichael numbers):
; 561, 1105, 1729, 2465, 2821, 6601

(fermat-test-all 561)
; => #t

(fermat-test-all 1105)
; => #t

(fermat-test-all 1729)
; => #t

(fermat-test-all 2465)
; => #t

(fermat-test-all 2821)
; => #t

(fermat-test-all 6601)
; => #t
