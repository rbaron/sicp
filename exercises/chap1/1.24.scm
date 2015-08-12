; Given procedures

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; Timed procedure to test primality from exercise 1.22, modified to use `fast-prime?`

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  ; Arbitrarily running `fast-prime?` 1000 times so we can have a very good
  ; likelihood of `n` being prime and also so we can measure time more precisely
    (if (fast-prime? n 1000)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


; Testing the runtime of the 12 found primes:

; 11-digit numbers
(timed-prime-test 10000000019)
(timed-prime-test 10000000033)
(timed-prime-test 10000000061)
; Takes around `.10` s

; 12-digit numbers
(timed-prime-test 100000000003)
(timed-prime-test 100000000019)
(timed-prime-test 100000000057)
; Takes around `.1` s

; 13-digit numbers
(timed-prime-test 1000000000039)
(timed-prime-test 1000000000061)
(timed-prime-test 1000000000063)
; Takes around `.1` s

; Lets try with an arbritrary 30-digit prime number from https://primes.utm.edu/lists/small/small.html
(timed-prime-test 671998030559713968361666935769)
; Takes around `.31` s

; 80-digit number from same source
(timed-prime-test 282755483533707287054752184321121345766861480697448703443857012153264407439766013042402571)
; Takes around `1.05` s

; 100-digit number from same source
(timed-prime-test 2367495770217142995264827948666809233066409497699870112003149352380375124855230068487109373226251983)
; Takes around `1.14` s


; Summarizing the results:
; digits(n) | time  | increase | log(increase factor)
;       11  |   .10 |       -- |            --
;       12  |   .10 |      .00 |     1k.log(10) = 0.0
;       13  |   .10 |      .00 |     2k.log(10) = 0.0
;       30  |   .31 |      .21 |    19k.log(10) = 0.21 => k ~ 3.3e-3
;       80  |  1.05 |      .95 |    69k.log(10) = 0.95 => k ~ 4.1e-3
;      100  |  1.14 |     1.04 |    89k.log(10) = 1.14 => k ~ 3.8e-3
;
; Since `k` remains more or less constant, these results suggest that the
; runtime increases logarithmic with `n`. A more straight-forward conclusion
; can br drawn by the fact that the time required to test a 30-digit number
; is roughly 30/100 that the time required to test a 100-digit number.
