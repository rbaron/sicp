; Nondeterministic search for Pythagorean triplets
; within an infinite range of integers

; Load this file and drop yourself in the REPL with:
; $ cat 4.36.scm - | mit-scheme

; In order to generate all the triples i, j, k, stating from
; a given integer and with no upper bound, we need to find a way
; of actually visiting all possible triplets. Simply replacing
; `an-integer-between` by `an-integer-starting-from` won't work
; because the last element of the triples would range from
; the lower bound to infinite, leaving no room for the other
; to elements to vary.

; The solution (given below) is to use `an-integer-starting-from` for
; the "topmost" (or most significant index) and bound the two other
; indices by it. This way, we guarantee that, given enough time, all
; triples will be considered.

; Load metacircular evaluator and override it with
; nondeterministic procedures:
(load "book-code/ch4-mceval.scm")
(load "book-code/ch4-ambeval.scm")

; Kick off Amb-Eval REPL. From now on everything is from
; the user's perspective.
(define the-global-environment (setup-environment))
(driver-loop)

; Given "userland" procedures

(define (require p)
  (if (not p) (amb)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

; From exercise 4.35
(define (an-integer-between low high)
  (if (> low high)
    ; (amb) with no choices represent a failed computation
    (amb)
    (amb low (an-integer-between (+ low 1) high))))

; Solution

; To get some initial intuition, we might want to arbitrarily bound
; the values of j and k so that we can be sure the Amb evaluator will
; backtrack eventually and vary the values of j and i, as opposed to
; just k as in the "naïve" approach:
(define (a-pythagorean-triple-from n)
  (let ((i (an-integer-starting-from n)))
    (let ((j (an-integer-between n 1000)))
      (let ((k (an-integer-between n 1000)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

;(a-pythagorean-triple-from 3)
; => (3 4 5)

;try-again
; => Hangs.

; Okay, that works but will not generate all possible triples.
; There are several ways of generating Pythagorean triples [0].
; One of them [1] states that:

; Given

; a^2 + b^2 = c^2,

; If a is odd, lim(b) =  (a^2 - 1)/2
; If a is even, lim(b) =  (a/2)^2

(define (a-pythagorean-triple-from n)
  (let ((i (an-integer-starting-from n)))
    (let ((j (an-integer-between n (lim i))))
      (let ((k (sqrt (+ (* i i) (* j j)))))
        (require (integer? k))
        (list i j k)))))

(define (lim a)
  (if (odd? a)
    (/ (- (* a a) 1) 2)
    (- (/ (* a a) 4) 1)))

(a-pythagorean-triple-from 3)
; => (3 4 5)

try-again
; => (4 3 5) [same one]

try-again
; => (5 12 13)

;[0]: https://en.wikipedia.org/wiki/Pythagorean_triple
;[1]: https://en.wikipedia.org/wiki/Pythagorean_triple#The_Platonic_sequence
