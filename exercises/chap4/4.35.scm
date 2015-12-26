; Nondeterministic search for Pythagorean triplets

; Load this file and drop yourself in the REPL with:
; $ cat 4.35.scm - | mit-scheme

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

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

; Solution - implementation of `an-integer-between`
; Exercise doesn't mention whether the interval should include
; the bounds. I'm gonna let it do.
(define (an-integer-between low high)
  (if (> low high)
    ; (amb) with no choices represent a failed computation
    (amb)
    (amb low (an-integer-between (+ low 1) high))))

; Testing
(a-pythagorean-triple-between 1 5)
; => (3 4 5)

try-again
; => ;;; There are no more values of (a-pythagorean-triple-between 1 5)

(a-pythagorean-triple-between 10 40)
; => (10 24 26)

try-again
; => (12 16 20)

try-again
; => (12 35 37)

try-again
; => (15 20 25)

try-again
; => (15 36 39)

try-again
; => (16 30 34)

