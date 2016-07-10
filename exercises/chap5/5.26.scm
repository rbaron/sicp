; Monitoring the stack

; Load this file and drop yourself in the REPL with:
; $ cat 5.26.scm - | mit-scheme

(load "book-code/load-eceval")

; Kickoff the evaluator
(define the-global-environment (setup-environment))
(start eceval)

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(factorial 1)
; => (total-pushes = 64 maximum-depth = 10)
; => 1

(factorial 2)
; => (total-pushes = 99 maximum-depth = 10)
; => 2

(factorial 3)
; => (total-pushes = 134 maximum-depth = 10)
; => 6

(factorial 4)
; => (total-pushes = 169 maximum-depth = 10)
; => 24

(factorial 5)
; => (total-pushes = 204 maximum-depth = 10)
; => 120

; a. The maximum depth stays constant at 10.

; b. The linear relationship has the form:

; pushes(n) = a*n + b

; Substituting n = 3 and n = 4 yields the following linear system:

; a*3 + b = 134
; a*4 + b = 169

; Which yields a = 35 and b = 29.
