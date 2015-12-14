; Investigating memoization gains in lazy evaluation

; a. Exhibit a program that you would expect to run much more slowly
; without memoization than with memoization.

; Let's try to come up with a scenario in which a thunk will be used
; more than once:

; (define (square x)
;   (* x x))
;
; (square (factorial 100))

; In this example, the argument `(factorial 100)` will be delayed until
; it has to be used in a primitive procedure - `*`. The memoization
; guarantees that the expensive calculation of factorial of 100 will
; be only done once.

; b. Let's evaluate the following interactions.

; b.1. With memoization:

; $ mit-scheme
; (load "book-code/ch4-mceval.scm")
; (load "book-code/ch4-leval.scm")
;
; (define the-global-environment (setup-environment))
; (driver-loop)
;
; (define count 0)
;
; (define (id x)
;   (set! count (+ count 1))
;   x)
;
; (define (square x)
;   (* x x))
; ;;; L-Eval input:
; (square (id 10))
; ;;; L-Eval value:
; 100
; ;;; L-Eval input:
; count
; ;;; L-Eval value:
; 1

; b.2 Without memoization
; (load "book-code/ch4-mceval.scm")
; (load "book-code/ch4-leval.scm")
;
; (define (force-it obj)
;   (if (thunk? obj)
;       (actual-value (thunk-exp obj) (thunk-env obj))
;       obj))
;
; (define the-global-environment (setup-environment))
; (driver-loop)
;
; (define count 0)
;
; (define (id x)
;   (set! count (+ count 1))
;   x)
;
; (define (square x)
;   (* x x))
;
; ;;; L-Eval input:
; (square (id 10))
; ;;; L-Eval value:
; 100
; ;;; L-Eval input:
; count
; ;;; L-Eval value:
; 2

; In the non-memoizing version, the argument `(id 10)` is evaluated twice.
