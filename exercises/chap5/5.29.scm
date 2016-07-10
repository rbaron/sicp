; Monitoring stack operations in the tree-recursive Fibonacci procedure

; Load this file and drop yourself in the REPL with:
; $ cat 5.29.scm - | mit-scheme

(load "book-code/load-eceval")

(define the-global-environment (setup-environment))
(start eceval)

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(display "\nFor 2:\n")
(fib 2)
; (total-pushes = 72 maximum-depth = 13)

(display "\nFor 3:\n")
(fib 3)
; (total-pushes = 128 maximum-depth = 18)

(display "\nFor 4:\n")
(fib 4)
; (total-pushes = 240 maximum-depth = 23)

; a. Give a formula in terms of n for the maximum depth of the stack
; required to compute Fib(n) for n >= 2.

; Assuming the linear dependency max-depth(n) = a*n + b, we can substitute
; n = 2 and n = 3 and find the values of a and b:

; a*3 + b = 18
; a*4 + b = 23
; => max-depth(n) = 5n + 3

; b. Give a formula for the total number of pushes used to
; compute Fib(n) for n >= 2.

; Let S(n) be the number of pushes Fib(n) does. We have:

; S(n) = S(n-1) + S(n-2) + k

; where k is a constant that represents the number of pushes done in
; a single pass over fib.

; We can find k by replacing the number of pushes we found in our simulation:

; S(4) = S(3) + S(2) + k
; 240 = 128 + 72 + k
; => k = 40

; To find a relationship in the form of S(n) = a*Fib(n+1) + b, we can also
; substitute values of n = 2 and n = 3:

; a*2 + b = 72
; a*3 + b = 128
; => S(n) = 56*Fib(n+1) - 40
