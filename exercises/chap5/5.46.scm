; Measuring the performance of the compiled factorial procedure

; Load this file and drop yourself in the REPL with
; $ cat 5.46.scm - | mit-scheme

(load "book-code/load-eceval-compiler.scm")
(load "book-code/ch5-compiler.scm")

; Let's compile and measure the performance of the fib procedure
; for a few values of n:

(compile-and-go
  '(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))

(display "\nFor n = 2:\n")
(fib 2)
; => (total-pushes = 17 maximum-depth = 5)

(display "\nFor n = 3:\n")
(fib 3)
; => (total-pushes = 27 maximum-depth = 8)

(display "\nFor n = 4:\n")
(fib 4)
; => (total-pushes = 47 maximum-depth = 11)

; Even though the procedure has a non-linear running time,
; the maximum-depth (of space complexity) _can_ be described as
; a linear function of n. Let's find out what it is:

; maximum-depth(n) = a + b*n
; For n = 3 and n = 4, we have:

; a + 3b = 8
; a + 4b = 11

; => maximum-depth(n) = 3*n - 1

; Finding the number of pushes as a function of n, we can use the following
; relationship:

; n_pushes(n) = S(n) = S(n-1) + S(n-2) + k

; Substituting n = 2, 3, 4, we get:

; 47 = 27 + 17 + k
; => k = 3

; We can also write the number of pushes S in terms of Fib(n-1):
; S(n) = a*Fib(n+1) + b

; For n = 2 and n = 3, we have:
; a*2 + b = 17
; a*3 + b = 27

; => S(n) = 10*Fib(n+1) - 3

; We can now build the following table:

;                                  |  maximum-depth  |                 total-pushes (S)
;-----------------------------------------------------------------------------------------------------
; interpreted-fib (exercise 5.29)  |      5n + 3     |  S(n) = S(n-1) + S(n-2) + 40 = 56*Fib(n+1) - 40
; compiled-fib                     |      3n - 1     |  S(n) = S(n-1) + S(n-2) + 3  = 10*Fib(n+1) - 3
