; If we write the iterative fibonacci function, we'd get something like:

; def fib(n):
;   a = 0
;   b = 1
;   for i in range(0,n):
;     a, b = b, (b+a)
;   return a

; In the loop, we have the following expression:
;
; a <- b
; b <- a+b
;
; On interesting statistic is the ratio between two consecutive terms on the sequence:
;
; b/a = phi = (a+b)/b
; phi = 1/phi + 1
; phi^2 -phi -1 = 0
;
; The roots are (1 +/- sqrt(5))/2. Following SICP's naming convention, we get that
;
; phi = (1 + sqrt(5))/2
; psi = (1 - sqrt(5))/2
;
; are both roots of the equation.
;
; In order to prove that Fib(n) = (phi^n - psi^n)/sqrt(5), we need to steps:
;
; 1. Base step
;
; Prove that it works for Fib(2) = Fib(0) + Fib(1)
;
;   1.1 Right hand side
;   Fib(0) = (1 - 1)/sqrt(5) = 0
;   Fib(1) = (phi-psi)/sqrt(5) = 1
;
;   1.2 Left handsize
;   Fib(2) = (phi^2-psi^2)/sqrt(5) = 1
;
; 2. Inductive step
;
;   Fib(n+1) = Fib(n-1) + Fib(n)
;   Fib(n+1) = (phi^(n-1)-psi^(n-1))/sqrt(5) + (phi^n-psi^n)/sqrt(5)
;   Fib(n+1) = (phi^(n+1)-psi^(n+1))/sqrt(5)
;
;
; That proves that Fib(n) = (phi^n - psi^n)/sqrt(5).
;
; How does that prove that Fib(n) is the closest integer to phi^n/sqrt(5)?
;
; For that to be true, we need to have psi^n/sqrt(5) < 1/2. That is the same as:
;
; psi^n < sqrt(5)/2
;
; since psi ~= -.6180 and sqrt(5)/2 ~= 1.1180, we have that, since 0 < psi < 1, psi^n, n>0,
; is less than psi, which is already less than sqrt(5)/2.
