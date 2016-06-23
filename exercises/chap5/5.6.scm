; Optimizing the Fibonacci machine

; We can see that there are indeed two instructions
; that could be erased with no compromise in functionality
; from the Fibonacci machine.

; Let's take a look at the label after-fib-n-1:

 afterfib-n-1
   (restore n)
   (restore continue) ; **
   (assign n (op -) (reg n) (const 2))
   (save continue) ; **
   (assign continue (label afterfib-n-2))
   (save val)
   (goto (label fib-loop))

; We can see that we restore and re-save the continue
; register, without ever modifying it. We could safely
; remove both instructions and the functionality would
; remain the exact same.
