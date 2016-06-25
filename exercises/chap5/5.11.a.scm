; Investigating alternative behaviors for push/pop operations

; a. (restore y) puts into y the last value saved on the stack,
; regardless of what register that value came from.

; As the exercise suggests, we can levarage this fact in order to remove
; one instruction from the Fibonacci machine. Let's take a look at this part:

;   (assign n (reg val))               ; n now contains Fib(n - 2)
;   (restore val)                      ; val now contains Fib(n - 1)
;   (restore continue)
;   (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
;           (op +) (reg val) (reg n))

; The point of this section of instructions is to calculate Fib(n-1) + Fib(n-2)
; and store it in the val register. At the beginning, the top of the stack
; contains Fib(n-1) and val holds the value of Fib(n-2). Another solution would be
; to restore the top of the stack (Fib(n-1))straight into the n register and let val keep
; holding the value of Fib(n-2). The resulting operation will still store the correct value
; on val.
