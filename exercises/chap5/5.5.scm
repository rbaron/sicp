; Hand simulating the factorial and the Fibonacci machine

; a. Factorial machine (n = 3)

; 1. Entry point
; stack []
; n 3
; continue fact-done

; 2. fact-loop
; stack [fact-done, 3]
; n 2
; continue after-fact

; 3. fact-loop
; stack [fact-done, 3, after-fact, 2]
; n 1
; continue after-fact

; 4. fact-loop

; 5. base-case
; val 1

; 6. after-fact
; stack [fact-done, 3]
; n 2
; continue after-fact
; val 2

; 7. after-fact
; stack []
; n 3
; continue fact-done
; val 6

; 8. done (result in val register)

; b. Fibonacci machine (n = 3)

; 1. Entry point
; n 3
; continue fib-done
; stack []

; 2. fib-loop
; n 2
; continue after-fib-n-1
; stack [fib-done, 3]

; 3. fib-loop
; n 1
; continue after-fib-n-1
; stack [fib-done, 3, after-fib-n-1, 2]

; 4. fib-loop

; 5. immediate-answer
; val 1

; 6. after-fib-n-1
; n 0
; continue after-fib-n-2
; stack [fib-done, 3, after-fib-n-1, 1]

; 7. fib-loop

; 8. immediate-answer
; val 0

; 9. after-fib-n-2
; n 0
; val 1
; continue after-fib-n-1
; stack [fib-done, 3]

; 10. after-fib-n-1
; n 1
; continue after-fib-n-2
; stack [fib-done 1]

; 11. fib-loop

; 12. immediate-answer
; val 1

; 13. after-fib-n-2
; n 1
; val 2
; continue fib-done
; stack []

; 14. done (result in val register)
