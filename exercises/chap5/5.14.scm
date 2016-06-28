; Stacktistics

; Load this file and drop yourself in the REPL with:
; $ cat 5.14.scm - | mit-scheme

(load "book-code/ch5-regsim.scm")

(define fact-machine
  (make-machine
    '(continue n val)
    (list (list '= =) (list '- -) (list '* *))
    '(controller
       (assign continue (label fact-done))     ; set up final return address
     fact-loop
       (test (op =) (reg n) (const 1))
       (branch (label base-case))
       ;; Set up for the recursive call by saving n and continue.
       ;; Set up continue so that the computation will continue
       ;; at after-fact when the subroutine returns.
       (save continue)
       (save n)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-fact))
       (goto (label fact-loop))
     after-fact
       (restore n)
       (restore continue)
       (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
       (goto (reg continue))                   ; return to caller
     base-case
       (assign val (const 1))                  ; base case: 1! = 1
       (goto (reg continue))                   ; return to caller
     fact-done)))

(define (enumerate low hi)
  (if (> low hi)
    '()
    (cons low (enumerate (+ low 1) hi))))

((cadr (assoc 'initialize-stack (fact-machine 'operations))))

(let ((initialize-stack (cadr (assoc 'initialize-stack (fact-machine 'operations)))))
  (for-each
    (lambda (n)
      (set-register-contents! fact-machine 'n n)
      (initialize-stack)
      (start fact-machine)
      (newline)
      (display (list 'n '= n 'val '= (get-register-contents fact-machine 'val)))
      ((fact-machine 'stack) 'print-statistics)
      (newline)
      )
    (enumerate 1 10)))

; => (n = 1 val = 1)
; => (total-pushes = 0 maximum-depth = 0)
; =>
; => (n = 2 val = 2)
; => (total-pushes = 2 maximum-depth = 2)
; =>
; => (n = 3 val = 6)
; => (total-pushes = 4 maximum-depth = 4)
; =>
; => (n = 4 val = 24)
; => (total-pushes = 6 maximum-depth = 6)
; =>
; => (n = 5 val = 120)
; => (total-pushes = 8 maximum-depth = 8)
; =>
; => (n = 6 val = 720)
; => (total-pushes = 10 maximum-depth = 10)
; =>
; => (n = 7 val = 5040)
; => (total-pushes = 12 maximum-depth = 12)
; =>
; => (n = 8 val = 40320)
; => (total-pushes = 14 maximum-depth = 14)
; =>
; => (n = 9 val = 362880)
; => (total-pushes = 16 maximum-depth = 16)
; =>
; => (n = 10 val = 3628800)
; => (total-pushes = 18 maximum-depth = 18)

; As we can see, both the number of pushes and the total depth
; is a linear function of n. We can quickly see that the relationship is
; #(n) = 2*n - 2.
