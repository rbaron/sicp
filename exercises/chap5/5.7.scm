; Automatically simulating the solutions to exercise 5.4

; Load this file and drop yourself in the REPL with:
; $ cat 5.7.scm - | mit-scheme

(load "book-code/ch5-regsim.scm")

; a. Recursive exponentiation

(define rec-expt-machine
  (make-machine
    '(b continue n val)
    (list (list '- -) (list '* *) (list '= =))
    '(controller
        (assign continue (label done))
      exp-loop
        (test (op =) (reg n) (const 0))
        (branch (label base-case))
        (save continue)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label ret))
        (goto (label exp-loop))
      ret
        (restore continue)
        (assign val (op *) (reg b) (reg val))
        (goto (reg continue))
      base-case
        (assign val (const 1))
        (goto (reg continue))
      done)))

; let's calc 2^5
(set-register-contents! rec-expt-machine 'n 5)
(set-register-contents! rec-expt-machine 'b 2)

(start rec-expt-machine)

(display "Results of calculating 2^5: ")
(display (get-register-contents rec-expt-machine 'val))
; => 32

; b. Iterative exponentiation

(define iter-expt-machine
  (make-machine
    '(b continue counter n product)
    (list (list '- -) (list '* *) (list '= =))
    '(controller
        (assign counter (reg n))
        (assign product (const 1))
        (assign continue (label done))

      expt-loop
        (test (op =) (reg counter) (const 0))
        (branch (label done))
        (assign counter (op -) (reg counter) (const 1))
        (assign product (op *) (reg b) (reg product))
        (goto (label expt-loop))
      done)))

;; let's calc 2^5
(set-register-contents! iter-expt-machine 'n 5)
(set-register-contents! iter-expt-machine 'b 2)
;
(start iter-expt-machine)

(display "Results of calculating 2^5: ")
(display (get-register-contents iter-expt-machine 'product))
