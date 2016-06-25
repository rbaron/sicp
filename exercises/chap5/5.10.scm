; New syntax for register-machine instructions

; Load this file and drop yourself in the REPL with:
; $ cat 5.10.scm - | mit-scheme

(load "book-code/ch5-regsim.scm")

; Let's consider a new syntax for assignment instructions that look like these:

; (a <- (const 1))

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (cadr inst) '<-)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

(define (assign-reg-name assign-instruction)
  (car assign-instruction))

(define test-machine
  (make-machine
    '(a)
    (list (list '- -))
    '(controller
      here
        (a <- (op -) (const 1) (const 2))
      )))

(start test-machine)
(display (get-register-contents test-machine 'a))
; => -1
