; Investigating alternative behaviors for push/pop operations

; Load this file and drop yourself in the REPL with:
; $ cat 5.11.c.scm - | mit-scheme

(load "book-code/ch5-regsim.scm")

; c. (restore y) puts into y the last value saved from y regardless of what other
; registers were saved after y and not restored

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack '())
        (the-instruction-sequence '()))
    (let ((the-ops '())
          (register-table
           (list (list 'pc pc) (list 'flag flag))))

        ; Added
        (define (initialize-stack)
          (display "\nWill intialize stacks")
          (set! stack
                (map (lambda (name-reg)
                  (cons (car name-reg) (make-stack)))
                 register-table)))

        ; Added
        (define (lookup-stack register-name)
          (cdr (assoc register-name stack)))

        (define (allocate-register name)
          (if (assoc name register-table)
              (error "Multiply defined register: " name)
              (set! register-table
                    (cons (list name (make-register name))
                          register-table)))
          'register-allocated)
        (define (lookup-register name)
          (let ((val (assoc name register-table)))
            (if val
                (cadr val)
                (error "Unknown register:" name))))
        (define (execute)
          (let ((insts (get-contents pc)))
            (if (null? insts)
                'done
                (begin
                  ((instruction-execution-proc (car insts)))
                  (execute)))))
        (define (dispatch message)
          (cond ((eq? message 'start)
                 (set-contents! pc the-instruction-sequence)
                 (execute))
                ((eq? message 'initialize-stack) initialize-stack)
                ((eq? message 'lookup-stack) lookup-stack)
                ((eq? message 'install-instruction-sequence)
                 (lambda (seq) (set! the-instruction-sequence seq)))
                ((eq? message 'allocate-register) allocate-register)
                ((eq? message 'get-register) lookup-register)
                ((eq? message 'install-operations)
                 (lambda (ops) (set! the-ops (append the-ops ops))))
                ((eq? message 'stack) stack)
                ((eq? message 'operations) the-ops)
                (else (error "Unknown request -- MACHINE" message))))
        dispatch)))

(define (make-save inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (lambda ()
        (push ((machine 'lookup-stack) reg-name) (get-contents reg))
        (advance-pc pc)))))

(define (make-restore inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
    (lambda ()
      (set-contents! reg (pop ((machine 'lookup-stack) reg-name)))
      (advance-pc pc)))))


(define c-test-machine
  (make-machine
    '(a b)
    '()
    '(controller
      (assign a (const 1))
      (assign b (const 2))
      (save a)
      (save b)
      (restore a)
      (restore b))))

((c-test-machine 'initialize-stack))
(start c-test-machine)

; Final value of a:
(display (get-register-contents c-test-machine 'a))
; => 1

; Final value of b:
(display (get-register-contents c-test-machine 'b))
; => 2

; NB: I choose the path of handling multiple stacks inside the machine itself.
; Another way would be making the stack itself handle multiple stacks.
