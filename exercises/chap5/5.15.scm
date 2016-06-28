; Analyzing the number of instructions

; Load this file and drop yourself in the REPL with:
; $ cat 5.15.scm - | mit-scheme

(load "book-code/ch5-regsim.scm")

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        ; Added
        (inst-counter 0)
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
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
                ; Added
                ((instruction-execution-proc (car insts)))
                (set! inst-counter (+ inst-counter 1))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ; Added
              ((eq? message 'inst-counter) inst-counter)
              ((eq? message 'reset-inst-counter)
               (lambda () (set! inst-counter 0)))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define fact-machine
  (make-machine
    '(continue n val)
    (list (list '= =) (list '- -) (list '* *))
    '(controller
       (assign continue (label fact-done))     ; set up final return address
     fact-loop
       (test (op =) (reg n) (const 1))
       (branch (label base-case))
       (save continue)
       (save n)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-fact))
       (goto (label fact-loop))
     after-fact
       (restore n)
       (restore continue)
       (assign val (op *) (reg n) (reg val))
       (goto (reg continue))
     base-case
       (assign val (const 1))
       (goto (reg continue))
     fact-done)))

(set-register-contents! fact-machine 'n 10)
(start fact-machine)
(display (fact-machine 'inst-counter))
; => 104

((fact-machine 'reset-inst-counter))
(start fact-machine)
(display (fact-machine 'inst-counter))
; => 104
