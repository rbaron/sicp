; Tracing registers

; Load this file and drop yourself in the REPL with:
; $ cat 5.18.scm - | mit-scheme

(load "book-code/ch5-regsim.scm")

(define (make-register name)
  (let ((contents '*unassigned*)
        (tracing? #f))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (if tracing?
                 (begin (display (list "Register " name ": " contents " -> " value))
                        (newline)))
               (set! contents value)))
            ((eq? message 'set-tracing)
             (lambda (should-trace?)
               (set! tracing? should-trace?)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
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
                ((instruction-execution-proc (car insts)))
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
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ; Added
              ((eq? message 'set-register-tracing)
                (lambda (reg-name should-trace?)
                  (((lookup-register reg-name) 'set-tracing) should-trace?)))
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

(set-register-contents! fact-machine 'n 3)

((fact-machine 'set-register-tracing) 'n #t)
((fact-machine 'set-register-tracing) 'val #t)

(start fact-machine)

; => (Register  n :  3  ->  2)
; => (Register  n :  2  ->  1)
; => (Register  val :  *unassigned*  ->  1)
; => (Register  n :  1  ->  2)
; => (Register  val :  1  ->  2)
; => (Register  n :  2  ->  3)
; => (Register  val :  2  ->  6)
