; Dinamycally allocating registers

; Load this file and drop yourself in the REPL with:
; $ cat 5.13.scm - | mit-scheme

(load "book-code/ch5-regsim.scm")

; Removed the registers argument
(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

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

      ; Added
      (define (get-or-allocate-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (let ((new-reg (make-register name)))
                (set! register-table (cons (list name new-reg) register-table))
                new-reg))))
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
              ; Added
              ((eq? message 'get-or-allocate-register) get-or-allocate-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

; Overriding get-register so it gets or creates it
(define (get-register machine reg-name)
  ((machine 'get-or-allocate-register) reg-name))

(define test-machine
  (make-machine
    (list (list '+ +))
    '(controller
      main
        (assign a (const 1))
        (assign b (const 2))
        (save b)
        (restore continue)
      done)))

(start test-machine)

(display (get-register-contents test-machine 'a))
; => 1

(display (get-register-contents test-machine 'continue))
; => 2
