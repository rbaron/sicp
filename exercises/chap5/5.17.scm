; Printing instructions and labels before execution

; Load this file and drop yourself in the REPL with:
; $ cat 5.17.scm - | mit-scheme

(load "book-code/ch5-regsim.scm")

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    ;(display (list "\nOn update-insts!: " labels "\n"))
    (for-each
     (lambda (inst)
       ;(display "\nOn assemble of inst ")(display inst)(display ": ")(display stack)
       ;(display (list "\nFound label: " (lookup-label-for-instruction (car inst) labels) "\n"))
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)
        ; Added
        (lookup-label-for-instruction (car inst) labels)))
     insts)))

(define (set-instruction-execution-proc! inst proc label)
  (set-cdr! inst (cons proc label)))

(define (instruction-execution-proc inst)
  (cadr inst))

(define (instruction-label inst)
  (cddr inst))

(define (lookup-label-for-instruction inst labels)
  ;(display (list "\nLooking up " inst " on " labels "\n"))
  (cond ((null? labels) '*no-label*)
        ((and (not (null? (cdar labels))) (eq? (caadar labels) inst)) (caar labels))
        (else (lookup-label-for-instruction inst (cdr labels)))))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        ; Added
        (trace? #t)
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
                (if trace?
                  (begin (newline)
                         (if (not (eq? '*no-label* (instruction-label (car insts))))
                           (begin (display (instruction-label (car insts)))
                                  (display ":")
                                  (newline)))
                         (display (instruction-text (car insts)))))

                (set! inst-counter (+ inst-counter 1))
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
              ; Added
              ((eq? message 'set-trace)
                (lambda (value) (set! trace? value)))
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

(set-register-contents! fact-machine 'n 3)
(start fact-machine)

; => controller:
; => (assign continue (label fact-done))
; => fact-loop:
; => (test (op =) (reg n) (const 1))
; => (branch (label base-case))
; => (save continue)
; => (save n)
; => (assign n (op -) (reg n) (const 1))
; => (assign continue (label after-fact))
; => (goto (label fact-loop))
; => fact-loop:
; => (test (op =) (reg n) (const 1))
; => (branch (label base-case))
; => (save continue)
; => (save n)
; => (assign n (op -) (reg n) (const 1))
; => (assign continue (label after-fact))
; => (goto (label fact-loop))
; => fact-loop:
; => (test (op =) (reg n) (const 1))
; => (branch (label base-case))
; => base-case:
; => (assign val (const 1))
; => (goto (reg continue))
; => after-fact:
; => (restore n)
; => (restore continue)
; => (assign val (op *) (reg n) (reg val))
; => (goto (reg continue))
; => after-fact:
; => (restore n)
; => (restore continue)
; => (assign val (op *) (reg n) (reg val))
; => (goto (reg continue))

