; Breaking points

; Load this file and drop yourself in the REPL with:
; $ cat 5.19.scm - | mit-scheme

(load "book-code/ch5-regsim.scm")

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)
        (lookup-label-for-instruction (car inst) labels)))
     insts)))

(define (set-instruction-execution-proc! inst proc label)
  (set-cdr! inst (cons proc label)))

(define (instruction-execution-proc inst)
  (cadr inst))

(define (instruction-label inst)
  (cddr inst))

(define (lookup-label-for-instruction inst labels)
  (cond ((null? labels) '*no-label*)
        ((and (not (null? (cdar labels))) (eq? (caadar labels) inst)) (caar labels))
        (else (lookup-label-for-instruction inst (cdr labels)))))

(define (make-breakpoint label counter)
  (list label counter counter))

(define (decrement-countdown! bp)
  (list-set! bp 2 (- (breakpoint-countdown bp) 1)))

(define (breakpoint-countdown bp)
  (list-ref bp 2))

(define (breakpoint-label bp)
  (car bp))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        ; Added
        ; Association list with (label counter countdown)
        (breakpoints '())
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

      (define (update-breakpoints!)
        (for-each decrement-countdown! breakpoints))

      (define (clear-breakpoint! bp)
        (set! breakpoints (del-assoc! (breakpoint-label bp) breakpoints)))

      (define (clear-all-breakpoints!)
        (set! breakpoints '()))

      (define (get-0-countdown-breakpoint)
        (define (inner bps)
          (cond ((null? bps) #f)
                ((= (breakpoint-countdown (car bps)) 0) (car bps))
                (else (inner (cdr bps)))))
        (inner breakpoints))

      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (let* ((inst (car insts))
                     (label (instruction-label inst)))

                (update-breakpoints!)

                (let ((bp (get-0-countdown-breakpoint)))
                  (if bp
                    (begin
                      (display "\nReached breakpoint @ ")
                      (display (car inst))
                      (clear-breakpoint! bp))
                    (begin
                      ((instruction-execution-proc inst))
                      (execute))))))))

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
              ((eq? message 'set-breakpoint)
               (lambda (label counter)
                (set! breakpoints (cons (make-breakpoint label counter) breakpoints))))

              ; Added
              ((eq? message 'proceed) execute)


              ((eq? message 'inst-counter) inst-counter)
              ((eq? message 'reset-inst-counter)
               (lambda () (set! inst-counter 0)))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (set-breakpoint machine label counter)
  ((machine 'set-breakpoint) label counter))

(define (proceed-machine machine)
  ((machine 'proceed)))

(define
  fact-machine
  (make-machine
    '(continue n val)
    (list (list '= =) (list '- -) (list '* *))
    '(controller
       (assign continue (label fact-done))
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

(set-breakpoint fact-machine 'controller 2)

(start fact-machine)

; Will break.
(get-register-contents fact-machine 'val)
; => *unassigned*

(get-register-contents fact-machine 'n)
; => 3

(proceed-machine fact-machine)

(get-register-contents fact-machine 'val)
; => 6

(get-register-contents fact-machine 'n)
; => 3
