; Compiling information from a machine

; Load this file and drop yourself in the REPL with:
; $ cat 5.12.scm - | mit-scheme

(load "book-code/ch5-regsim.scm")

(define (make-set)
  '())

(define (add-set set value)
  (if (member value set)
    set
    (cons value set)))

(define (sort-list-of-symbols insts)
  (define (compare-names inst1 inst2)
    (symbol<? (car inst1) (car inst2)))
  (sort insts compare-names))

(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (let ((info (compile-info insts)))
        (update-insts! insts labels machine)
        (cons insts info)))))

(define (compile-info insts)
  (let ((insts (map car insts)))
    (list
      (fold-left add-set '() insts)
      (fold-left add-set
                 (make-set)
                 (goto-regs insts))
      (fold-left add-set
                 (make-set)
                 (saved-restored-regs insts))
      (make-sources-by-reg insts)
    )))

; Filter instructions for gotos and collect registers that are used
(define (goto-regs insts)
  (map cadadr
       (filter
          (lambda (i)
            (and (tagged-list? i 'goto)
                 (tagged-list? (cadr i) 'reg)))
         insts)))

(define (saved-restored-regs insts)
  (map cadr
       (filter
          (lambda (i)
            (or (tagged-list? i 'save)
                (tagged-list? i 'restore)))
         insts)))

(define (make-sources-by-reg insts)
  (fold-left add-assignment
             '()
             (filter (lambda (i) (tagged-list? i 'assign)) insts)))

(define (add-assignment sources-by-reg assign)
  (let ((reg-name (assign-reg-name assign))
        (source (assign-value-exp assign)))
    (let ((current-assoc (assoc reg-name sources-by-reg)))
      (if current-assoc
        (cons (cons reg-name (add-set (cdr current-assoc) source))
              (del-assoc reg-name sources-by-reg))
        (cons (cons reg-name (list source))
              sources-by-reg)))))

(define (print-info info)
  (let ((insts (car info))
        (entry-point-registers (cadr info))
        (saved-restored (caddr info))
        (sources-by-register (cadddr info)))
    (display "\nINFO\n====\n")
    (display "\nUnique assignments:")
    (for-each (lambda (a) (display "\n\t")(display a)) (sort-list-of-symbols insts))
    (display "\nRegisters used for entry points:")
    (for-each (lambda (a) (display "\n\t")(display a)) entry-point-registers)
    (display "\nRegisters used for saving and restoring:")
    (for-each (lambda (a) (display "\n\t")(display a)) saved-restored)
    (display "\nSources by register:")
    (for-each
      (lambda (sources-by-1-register)
        (let ((reg (car sources-by-1-register))
              (sources (cdr sources-by-1-register)))
          (display "\n\t")(display reg)
          (for-each (lambda (source) (display "\n\t\t")(display source)) sources)))
      sources-by-register)))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        ; Added
        (info '())
        (the-instruction-sequence '()))
    (let ((the-ops '())
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
               ; Changed
               (lambda (seq-info)
                 (set! the-instruction-sequence (car seq-info))
                 (set! info (cdr seq-info))))
              ; Added
              ((eq? message 'get-info) info)
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define test-machine
  (make-machine
    '(a b continue)
    (list (list '+ +))
    '(controller
      start
        (assign continue (label done))
      main
        (assign a (const 1))
        (assign a (const 1))
        (goto (label done))
        (assign b (const 2))
        (goto (reg continue))
        (assign a (op +) (reg a) (reg b))
        (assign a (reg b))
        (save a)
        (restore a)
      done)))

(start test-machine)
; Final value of a:
(display (get-register-contents test-machine 'a))
; => 1

(print-info (test-machine 'get-info))

; => INFO
; => ====
; =>
; => Unique assignments:
; =>   (assign a (reg b))
; =>   (assign a (op +) (reg a) (reg b))
; =>   (assign b (const 2))
; =>   (assign a (const 1))
; =>   (assign continue (label done))
; =>   (goto (reg continue))
; =>   (goto (label done))
; =>   (restore a)
; =>   (save a)
; => Registers used for entry points:
; =>   continue
; => Registers used for saving and restoring:
; =>   a
; => Sources by register:
; =>   a
; =>     ((reg b))
; =>     ((op +) (reg a) (reg b))
; =>     ((const 1))
; =>   b
; =>     ((const 2))
; =>   continue
; =>     ((label done))


