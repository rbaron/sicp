; Order of evaluation

; Load this file
; $ cat 5.37.scm - | mit-scheme

; Let's compare the code generated with and without the preserving strategy
; for saving stack operations. The code used for testing is:

; Simple example.
(define code
  '(begin
    (define x 1)
    (+ x 1)))

(load "book-code/load-eceval-compiler.scm")
(load "book-code/ch5-compiler.scm")

; Helper procedures
(define (get-instructions compiled-code)
  (caddr compiled-code))

(define (simulate instructions)
  (let ((test-machine (make-machine all-regs
                                    eceval-operations
                                    instructions)))
    (set-register-contents! test-machine 'env (setup-environment))
    (start test-machine)
    test-machine))

(define (pprint-instrs instrs)
  (if (null? instrs)
    'done
    (begin (newline)
           (display (car instrs))
           (pprint-instrs (cdr instrs)))))


; Optimized version
(define opt-compiled-code (compile code 'val 'next))
(define opt-machine (simulate (get-instructions opt-compiled-code)))
((opt-machine 'stack) 'print-statistics)
; => (total-pushes = 0 maximum-depth = 0)

(pprint-instrs (get-instructions opt-compiled-code))
; =>    (assign val (const 1))
; =>    (perform (op define-variable!) (const x) (reg val) (reg env))
; =>    (assign val (const ok))
; =>    (assign proc (op lookup-variable-value) (const +) (reg env))
; =>    (assign val (const 1))
; =>    (assign argl (op list) (reg val))
; =>    (assign val (op lookup-variable-value) (const x) (reg env))
; =>    (assign argl (op cons) (reg val) (reg argl))
; =>    (test (op primitive-procedure?) (reg proc))
; =>    (branch (label primitive-branch3))
; =>    compiled-branch2
; =>    (assign continue (label after-call1))
; =>    (assign val (op compiled-procedure-entry) (reg proc))
; =>    (goto (reg val))
; =>    primitive-branch3
; =>    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
; =>    after-call1

; Let's modify preserving so it never optimizes; It will always save and restore
; every register in regs around seq1:
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
            (preserving (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2))))

(define alt-compiled-code (compile code 'val 'next))
(define alt-machine (simulate (get-instructions alt-compiled-code)))
((alt-machine 'stack) 'print-statistics)
; => (total-pushes = 15 maximum-depth = 5)

(pprint-instrs (get-instructions alt-compiled-code))
; =>    (save continue)
; =>    (save env)
; =>    (save continue)
; =>    (save env)
; =>    (save continue)
; =>    (assign val (const 1))
; =>    (restore continue)
; =>    (restore env)
; =>    (perform (op define-variable!) (const x) (reg val) (reg env))
; =>    (assign val (const ok))
; =>    (restore continue)
; =>    (restore env)
; =>    (restore continue)
; =>    (save continue)
; =>    (save env)
; =>    (save continue)
; =>    (assign proc (op lookup-variable-value) (const +) (reg env))
; =>    (restore continue)
; =>    (restore env)
; =>    (restore continue)
; =>    (save continue)
; =>    (save proc)
; =>    (save env)
; =>    (save continue)
; =>    (assign val (const 1))
; =>    (restore continue)
; =>    (assign argl (op list) (reg val))
; =>    (restore env)
; =>    (save argl)
; =>    (save continue)
; =>    (assign val (op lookup-variable-value) (const x) (reg env))
; =>    (restore continue)
; =>    (restore argl)
; =>    (assign argl (op cons) (reg val) (reg argl))
; =>    (restore proc)
; =>    (restore continue)
; =>    (test (op primitive-procedure?) (reg proc))
; =>    (branch (label primitive-branch6))
; =>    compiled-branch5
; =>    (assign continue (label after-call4))
; =>    (assign val (op compiled-procedure-entry) (reg proc))
; =>    (goto (reg val))
; =>    primitive-branch6
; =>    (save continue)
; =>    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
; =>    (restore continue)
; =>    after-call4

; As we can see, being smart with preserving saves us a lot of unneeded operations.
; In this case, no saves/restores were needed whatsoever!
