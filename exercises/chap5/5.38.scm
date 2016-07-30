; Open code generation for primitive procedures

; Load this file
; $ cat 5.37.scm - | mit-scheme

(load "book-code/load-eceval-compiler.scm")
(load "book-code/ch5-compiler.scm")

; Helper procedures
(define (get-instructions compiled-code)
  (caddr compiled-code))

(define all-regs '(arg1 arg2 env proc val argl continue))

(define (simulate instructions)
  (let ((test-machine (make-machine all-regs
                                    (cons (list '+ +) eceval-operations)
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

; a.

(define (spread-arguments argument-codes)
  (let ((arg1-comp (compile (car argument-codes) 'arg1 'next))
        (arg2-comp (compile (cadr argument-codes) 'arg2 'next)))
    (preserving '(env arg1 arg2)
                arg1-comp
                arg2-comp)))

(pprint-instrs
  (get-instructions
    (spread-arguments '(1 (+ 1 2)))))
; => (assign arg1 (const 1))
; => (assign proc (op lookup-variable-value) (const +) (reg env))
; => (assign val (const 2))
; => (assign argl (op list) (reg val))
; => (assign val (const 1))
; => (assign argl (op cons) (reg val) (reg argl))
; => (test (op primitive-procedure?) (reg proc))
; => (branch (label primitive-branch3))
; => compiled-branch2
; => (assign continue (label proc-return4))
; => (assign val (op compiled-procedure-entry) (reg proc))
; => (goto (reg val))
; => proc-return4
; => (assign arg2 (reg val))
; => (goto (label after-call1))
; => primitive-branch3
; => (assign arg2 (op apply-primitive-procedure) (reg proc) (reg argl))
; => after-call1

; b.

(define (open-code-generator exp target linkage)
  (let* ((operand-code (car exp))
         (arg-codes (cdr exp))
         (comp-arg-codes (spread-arguments arg-codes)))
    (append-instruction-sequences
     comp-arg-codes
     (make-instruction-sequence '(arg1 arg2)
                                `(,target)
                                `((assign ,target (op ,operand-code) (reg arg1) (reg arg2)))))))

(pprint-instrs
  (get-instructions
    (open-code-generator '(+ 1 2)
                         'val
                         'next)))
; => (assign arg1 (const 1))
; => (assign arg2 (const 2))
; => (assign val (op +) (reg arg1) (reg arg2))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))

        ; Added
        ((open-codeable? exp)
         (open-code-generator exp target linkage))

        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (open-codeable? exp)
  (and (pair? exp)
       (memq (car exp) '(+ - / *))))

; c.

(define result (compile
  '(define (factorial n)
     (define (iter product counter)
       (if (> counter n)
           product
           (iter (* counter product)
                 (+ counter 1))))
     (iter 1 1))
  'val
  'next))

(pprint-instrs (get-instructions result))

;    (assign val (op make-compiled-procedure) (label entry6) (reg env))
;    (goto (label after-lambda5))
;    entry6
;    (assign env (op compiled-procedure-env) (reg proc))
;    (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;    (assign val (op make-compiled-procedure) (label entry11) (reg env))
;    (goto (label after-lambda10))
;    entry11
;    (assign env (op compiled-procedure-env) (reg proc))
;    (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
;    (save continue)
;    (save env)
;    (assign proc (op lookup-variable-value) (const >) (reg env))
;    (assign val (op lookup-variable-value) (const n) (reg env))
;    (assign argl (op list) (reg val))
;    (assign val (op lookup-variable-value) (const counter) (reg env))
;    (assign argl (op cons) (reg val) (reg argl))
;    (test (op primitive-procedure?) (reg proc))
;    (branch (label primitive-branch20))
;    compiled-branch19
;    (assign continue (label after-call18))
;    (assign val (op compiled-procedure-entry) (reg proc))
;    (goto (reg val))
;    primitive-branch20
;    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;    after-call18
;    (restore env)
;    (restore continue)
;    (test (op false?) (reg val))
;    (branch (label false-branch13))
;    true-branch14
;    (assign val (op lookup-variable-value) (const product) (reg env))
;    (goto (reg continue))
;    false-branch13
;    (assign proc (op lookup-variable-value) (const iter) (reg env))

     ; Here's the difference. While calculating (+ counter 1), the compiler now
     ; optimizes so that, instead of building the argument list in argl and applying
     ; a procedure, it generates code that put those arguments into arg1 and arg2 and
     ; call a machine primitive right away.
;    (assign arg1 (op lookup-variable-value) (const counter) (reg env))
;    (assign arg2 (const 1))
;    (assign val (op +) (reg arg1) (reg arg2))
;    (assign argl (op list) (reg val))

     ; Same goes for the call (* counter product)
;    (assign arg1 (op lookup-variable-value) (const counter) (reg env))
;    (assign arg2 (op lookup-variable-value) (const product) (reg env))
;    (assign val (op *) (reg arg1) (reg arg2))
;    (assign argl (op cons) (reg val) (reg argl))
;    (test (op primitive-procedure?) (reg proc))
;    (branch (label primitive-branch17))
;    compiled-branch16
;    (assign val (op compiled-procedure-entry) (reg proc))
;    (goto (reg val))
;    primitive-branch17
;    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;    (goto (reg continue))
;    after-call15
;    after-if12
;    after-lambda10
;    (perform (op define-variable!) (const iter) (reg val) (reg env))
;    (assign val (const ok))
;    (assign proc (op lookup-variable-value) (const iter) (reg env))
;    (assign val (const 1))
;    (assign argl (op list) (reg val))
;    (assign val (const 1))
;    (assign argl (op cons) (reg val) (reg argl))
;    (test (op primitive-procedure?) (reg proc))
;    (branch (label primitive-branch9))
;    compiled-branch8
;    (assign val (op compiled-procedure-entry) (reg proc))
;    (goto (reg val))
;    primitive-branch9
;    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;    (goto (reg continue))
;    after-call7
;    after-lambda5
;    (perform (op define-variable!) (const factorial) (reg val) (reg env))
;    (assign val (const ok))

; d. Allowing more than 2 arguments with open-code generation.

; The idea I persued here was to implement some sort of reduce operation. I
; had in mind the following transformation:

; (+ 1 2 3) => (+ (+ 1 2) 3)

; I used the register arg1 as the accumulator and compiled the arguments successively
; into arg2. The

(define (open-code-generator exp target linkage)
  (let* ((operand-code (car exp))
         (arg-codes (cdr exp)))

    (define (reduce remaining-arg-codes)
      (if (null? remaining-arg-codes)
        (empty-instruction-sequence)
        (let ((comp-first-arg (compile (car remaining-arg-codes) 'arg2 'next)))
          (append-instruction-sequences
            comp-first-arg
            (append-instruction-sequences
              (make-instruction-sequence '(arg1 arg2)
                                         '(arg1)
                                         `((assign arg1 (op ,operand-code) (reg arg1) (reg arg2))))
              (reduce (cdr remaining-arg-codes)))))))

     ; Initialize the register arg1 to the first compiled element from arg-codes
     (let ((comp-first-arg (compile (car arg-codes) 'arg1 'next)))
       (append-instruction-sequences
         comp-first-arg
         (append-instruction-sequences
           (reduce (cdr arg-codes))
           (make-instruction-sequence '(arg1)
                                      `(,target)
                                      `((assign ,target (reg arg1)))))))))

(define result (compile
  '(+ 1 2 3)
  'val
  'next))

(define machine (simulate (get-instructions result)))
(get-register-contents machine 'val)
; => 6

(pprint-instrs (get-instructions result))
; => (assign arg1 (const 1))
; => (assign arg2 (const 2))
; => (assign arg1 (op +) (reg arg1) (reg arg2))
; => (assign arg2 (const 3))
; => (assign arg1 (op +) (reg arg1) (reg arg2))
; => (assign val (reg arg1))
