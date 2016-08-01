; Measuring the performance of the compiled factorial procedure

; Load this file and drop yourself in the REPL with
; $ cat 5.45.scm - | mit-scheme

(load "book-code/load-eceval-compiler.scm")
(load "book-code/ch5-compiler.scm")

; This will help up identify possible optimizations for item b.

(define (get-instructions compiled-code)
  (caddr compiled-code))

(define (pprint-instrs instrs)
  (if (null? instrs)
    'done
    (begin (newline)
           (display (car instrs))
           (pprint-instrs (cdr instrs)))))

(define code
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))

; Uncomment for pretty-printing the compiled code for debugging.
; The output is commented at the end of this file
;(pprint-instrs (get-instructions (compile code 'val 'next)))

(compile-and-go
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))

; a. Let's find the linear functions of n for n_pushes(n) and maximum-depth(n):

(display "\nFor n = 1:\n")
(factorial 1)
; => (total-pushes = 7 maximum-depth = 3)

(display "\nFor n = 2:\n")
(factorial 2)
; => (total-pushes = 13 maximum-depth = 5)

(display "\nFor n = 3:\n")
(factorial 3)
; => (total-pushes = 19 maximum-depth = 8)

; Let's define the linear relationship as:

; n_pushes(n) = a + b*n
; for n = 1 and n = 2, we have:

; a +  b = 7
; a + 2b = 13

; => n_pushes(n) = 1 + 6*n

; Conversely, for maximum_depth:

; maximum-depth(n) = a + b*n
; for n = 1 and n = 2, we have:

; a +  b = 3
; a + 2b = 5

; => maximum-depth(n) = 1 + b*n

; Now we can build the following table:

;                                        |  maximum-depth  | total-pushes
; ------------------------------------------------------------------------
; interpreted-factorial (ex. 5.27)       |      5n + 3     |    32n - 16
; special-purpose-factorial (ex. 5.14)   |      2n - 2     |     2n -  2
; compiled-factorial                     |      2n + 1     |     6n +  1

; As n grows larger, the constant terms become more irrelevant and the ratio for
; maximum-depth and total-pushes become:

; 1. interpreted-factorial to compiled-factorial:
;    - maximum-depth: 5/2
;    - total-pushes: 32/6 = 16/3

; 1. compiled-factorial to special-purpose-factorial:
;    - maximum-depth: 1
;    - total-pushes: 6/2 = 3

; b. Suggested improvements to the compiler:

; As we can see, the hand-tailored special-purpose-factorial machine does a better
; job than the compiled factorial version. Let's compare both codes:



; Compiled code:

;    (assign val (op make-compiled-procedure) (label entry2) (reg env))
;    (goto (label after-lambda1))

    ; Start of factorial code
;    entry2
;    (assign env (op compiled-procedure-env) (reg proc))
;    (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;    (save continue)
;    (save env)
;    (assign proc (op lookup-variable-value) (const =) (reg env))
;    (assign val (const 1))
;    (assign argl (op list) (reg val))
;    (assign val (op lookup-variable-value) (const n) (reg env))
;    (assign argl (op cons) (reg val) (reg argl))
;    (test (op primitive-procedure?) (reg proc))
;    (branch (label primitive-branch17))
;    compiled-branch16
;    (assign continue (label after-call15))
;    (assign val (op compiled-procedure-entry) (reg proc))
;    (goto (reg val))
;    primitive-branch17
;    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;    after-call15
;    (restore env)
;    (restore continue)
;    (test (op false?) (reg val))
;    (branch (label false-branch4))
;    true-branch5
;    (assign val (const 1))
;    (goto (reg continue))
;    false-branch4
;    (assign proc (op lookup-variable-value) (const *) (reg env))
;    (save continue)
;    (save proc)
;    (assign val (op lookup-variable-value) (const n) (reg env))
;    (assign argl (op list) (reg val))
;    (save argl)
;    (assign proc (op lookup-variable-value) (const factorial) (reg env))
;    (save proc)
;    (assign proc (op lookup-variable-value) (const -) (reg env))
;    (assign val (const 1))
;    (assign argl (op list) (reg val))
;    (assign val (op lookup-variable-value) (const n) (reg env))
;    (assign argl (op cons) (reg val) (reg argl))
;    (test (op primitive-procedure?) (reg proc))
;    (branch (label primitive-branch8))
;    compiled-branch7
;    (assign continue (label after-call6))
;    (assign val (op compiled-procedure-entry) (reg proc))
;    (goto (reg val))
;    primitive-branch8
;    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;    after-call6
;    (assign argl (op list) (reg val))
;    (restore proc)
;    (test (op primitive-procedure?) (reg proc))
;    (branch (label primitive-branch11))
;    compiled-branch10
;    (assign continue (label after-call9))
;    (assign val (op compiled-procedure-entry) (reg proc))
;    (goto (reg val))
;    primitive-branch11
;    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;    after-call9
;    (restore argl)
;    (assign argl (op cons) (reg val) (reg argl))
;    (restore proc)
;    (restore continue)
;    (test (op primitive-procedure?) (reg proc))
;    (branch (label primitive-branch14))
;    compiled-branch13
;    (assign val (op compiled-procedure-entry) (reg proc))
;    (goto (reg val))
;    primitive-branch14
;    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;    (goto (reg continue))
;    after-call12
;    after-if3
;    after-lambda1
;    (perform (op define-variable!) (const factorial) (reg val) (reg env))
;    (assign val (const ok))

; Special, hand-crafted factorial machine (fig 5.11):

;    (controller
;       (assign continue (label fact-done))
;     fact-loop
;       (test (op =) (reg n) (const 1))
;       (branch (label base-case))
;       (save continue)
;       (save n)
;       (assign n (op -) (reg n) (const 1))
;       (assign continue (label after-fact))
;       (goto (label fact-loop))
;     after-fact
;       (restore n)
;       (restore continue)
;       (assign val (op *) (reg n) (reg val))
;       (goto (reg continue))
;     base-case
;       (assign val (const 1))
;       (goto (reg continue))
;     fact-done)

; Right off the bet, we can see that the compiler could be optimized to
; avoid unnecessary stack operations. A great example of how to achieve
; that was explained throughout the session on open-coding primitives.
; This avoids unecessary procedure/branching/stack operations by
; directly converting procedures into machine primitives. This was done
; "by hand" in the special-purpose factorial machine.
