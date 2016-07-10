; Removing tail recursion optimization from the evaluator

; Load this file and drop yourself in the REPL with:
; $ cat 5.28.scm - | mit-scheme

(load "book-code/load-eceval")

; As the authors explained, tail recursion optimization comes from the
; different handling of the last expression in a sequence. By cleverly
; avoiding pushes to the stack in the last evaluation, we can avoid having
; to "come back" and undo those changes in the future. The idea to remove
; this optimization is to simply handle the last expression in a sequence
; as we would do normally.

; To make the changes appareny, I commented out the code I got rid off
; and added an "; Added" commend above the parts I introduced.

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
  '(
;;SECTION 5.4.4
read-eval-print-loop
  (perform (op initialize-stack))
  (perform
   (op prompt-for-input) (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label eval-dispatch))
print-result
  (perform (op print-stack-statistics))
  (perform
   (op announce-output) (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

unknown-expression-type
  (assign val (const unknown-expression-type-error))
  (goto (label signal-error))

unknown-procedure-type
  (restore continue)
  (assign val (const unknown-procedure-type-error))
  (goto (label signal-error))

signal-error
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

;;SECTION 5.4.1
eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))

ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))
ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))
ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))
ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure)
              (reg unev) (reg exp) (reg env))
  (goto (reg continue))

ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))
ev-appl-did-operator
  (restore unev)
  (restore env)
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)
ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))
ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))
ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label apply-dispatch))
apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))
  (branch (label compound-apply))
  (goto (label unknown-procedure-type))

primitive-apply
  (assign val (op apply-primitive-procedure)
              (reg proc)
              (reg argl))
  (restore continue)
  (goto (reg continue))

compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment)
              (reg unev) (reg argl) (reg env))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))

;;;SECTION 5.4.2
ev-begin
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (label ev-sequence))

ev-sequence
  ; Added
  (test (op no-more-exps?) (reg unev))
  (branch (label ev-sequence-done))

  (assign exp (op first-exp) (reg unev))

  ; Took out
  ;(test (op last-exp?) (reg unev))
  ;(branch (label ev-sequence-last-exp))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))

; Took out
;ev-sequence-last-exp
;  (restore continue)
;  (goto (label eval-dispatch))

; Added
; Note that ev-sequence-done is the exact same as the old
; ev-sequence-last-exp. The difference is that ev-sequence-done runs
; _after_ the last expression was executed. This means we actually have
; to come back here after the last expression was evaluated. This kills
; the nice tail recursion optimization we have.
ev-sequence-done
  ; Restore the continue pushed by either begin or ev-application
  (restore continue)
  ; Proceed with computation
  (goto (reg continue))

;;;SECTION 5.4.3

ev-if
  (save exp)
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (label eval-dispatch))
ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))
ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))

ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))
  (goto (label eval-dispatch))
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op set-variable-value!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))

ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev)
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  (goto (label eval-dispatch))
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op define-variable!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))
   )))

; Kickoff the evaluator
(define the-global-environment (setup-environment))
(start eceval)

(define (iter-factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(define (rec-factorial n)
  (if (= n 1)
      1
      (* (rec-factorial (- n 1)) n)))

; Let's now repeat the experiment from exercise 5.27 and compare the results.

(display "\nFor 1:\n")
(iter-factorial 1)
; (total-pushes = 70 maximum-depth = 17)
(rec-factorial 1)
; (total-pushes = 18 maximum-depth = 11)

(display "\nFor 2:\n")
(iter-factorial 2)
; (total-pushes = 107 maximum-depth = 20)
(rec-factorial 2)
; (total-pushes = 52 maximum-depth = 19)

(display "\nFor 3:\n")
(iter-factorial 3)
; (total-pushes = 144 maximum-depth = 23)
(rec-factorial 3)
; (total-pushes = 86 maximum-depth = 27)

; 1. iter-factorial
; 1.1 total-pushes

;  a + b = 70
; 2a + b = 107
; => 37n + 33

; 1.2 maximum-depth
;  a + b = 17
; 2a + b = 20
; => 3n + 14

; 2. rec-factorial
; 2.1 total-pushes

;  a + b = 18
; 2a + b = 52
; => 34n - 16

; 2.2 maximum-depth

;  a + b = 11
; 2a + b = 19
; => 8n + 3

; Summarizing:


; 1. Without tail recursion optimization
; =======================================

;                 |  maximum-depth  | total-pushes
; ---------------------------------------------------
; rec-factorial   |      8n + 3     |    34n - 16
; iter-factorial  |      3n + 14    |    37n + 33

; 2. With tail recursion optimization
; ===================================

;                 |  maximum-depth  | total-pushes
; ---------------------------------------------------
; rec-factorial   |      5n + 3     |    32n - 16
; iter-factorial  |      10         |    35n + 29

; As expected, we lost the nice constant-space property that we
; got as a result from the optimization. The space now grows linearly
; with n in both the iterative and the recursive factorial procedures.
