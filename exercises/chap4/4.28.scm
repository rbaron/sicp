; Why do operators need to be forced before applied?

; Suppose we had defined the following compound procedure:
;
; (define (my-sum a b)
;   (+ a b))
;
; and we were to evaluate the following expression:
;
; (my-sum (+ 1 2) 3)

; Without `actual-value`, `eval` would trigger the following
; `cond` clause:

; ((application? exp)
;  (apply (eval (operator exp) env) ; changed `actual-value`  to `eval`
;         (operands exp)
;         env))

; This would cause the evaluation of `my-sum` which would trigger
; the following clause on `eval`:

; ((variable? exp) (lookup-variable-value exp env))

; Which in turn would return the `my-sum` procedure
; defined on the global environment.

; This works well. What if the operator itself is a thunk? Let's
; try this one:

; (define (id x) x)
;
; ((id +) 1 2)

; This would cause the argument `+` to be delayed. `id` in this case
; returns a thunk. When evaluating the whole expression, we would try
; to `apply` a thunk, which would trigger the following `apply` clause:

; (error "Unknown procedure type -- APPLY" procedure))))

; Let's try it:

; $ mit-scheme
; (load "book-code/ch4-mceval.scm")
; (load "book-code/ch4-leval.scm")

; Modified `eval` without `actual-value`:
; (define (eval exp env)
;   (cond ((self-evaluating? exp) exp)
;         ((variable? exp) (lookup-variable-value exp env))
;         ((quoted? exp) (text-of-quotation exp))
;         ((assignment? exp) (eval-assignment exp env))
;         ((definition? exp) (eval-definition exp env))
;         ((if? exp) (eval-if exp env))
;         ((lambda? exp)
;          (make-procedure (lambda-parameters exp)
;                          (lambda-body exp)
;                          env))
;         ((begin? exp)
;          (eval-sequence (begin-actions exp) env))
;         ((cond? exp) (eval (cond->if exp) env))
;         ((application? exp)             ; clause from book
;          (apply (eval (operator exp) env)
;                 (operands exp)
;                 env))
;         (else
;          (error "Unknown expression type -- EVAL" exp))))

; (define the-global-environment (setup-environment))
; (driver-loop)

;;; L-Eval input:
; (define (id x) x)

;;; L-Eval value:
; ok

;;; L-Eval input:
; ((id +) 1 2)
;Unknown procedure type -- APPLY (thunk + (...))
;To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.
