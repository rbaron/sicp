; Fun with the lazy evaluator

; The exercise asks us to evaluate a few expressions inside
; the lazy REPL loop. Let's try it:

(load "book-code/ch4-mceval.scm")
(load "book-code/ch4-leval.scm")

(define the-global-environment (setup-environment))
(driver-loop)

; Load this file with `(load "4.27.scm")` inside the REPL and
; then type the following lines:

(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

; Same as
(define id
        (lambda (x)
          (set! count (+ count 1))
          x))

(define w (id (id 10)))

;;; L-Eval input:
count
;;; L-Eval value:
;<response>
1

; `count` is 1 because in the evaluation of `(define w ...)`, the outer
; `id` was evaluated when evaluating:
; 1. (eval-definition exp env) - from mceval
; 2.1  (eval (definition-value exp) env) - form mceval
; 2.2  (eval '(id (id 10)) env)
;   3.     (apply (actual-value (operator '(id (id 10))) env)
;                 (operands '(id (id 10)))
;                 env)
;   3.1  (actual-value 'id env)
;        (force-it (eval 'id env)) - looks up `id` on `env`
;   3.2  (operands '(id (id 10))) - returns '((id 10))
;
; 2.3  (apply "id-procedure" '((id 10)) env)
;
;   4. (eval-sequence
;       (procedure-body "id-procedure")
;       (extend-environment
;        (procedure-parameters "id-procedure")
;        (list-of-delayed-args '((id 10)) env)
;        (procedure-environment "id-procedure")))

; This will cause the body of `id` to be evaluated, which
; will updated `count` to `count + 1`. The last expression
; on `id` is `x`, which will cause the argument `x` to be
; returned. `x` was delayed as the result of `list-of-delayed-args`,
; so it's a thunk for `((id 10))`. That is, `w` is that thunk.


;;; L-Eval input:
w

; Now `w` gets forced, since the `driver-loop` calls `force-it` on
; the user input. This causes the thunk `w` to be evaluated, which
; is the evaluation of `(id 10)`. This evaluation causes `count`
; to be incremented by 1 and returns `10`. We can see that the two
; following results are straight confirmation of these:

;;; L-Eval value:
; <response>
10
;;; L-Eval input:
count
;;; L-Eval value:
; <response>
2
