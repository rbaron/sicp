; Rewriting `eval` in a data-oriented way

; Using data-oriented programming allows us to support
; new types without having to touch the `eval` procedure.
; We are able to "install" new types of which `eval` will
; automatically be aware.

(define (eval exp env)
  ((get 'eval (tag exp)) exp env))

(define (tag exp) (car exp))

; `eval` now looks up a procedure for each type and executes
; it passing the whole expression and the environment.
; Using the scheme, all expressions should begin with a tag.

; This would not be very fun to work with tough, since
; we would have to write all expression with a tag, such as:

;'(define (symbol x) (number 10))

; In order to be able to use some expressions in a more efficient
; way, we can hard write some clauses to eval (an also deal with
; application withouth a `call` tag):

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'eval (tag exp))
          ((get 'eval (tag exp)) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
