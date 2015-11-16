; Implementing `and` and `or`

; 1. As special forms

(define (and? exp)
  (tagged-list? exp 'and))

(define (and-expressions exp) (cdr exp))

(define (and-first-exp exps) (car exps))
(define (and-rest-exps exps) (cdr exps))
(define (and-no-exps? exps) (null? exps))

(define (eval-and exps env)
  (if (and-no-exps? exps)
    true
    (if (eval (and-first-exp exps))
      (eval-and (and-rest-exps exps) env)
      false)))


(define (or? exp)
  (tagged-list? exp 'and))

(define (or-expressions exp) (cdr exp))

(define (or-first-exp exps) (car exps))
(define (or-rest-exps exps) (cdr exps))
(define (or-no-exps? exps) (null? exps))

(define (eval-or exps env)
  (if (or-no-exps? exps)
    false
    (if (eval (or-first-exp exps))
      true
      (eval-or (or-rest-exps exps) env))))


; Now we have to change our `eval` procedure to account for
; our two new special forms:

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval-and (and-expressions exp) env))
        ((or? exp) (eval-or (or-expressions exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))


; 2. As derived expressions

; We can use other special forms to write `and` and `or`. A possibility
; is to transform them in a sequence of `if`s.

(define (and->if exp)
  (and-expand-clauses (and-expressions exp)))

(define (and-expand-expressions exps)
  (if (and-no-exps? exps)
    true
    (make-if (and-first-exp exps)
             (and-expand-expressions (and-rest-exps exps))
             false)))


(define (or->if exp)
  (or-expand-clauses (and-expressions exp)))

(define (or-expand-expressions exps)
  (if (or-no-exps? exps)
    false
    (make-if (or-first-exp exps)
             true
             (or-expand-expressions (or-rest-exps exps)))))

; Modifying `eval`:

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval (and->if exp) env))
        ((or? exp) (eval (or->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))
