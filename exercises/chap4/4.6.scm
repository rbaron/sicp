; Implementing `let` as a derived expression

(define (let->combination exp)
  (list
    (make-lambda (let-argument-names exp)
                 (let-body exp))
    (let-argument-values exp)))

(define (let-arguments exp)
  (cadr exp))

(define (let-body exp)
  (caddr exp))

(define (let-argument-names exp)
  (map car (let-arguments exp)))

(define (let-argument-values exp)
  (map car (let-arguments exp)))

(define (let? exp)
  (tagged-list? exp 'let))

; Setting up the syntax in `eval`:

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
        ((let? exp) (eval (let->combination exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))
