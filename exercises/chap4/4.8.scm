; Implementing named `let`s in addition to regular `let`s
; from exercise 4.6

(load "book-code/ch4-mceval.scm")

(define (let->combination exp)
  (if (named-let? exp)
    ; We can make a local binding (let) to evaluate
    ; named lets
    (make-let

      ; Let's add a new biniding! The new variable name
      ; is (named-let-name exp) and the new value is a
      ; lambda whose body is the let's body and arguments
      ; are (let-argument-names exp)
      (cons (named-let-name exp)
            (list (make-lambda (let-argument-names exp)
                               (let-body exp))))

      ; let body is simply a call to the named binding
      (cons (named-let-name exp)
            (let-argument-values exp)))

    ; Regular let
    (list
      (make-lambda (let-argument-names exp)
                   (let-body exp))
      (let-argument-values exp))))

; Another way of doing it would be to use `define` to
; define a procedure given by it's name. It would polute
; the global scope, which can be avoided by using local
; binding (with `let`).

(define (let-arguments exp)
  (if (named-let? exp)
    (caddr exp)
    (cadr exp)))

(define (let-body exp)
  (if (named-let? exp)
    (cdddr exp)
    (cddr exp)))

(define (let-argument-names exp)
  (map car (let-arguments exp)))

(define (let-argument-values exp)
  (map car (let-arguments exp)))

(define (let? exp)
  (tagged-list? exp 'let))

(define (named-let? exp)
  (= (length exp) 4))

(define (named-let-name exp)
  (cadr exp))

(define (make-let bindings body)
  (list 'let (list bindings) body))


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

; Testing

(define exp '(let func-name ((a 1)) (+ a 2)))

(let->combination exp)
; => (let ((func-name (lambda (a) (+ a 2)))) (func-name a))
