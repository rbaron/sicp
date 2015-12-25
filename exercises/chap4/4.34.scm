; Pretty printing lazy lists

; Let's reuse most of the code from exercise 4.33.

(load "book-code/ch4-mceval.scm")
(load "book-code/ch4-leval.scm")

; Overriding `eval`display
(define (eval exp env)
  (display "\nEval: ")(display (if (pair? exp) (cadr exp) exp))
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ;((variable? exp) (display "\n\tVariable: ")(display exp)(newline)(lookup-variable-value exp env))
        ((quoted? exp) (eval-quoted exp env))
        ;((quoted? exp) (eval-quoted exp env))

        ; Added handling for lazy lists
        ((lazy-list? exp) (eval-lazy-list exp env))
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
        ((application? exp)             ; clause from book
         (display "\n\tApplying procedure: ")(display (car exp))
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (lazy-list? object)
  (tagged-list? object 'lazy-list))

(define (eval-lazy-list exp env)
  ; So now a lazy list will be internally represented by a
  ; list consisting of the symbol `'lazy-list` and the actual
  ; `cons` procedure
  ;(list 'lazy-list (eval (cadr exp) the-global-environment)))
  ;(list 'lazy-list (actual-value (lazy-list-extract-lambda exp) env) env))
  (list 'lazy-list (lazy-list-extract-lambda exp) env))

(define (lazy-list-extract-lambda exp)
  (cadr exp))

(define (user-print object)
  (cond ((lazy-list? object)
          (display-lazy-list object))
        ((compound-procedure? object)
          (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>)))
        (else
          ;(display "a"))))
          (display object))))

(define (display-lazy-list exp)
  (let ((env (caddr exp))
        (lmd (cadr exp)))
  (display "\nDISPLAYING LAZY LIST: ")
  ;(display "\n\t Proc car body: ")(display (procedure-body (eval 'car env)))
  ;(display "\n\t Proc lmb body: ")(display (procedure-body lmd))
  ;(display (apply (eval 'car env) (list 1 2) env))
  ;(apply (eval 'car env) (list lmd) env)
  (display (force-it (apply (eval 'car env) (list lmd) env)))
  ))

; BEGIN CODE FROM EXERCISE 4.33

(define (scheme-list->leval-list lst env)
  (display "scheme-list->leval-list")
  (if (null? lst)
      '()
      (eval
        (list 'cons
              (car lst)
              (list 'quote (cdr lst)))
        env)))

(define (eval-quoted exp env)
  (let ((text (cadr exp)))
    (display "TEXT: ")
    (display exp)
    (if (pair? text)
      (scheme-list->leval-list text env)
      text)))

(define the-global-environment (setup-environment))
(driver-loop)

(define (cons x y)
 (lazy-list (lambda (nnn) (nnn x y))))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(cons 1 2)
