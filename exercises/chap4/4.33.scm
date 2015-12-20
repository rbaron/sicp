; Using our lazy list implementation for quoted lists

; Obs.: This file can be loaded with:
; $ cat 4.33.scm - | mit-scheme

; Let's override how quoted expressions are evaluated
; so, if the quoted text is a list, we will use our new
; list implementation.

; The only thing we should be aware of is that our new
; `cons`, `car` and `cdr` implementations are not primitive
; procedures, but user defined ones. So now we have two choices:

; 1. Define `cons` et al. in the interpreter itself, so that
; lists are compatible with lists defined by the user;

; 2. Apply user defined `cons` et al. inside our `eval` procedure.

; Option 2 seems more interesting. Here it is:

(define scheme-cons cons)
(define scheme-car car)
(define scheme-cdr cdr)

(load "book-code/ch4-mceval.scm")
(load "book-code/ch4-leval.scm")

(define (scheme-list->leval-list lst env)
  (if (null? lst) '()
      (apply (eval 'cons env)
             (list (car lst)
                   (list 'quote (cdr lst) env))
              env)))

; Override `text-of-quotation` used by `eval`:
(define (text-of-quotation exp)
  (let ((text (cadr exp)))
    (if (pair? text)
      (scheme-list->leval-list text the-global-environment)
      text)))

; Kick off the interpreter
(define the-global-environment (setup-environment))
(driver-loop)

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

; Testing
'(1 2 3)
; => (compound-procedure (m) ((m x y)) <procedure-env>)

(car '(1 2 3))
; => 1

(cdr '(1 2 3))
; => (compound-procedure (m) ((m x y)) <procedure-env>)

(car (cdr '(1 2 3)))
; => 2
