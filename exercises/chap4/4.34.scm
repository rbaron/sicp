; Pretty printing lazy lists

; Okay, so I struggled for several days with this exercise.
; The key takeaway from this was a clearer distinction between
; what's going on from the user perspective and from the evaluator
; "core".

; I took the following approach:

; 1. User inputs a lazy list such as (cons 1 2)
; 2. Evaluator core evaluates that expression:
;      `(eval '(cons 1 2))`
; 3. Evaluator core sees user's `cons` and knows it's a procedure
;    application.
; 4. Evaluator core looks up user-supplied `cons` procedure and applies
;    it:
;       `(apply <looked-up user-supplied cons> (list 1 2) <global-env>)`
; 4. User-supplied `cons` return value is eval'd and returns:
;       `(evald-lazy-list <lambda-text from cons> <augmented env with cons args>)`
;   This is how we internally store lazy lists.
; 5. Now, in order to print lazy lists in our lazy REPL, a new clause was
;    added to `user-print`, in order for us to have a custom representation
;    of lazy lists.

; With all the hard part laid out, we only have to choose how to represent
; lazy - and possibly infinite - lists. Good choices are:
; - Adding "..." after `n` elements: (1 2 3 4 5 ...)
; - Always evaluate the `car` of the list and make it clear that the `cdr`
;   was not yet evaluated, like Scala does: (1 <?>)

; I chose to go with the second approach, since it seems more elegant and less
; arbitrary.

; Again, you can load this file and drop into the REPL by typing:
; $ cat 4.34.scm - | mit-scheme

(load "book-code/ch4-mceval.scm")
(load "book-code/ch4-leval.scm")

; Overriding `eval`
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))

        ; Added handling for quoted and lazy lists
        ((quoted? exp) (eval-quoted exp env))
        ((lazy-list? exp) (eval-lazy-list exp env))
        ((evald-lazy-list? exp)

          ; Extract the actual procedure from the evaled lazy list
          (eval (cadr exp) env))

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
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

; Overriding `apply`
(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env)
           (procedure-environment procedure))))
        ((evald-lazy-list? procedure)
         (apply (eval (cadr procedure) (caddr procedure)) arguments env))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (lazy-list? object)
  (tagged-list? object 'lazy-list))

(define (evald-lazy-list? object)
  (tagged-list? object 'evald-lazy-list))

(define (eval-lazy-list exp env)
  (list 'evald-lazy-list (lazy-list-extract-lambda exp) env))

(define (lazy-list-extract-lambda exp)
  (cadr exp))

(define (user-print object)
  (cond ((evald-lazy-list? object)
          (display-lazy-list object))
        ((compound-procedure? object)
          (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>)))
        (else
          (display object))))

(define (display-lazy-list exp)
  (let* ((env (caddr exp))
         (lmd (cadr exp))

         ; The magic line. Apply user-suppolied `car` procedure
         (list-car (force-it (apply (eval 'car env) (list lmd) env))))
    (display "(")
    (display list-car)
    (display " <?>)"))))

; From quoted notation to lazy lists with user-supplied `cons`
(define (scheme-list->leval-list lst env)
  (if (null? lst)
      '()
      (eval
        (list 'cons
              (car lst)
              (list 'quote (cdr lst)))
        env)))

(define (eval-quoted exp env)
  (let ((text (cadr exp)))
    (if (pair? text)
      (scheme-list->leval-list text env)
      text)))

(define the-global-environment (setup-environment))

; Kick off our REPL - from now on everything is from user perspective
(driver-loop)

(define (cons x y)
 (lazy-list (lambda (m) (m x y))))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

; Testing

; 1. Using `cons`

(cons 1 2)
; => (1 <?>)

(car (cons 1 2))
; => 1

(cdr (cons 1 2))
; => 2

; 2. Using quote notation

'(1 2 3 4)
; => (1 <?>)

(car '(1 2 3 4))
; => 1

(cdr '(1 2 3 4))
; => (2 <?>)

(car (cdr '(1 2 3 4)))
; => 2
