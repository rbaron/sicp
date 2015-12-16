; Implementing optinal memoization and laziness to
; procedure arguments

; The exercise asks us to support the following syntax for
; procedure definitions:

; (define (f a (b lazy) c (d lazy-memo))
;   ...)

; In `f`, `a` and `c` arguments are evaluated upon application
; and `b` and `d` arguments are delayed until forced by some
; primitive procedure. `d`'s value should be memoized.

; I'm gonna implement this new syntax in this fashion:
; - `eval` continues to simply call `eval-definition`, which,
;   in turn, evaluates `make-lambda` with the extracted args
;   and body. The only difference is that now arguments can
;   be something like `a`, `(a lazy)` or `(a lazy-memo)`.
; - `apply` will execute the procedures body within an envi-
;   ronment extende by either evaluating its arguments right
;   away or by a thunk to be evaluated at later time.
; - When forcing a thunk, we should check weather we want to
;   memoize the resulting computation or not.

(load "book-code/ch4-mceval.scm")
(load "book-code/ch4-leval.scm")

; Overriding `apply` from ch4-leval.scm
(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters-names procedure)
           (list-of-delayed-or-value-args (procedure-parameters procedure) arguments env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-delayed-or-value-args proc-params arguments env)
  (if (no-operands? arguments)
      '()
      (let ((first-param (car proc-params))
            (first-arg (car arguments)))
        (cond ((pair? first-param)
                (cons (delay-it (memo-arg? first-param) first-arg env)
                      (list-of-delayed-or-value-args (cdr proc-params)
                                                     (cdr arguments)
                                                     env)))
              (else
                (cons (actual-value first-arg env)
                      (list-of-delayed-or-value-args (cdr proc-params)
                                                     (cdr arguments)
                                                     env)))))))

; `delay-it` should be aware if the thunk should be memoized or not:
(define (delay-it memo-arg? exp env)
  (if memo-arg?
    (list 'memo-thunk exp env)
    (list 'thunk exp env)))

(define (memo-arg? arg)
  (eq? (cadr arg) 'lazy-memo))

; `force-it` should also be aware if a thunk is to be memoized or computed
; every time:

(define (force-it obj)
  (cond ((memo-thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (display "\nWill evaluate a memoizable thunk!\n")
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
           (display "\nAlready have computed thunk!\n")
         (thunk-value obj))
        ((thunk? obj)
           (display "\nWill evaluate a NON-memoizable thunk!\n")
          (actual-value (thunk-exp obj) (thunk-env obj)))
        (else obj)))

(define (memo-thunk? obj)
  (tagged-list? obj 'memo-thunk))

; The way the parameters names are extracted from the procedure should also be
; changed. The reason is that paramaters now can have the form `a` and `(a lazy)`.
(define (procedure-parameters-names proc)
  (define (extract-name param)
    (if (pair? param)
      (car param)
      param))
  (map extract-name (cadr proc)))

; For efficiency, you can run this file with:
; $ cat 4.31.scm - | mit-scheme
; This will make sure mit-scheme will interpret the contents of this file
; and then start reading again from stdin so you can play around in the
; lazy interpreter.

(define the-global-environment (setup-environment))
(driver-loop)

(define (f a (b lazy) c (d lazy-memo))
  'f-ran)

(define counter 0)

; First test: make sure lazy arguments are not evaluated if neither returned
; nor used in primitive procedures:
(f 1
   (set! counter 2)
   2
   (set! counter 4))

counter
; => 0
; Great, neither lazy arguments were evaluated!

; Second test: eager arguments are evaluated even if not needed
(f 1
   (set! counter 2)
   (set! counter 3)
   (set! counter 4))

counter
; => 3
; Perfect! argument `c` was evaluated even though it's not needed.

; Third test: lazy, non-memoizable arguments are computed each time
; the thunk is needed

(define (f2 (a lazy))
  ; Primitive procedures to force thunk
  (display a)
  (display a)
  'f2-ran)
(set! counter 0)
(f2 (set! counter (+ counter 1)))
; => Will evaluate a NON-memoizable thunk!
; => ok
; => Will evaluate a NON-memoizable thunk!
; => ok
counter
; => 2

; Fourth test: lazy, memoizable thunks are only computed once
(define (f3 (a lazy-memo))
  ; Primitive procedures to force thunk
  (display a)
  (display a)
  'f3-ran)
(set! counter 0)
(f3 (set! counter (+ counter 1)))

; Will evaluate a memoizable thunk!
; ok
; Already have computed thunk!
; ok
counter
; => 1

; Exactly what we expected! The lazy, memoizable thunk's expression
; was only evaluated once!

; Okay, this is seriously super cool.
