; Why does installing `map` as a primitive procedure fails?

; Let's try it out. Type this into the scheme interpreter:

; a. Eva Lu Ator version:
(load "book-code/ch4-mceval.scm")
(define the-global-environment (setup-environment))
(driver-loop)
(define (map func lst)
  (if (null? lst)
    '()
    (cons (func (car lst)) (map func (cdr lst)))))
(map null? '(1 2 3))
; => (#f #f #f)



; b. Louis Reasoner's version:
(load "book-code/ch4-mceval.scm")

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'map map)
        ))

(define the-global-environment (setup-environment))
(driver-loop)
(map null? '(1 2 3))
; => The object (primitive #[compiled-procedure 17 ("list" #x5) #x14 #x29e955c])
;    is not applicable.

; What is happening:
; 1. `map` is installed in the global environment with the following name:
; 'map => ('primitive <native map procedure object>)

; 2. Upon evaluation of `(map null? '(1 2 3))`, the following calls are made:

;   a. In `eval`
;      ((application? exp)
;       (apply (eval (operator exp) env)
;              (list-of-values (operands exp) env)))

;      => This will cause `eval` to be called on the symbol `map`, which will
;         cause a variable lookup and will return return `('primitive <original map>)`.
;         Also, `list-of-values` will evaluate operands:
;         - `null?` will be evaluated to `(primitive <original null?>)`
;         - `'(1 2 3) will be evaluated to `(1 2 3)`

;   b. In `apply`
;      (cond ((primitive-procedure? procedure)
;             (apply-primitive-procedure procedure arguments))
;
;   c. In `apply-primitive-procedure`
;      (define (apply-primitive-procedure proc args)
;        (apply-in-underlying-scheme
;         (primitive-implementation proc) args))
;
;   d. In `apply-in-underlying-scheme`
;      (<original apply> <original map procedure> ('primitive <original null?>) (1 2 3))
;
; There is the problem. The original application of map expects the first argument
; to be a scheme procedure. Since we are wrapping the original procedure with the
; 'primitive quotation, scheme cannot evaluate that.

