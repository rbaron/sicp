; Rewriting `list-of-values` so that

; a. Arguments surely are evaluated from left to right.

(define (list-of-values-l-r exps env)
  (if (no-operands? exps)
    '()
    ; `let` does not enforce order! `let*` does!
    (let ((this-value (eval (first-exp exps) env)))
      (let ((next-values (list-of-values-l-r (rest-exps exps) env)))
        (cons this-value next-values)))))

; b. Arguments are evaluated from right to left.

(define (list-of-values-r-l exps env)
  (if (no-operands? exps)
    '()
    ; `let` does not enforce order! `let*` does!
    (let ((prev-values (list-of-values-r-l (rest-exps exps) env)))
      (let ((this-value (eval (first-exp exps) env)))
        (cons this-value prev-values)))))


; Let's try to come up with a dummy way of testing these.

(define (eval expr env) (expr))
(define no-operands? null?)
(define first-exp car)
(define rest-exps cdr)

(define exps (list; 1 2 3 4))
  (lambda () (display "1 ") 1)
  (lambda () (display "2 ") 2)
  (lambda () (display "3 ") 3)
  (lambda () (display "4 ") 4)))


(list-of-values-l-r exps '())
; Prints 1 2 3 4
; => (1 2 3 4)

(list-of-values-r-l exps '())
; Prints 4 3 2 1
; => (1 2 3 4)
