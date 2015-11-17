; Implementing `let*` in terms of `let`

; The following expression

; (let* ((v1 e1)
;        (v2 e2)
;        (v3 e3))
;   <body>)

; Can also be written as

; (let ((v1 e1))
;   (let ((v2 e2))
;     (let ((v3 e3))
;   <body>)))

; So there is a transformation `let*->nested-lets` that solves
; the problem of writing `let*` as a derived expression. `let`
; itself, as implemented in exercise 4.6, is already a derived
; expression, but that shouldn't be a problem, since `eval` does
; not distinguish between them. Here's a solution:

(define (let*->nested-lets exp)
  (let*-clauses->let (let*-clauses exp)
                     (let*-body exp)))

(define (let*-clauses->let let*-clauses let*-body)
  (if (no-clauses? let*-clauses)
    let*-body
    (make-let (first-clause let*-clauses)
              (let*-clauses->let (rest-clauses let*-clauses)
                                 let*-body))))

(define (let*-clauses exp)
  (cadr exp))

(define (let*-body exp)
  (caddr exp))

(define no-clauses? null?)
(define first-clause car)
(define rest-clauses cdr)

(define (let*? exp)
  (tagged-list? exp 'let*))

; Shortcut for making a `let` expression with a single clause
(define (make-let clause body)
  (list 'let (list clause) body))


; Testing
(define exp '(let* ((a 1) (b (+ a 2)) (c (+ a b))) (+ a b)))

(let*->nested-lets exp)
; => (let ((a 1)) (let ((b (+ a 2))) (let ((c (+ a b))) (+ a b))))

; Formatted:
; (let ((a 1))
;   (let ((b (+ a 2)))
;     (let ((c (+ a b)))
;       (+ a b))))
