; a. Implementing `letrec` as a derived expression

(define (letrec? exp)
  (tagged-list exp 'letrec))

(define (letrec->let-set! exp)
  (append (list 'let (letrec-init-bindings exp))
          (letrec-body-set!s exp)
          (letrec-body exp)))

(define (letrec-init-bindings exp)
  (map (lambda (binding) (list (car binding) '*unassigned*))
       (cadr exp)))

(define (letrec-body-set!s exp)
  (map (lambda (binding) (cons 'set! binding))
       (cadr exp)))

(define (letrec-body exp)
  (cddr exp))

; Testing

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define exp
  '(letrec ((b 1)
            (c 2))
      (+ b c)))

(letrec->let-set! exp)
; (let ((b *unassigned*)
;       (c *unassigned*))
;   (set! b 1)
;   (set! c 2)
;  (+ b c))

; b. Environment for execution of `f`:
; (define (f x)
;   (letrec ((even?
;             (lambda (n)
;               (if (= n 0)
;                   true
;                   (odd? (- n 1)))))
;            (odd?
;             (lambda (n)
;               (if (= n 0)
;                   false
;                   (even? (- n 1))))))
;     <rest of body of f>))

; This will be transformed to:
; (define (f x)
;   (let ((even? '*unassigned*)
;         (odd? '*unassigned*))
;      (set! even? <even? lambda>)
;      (set! odd? <odd? lambda>)
;     <rest of body of f>))

; Which is desugared to:
; (define (f x)
;   ((lambda (even? odd?)
;      (set! even? <even? lambda>)
;      (set! odd? <odd? lambda>)
;      <rest of body of f>) '*unassigned* '*unassigned*))

; Upon calling `(f 5)`, a new environment E1 will be created, in
; which `x` will be 5. The inner lambda will be executed, creating
; a new environment E2, in which `even?` and `odd?` will be initially
; bound to `'*unassigned*`. Inside E2, `even?` and `odd?` will have
; their value mutated to represent the procedures given by <even? lambda>
; and <odd? lambda>. Both these procedures will have E2 as their closing
; environment, so their inner reference to each other will be looked
; up correctly.

; Let's see what it would look like if we simply used `let`:
(define (f x)
  (let ((even? <even? lambda>)
        (odd? <odd? lambda>))
    <rest of body of f>))

; Which is desugared to:
(define (f x)
  ((lambda (even? odd?)
    <rest of body of f>) (<even? lambda>
                          <odd? lambda>)))

; In this case, the arguments for the inner application will
; be evaluated to procedures whose enclosing enviroment is E1.
; When applying the topmost lambda, E2 will be created and the
; evaluated procedure arguments will be bound to `even?` and `odd?`.
; The problem is, when calling `even?`, for instance, it will create
; a new environment E3, whose closing environment is E1, so it won't
; be able to loop up the definition of `odd?`, since it was bound in E2!

; For further discussion on this topic,; see
; ../experiments/internal-definitions.scm.
