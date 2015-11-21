; Scanning out internal definitions

(load "book-code/ch4-mceval.scm")

; a. Change lookup-variable-value (section 4.1.3) to signal
;    an error if the value it finds is the symbol `'*unassigned*`.

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
               (error "Unassigned variable" var)
               (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; b. Write a procedure scan-out-defines that takes a procedure
;    body and returns an equivalent one that has no internal definitions,
;    by making the transformation described above.

(define (scan-out-defines procedure-body)
  (let ((bindings (definitions->bindings procedure-body)))
    (make-let (bindings-init bindings)
              (append (bindings-set! bindings)
                      (filter-out-definitions procedure-body)))))

(define (definitions->bindings procedure-body)
  (if (null? procedure-body)
    '()
    (let ((exp (car procedure-body)))
      (if (definition? exp)
        (cons (list (definition-variable exp)
                    (definition-value exp))
              (definitions->bindings (cdr procedure-body)))
        (definitions->bindings (cdr procedure-body))))))

(define (bindings-init bindings)
  (map (lambda (binding) (list (car binding) '*unassigned*)) bindings))

(define (bindings-set! bindings)
  (map (lambda (binding) (list 'set! (car binding) (cadr binding))) bindings))

(define (filter-out-definitions procedure-body)
  (filter (lambda (exp) (not (definition? exp))) procedure-body))

(define (make-let bindings body)
  (list 'let bindings body))

; Testing

(define exp '(lambda (a) (
               (define u 1)
               (define v 2)
               (+ u v))))

(scan-out-defines (procedure-body exp))
; => (let ((u *unassigned*)
;          (v *unassigned*))
;      ((set! u 1)
;       (set! v 2)
;       (+ u v)))


; c. Install scan-out-defines in the interpreter, either in make-procedure
;    or in procedure-body (see section 4.1.3). Which place is better? Why?

; Installing it in `make-procedure` will ensure `scan-out-defines` will only
; run once. If we choose to install in `procedure-body`, we might have to
; run it several times.
