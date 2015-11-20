; Representing frames as list of key-vals

(define (make-frame variables values)
  (map cons variables values))

(define (frame-variables frame)
  (map car frame))

(define (frame-values frame)
  (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons (cons var val) frame)))

; We also have to modify the following operations on frames:

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan key-vals)
      (cond ((null? key-vals)
             (env-loop (enclosing-environment env)))
            ((eq? var (car (car key-vals)))
             (set-cdr! (car (key-vals)) val))
            (else (scan (cdr key-vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan key-vals)
      (cond ((null? key-vals)
             (add-binding-to-frame! var val frame))
            ((eq? var (car (car key-valsvars))
             (set-cdr! (car key-vals) val)))
            (else (scan (cdr vars) (cdr vals)))))
    (scan frame)))

; `lookup-variable-value` can be left as is, provided
; we still have the `frame-variables` and `frame-values`
; procedures. IMO, it would be better to change its interface
; in order to have more consistency throughout the system.

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
