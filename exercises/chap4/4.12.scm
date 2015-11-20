; Abstracting enviroment traversals

(define (scan-frame frame if-found if-not-found)
  (define (scan vars vals)
    (cond ((null? vars)
           ;(env-loop (enclosing-environment env)))
            (if-not-found env))
          ((eq? var (car vars))
            ;(set-car! vals val))
            (if-found vars))
          (else (scan (cdr vars) (cdr vals)))))

  (scan (frame-variables frame)
        (frame-values frame)))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (if-found vars)
      (car vals))
    (define (if-not-found env)
      (env-loop (enclosing-environment env)))

    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan-frame frame if-found if-not-found))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (if-found vars)
      (set-car! vals val))
    (define (if-not-found env)
      (env-loop (enclosing-environment env)))

    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan-frame frame if-found if-not-found))))
  (env-loop env))

(define (define-variable! var val env)
  (define (if-found vars)
    (set-car! vals val))
  (define (if-not-found env)
    (add-binding-to-frame! var val frame))

  (let ((frame (first-frame env)))
    (scan frame if-found if-not-found)))
