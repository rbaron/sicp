; Why is `analyze-sequence` so complicated?

; As suggested, let's analyze the following single-exp sequence:

; '((+ 1 2))

; 1. Using the book version of `analyze-sequence`.

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

; `procs` will be bound to a list containing a single lambda:
; procs => ((lambda (env) (execute-application ... )))

; `loop` will be called and will return single lambda:
; => (lambda (env) (execute-application ... ))

; This means the body of a lambda with a single instruction will
; be the single lambda itself.

; 2. using Eva Lu Ator's version of `analyze-sequence`.

(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))

; `procs` will be bound to a list containing a single lambda:
; procs => ((lambda (env) (execute-application ... )))

; The return value of `analyze-sequence` will be a lambda consisting
; such as:
; (lambda (env) (execute-sequence (list (lambda (env) (execute-application ... )))))


; Using a two-exps sequence such as:

; '((+ 1 2) (+ 3 4))

; 1. Using the book version of `analyze-sequence`.

; `procs` will be bound to a list of two lambdas.

; `loop` will be called and will yield a lambda such as:
; (lambda (env) (proc 1) (proc 2))

; 2. using Eva Lu Ator's version of `analyze-sequence`.

(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))

; `procs` will be bound to a list of two lambdas.

; The return value of `analyze-sequence` will be a lambda such as:
; (lambda (env) (execute-application (list proc1 proc2) env))


; In the book version, the whole sequence call is constructed at analysis
; time. On Eva's version, the sequence has to be "investigated" at call time,
; since there is a `cond` expression. Thus, it's less efficient.
