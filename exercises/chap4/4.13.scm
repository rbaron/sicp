; Implementing `make-unbound!`

; The exercise left us with a choice of unbinding variables
; in the first frame only or removing all the bindings to that
; variable.

; Let's start with the first option - we'll remove only the
; binding in the first frame, if it exists.

(define (unbound-variable! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
              (error "Unbound variable -- UNSET!" var))
            ((eq? var (car vars))
              (set-car! vars '())
              (set-car! vals '()))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

; Testing

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define env (list (cons (list 'a 'b 'c 'd) (list 1 2 3 4))))

(unbound-variable! 'b env)
env
; => (((a () c d) 1 () 3 4))
