; Dividing power series

; Using `mul-series` and `invert-unit-series`, we can
; define a `div-series` procedure that divides
; two power series.

; Our `invert-unit-series` can only invert power series
; whose constant term is 1. Let's see how we'd handle
; constant terms different than 1:

;        S*X = 1
;   (K+Sr)*X = 1
; K*X + Sr*X = 1
;          X = (1 - Sr*X)/K

(define (invert-series s1)
  (if (= (stream-car s1) 0)
    (error "Constant term is zero -- INVERT-SERIES")
    (scale-stream (invert-unit-series s1)
                  (/ 1 (stream-car s1)))))

(define (div-series s1 s2)
  (mul-series s1
              (invert-series s2)))

; ========================================
;    Begin copied code for testing
; ========================================

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (display-stream s)
  (newline)
  (stream-for-each (lambda (e) (display e)(display " ")) s))

(define (display-inf-stream s n-terms)
  (define (iter s n-terms)
    (if (> n-terms 0)
      (begin (display " ")(display (stream-car s))
             (iter (stream-cdr s) (- n-terms 1)))))
  (newline)
  (iter s n-terms))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define inverse-integers
  (stream-map / integers))

(define (integrate-series series)
  (mul-streams inverse-integers series))

(define cosine-series
  (cons-stream 1 (stream-map (lambda (e) (* e -1))
                             (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
   (add-streams

     ; Multiply every term of s2 by the first element of s1
     (stream-map (lambda (el) (* (stream-car s1) el)) (stream-cdr s2))
     (mul-series (stream-cdr s1) s2))))

(define (invert-unit-series s1)
  (define inverse
    (cons-stream 1
                (stream-map (lambda (e) (* e -1))
                            (mul-series (stream-cdr s1)
                                        inverse))))
  inverse)

; ========================================
;    END OF copied code for testing
; ========================================

; Testing

; Let's calculate the power series for tangent
(define tangent-series (div-series sine-series cosine-series))

(display-inf-stream tangent-series 10)
; =>  0 1 0 1/3 0 2/15 0 17/315 0 62/2835
