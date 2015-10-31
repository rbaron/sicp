; Inverting power series

; As the exercise hinted, we can find the inverse power
; series `X` by doing:

;       S*X = 1
;  (1+Sr)*X = 1
;  X + Sr*X = 1
;         X = 1 - Sr*X

; Where Sr

; Using that idea, we can define a recursive stream
; that will yield the inverse of a given stream

(define (invert-unit-series s1)
  (define inverse
    (cons-stream 1
                (stream-map (lambda (e) (* e -1))
                            (mul-series (stream-cdr s1)
                                        inverse))))
  inverse)

; ========================================
;    Begin copied code for testing
; ========================================

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

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
  (stream-map (lambda(el)
                (/ 1 el))
              integers))

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

; ========================================
;    END OF copied code for testing
; ========================================

; Testing
(display-inf-stream (invert-unit-series sine-series) 10)
; =>  1 -1 1 -5/6 2/3 -61/120 17/45 -277/1008 62/315 -50521/362880
