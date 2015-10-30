; Integrating power series represented as streams

; To integrate a power series such as:

; f(x) ~= a0 + a1x + a2x2 + a3x3 + ...

; We have to produce the following output:

; F(x) ~= c + a0x + 1/2 a1x2 + 1/3 a2x3 + ...

; Let's implement a `integrate-series` procedure that
; receives a stream of coefficients of a power series
; and return the "non-constant" terms of the integrated
; series. That is, we would return the above `F(x)`
; without the `c` term.


; Let's first define some useful procedures

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define ones (cons-stream 1 ones))

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

; a. Let's define the `integrate-series` procedure
(define inverse-integers
  (stream-map (lambda(el)
                (/ 1 el))
              integers))

(define (integrate-series series)
  (mul-streams inverse-integers series))

; Testing

; f(x) = 3 + 7x + 5x2
(define s1 (cons-stream 3 (cons-stream 7 (cons-stream 5 the-empty-stream))))

; F(x) (except constant))
(display-stream (integrate-series s1))
; => 3 7/2 5/3

; b.

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(display-inf-stream exp-series 10)
; =>  1 1 1/2 1/6 1/24 1/120 1/720 1/5040 1/40320 1/362880

; Let's try to use the same principle to implement the series
; for sine and cosine:

; Let's not forget the derivative of cos(x) is -sin(x)!
(define cosine-series
  (cons-stream 1 (stream-map (lambda (e) (* e -1))
                             (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(display-inf-stream cosine-series 10)
; => 1 0 -1/2 0 1/24 0 -1/720 0 1/40320 0

(display-inf-stream sine-series 10)
; => 0 1 0 -1/6 0 1/120 0 -1/5040 0 1/362880
