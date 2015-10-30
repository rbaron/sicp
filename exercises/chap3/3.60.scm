; Multiplying power series

; In order to multiply two power series, we need to apply
; the distributive multiplication property between the two
; series. Let's try to do one by hand for intuition:

; Let:

; s1(x) = a10 + a11x + a12x2 + a13x3 + a14x4 + ...
; s2(x) = a20 + a21x + a22x2 + a23x3 + a24x4 + ...

; Multiplying both series would produce:

; s1*s2(x) = x^0 * (a10a20) +
;            x^1 * (a10a21 + a11a20) +
;            x^2 * (a10a22 + a11a21 + a12a20) +
;            x^3 * (a10a23 + a11a22 + a12a21 + a13a20) +
;            x^4 * (a10a24 + a11a23 + a12a22 + a13a21 + a14a20) +
;            ...

; If we look at the terms from s1*s2(x), we can see to things:

; 1. The first element is a simple multiplication between a10 and a20

; 2. every term has a term that is the old term from s2 times a10. That
; is, a10a20, a10a21, a12a22 and so on. So a first step
; would be multiplying each term of s2 by (car s1). We'd have
; something like:

;(define (mul-series s1 s2)
;  (cons-stream (* (stream-car s1) (stream-car s2))
;   (add-streams
;
;     ; Multiply every term of s2 by the first element of s1
;     (stream-map (lambda (el) (* (stream-car s1) el)) s2)
;     <??>)))

; Now we're left with the <??> from the second term of `add-streams`.
; If we now observe the recusion case by inspecting the term x1 from
; s1*s2(x), we can see that now every term has a11! We could recurse
; with `(stream-cdr s1)` in the place of `s1` to get the expected
; result. Let's try it:

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
   (add-streams

     ; Multiply every term of s2 by the first element of s1
     (stream-map (lambda (el) (* (stream-car s1) el)) (stream-cdr s2))
     (mul-series (stream-cdr s1) s2))))


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

; ========================================
;    END OF copied code for testing
; ========================================

(define cos2 (mul-series cosine-series cosine-series))
(define sin2 (mul-series sine-series sine-series))
(define cos2+sin2 (add-streams cos2 sin2))

(display-inf-stream cos2+sin2 10)
; => 1 0 0 0 0 0 0 0 0 0

; Cool! As expected, sin²(x) + cos²(x) = 1!
