; Detecting zero crossings in a stream of signals

; Here's Alyssa's version:
; (define (make-zero-crossings input-stream last-value)
;   (cons-stream
;    (sign-change-detector (stream-car input-stream) last-value)
;    (make-zero-crossings (stream-cdr input-stream)
;                         (stream-car input-stream))))
;
; (define zero-crossings (make-zero-crossings sense-data 0))

; The exercise then gives us the skeleton for Eva's version
; of `zero-crossings` and asks us to complete it.

; Given skeleton:
;(define zero-crossings
;  (stream-map sign-change-detector sense-data <expression>))

; We can see that `sign-change-detector` operates on two consecutive
; values from the signal stream. In order to pick two consecutive
; values from a stream (starting with a 0 in the "-1" position,
; we could use pad the second stream with a leading 0:

; (define zero-crossings
;   (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))


; Let's try it

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (list-to-stream lst)
  (if (null? lst)
    the-empty-stream
    (cons-stream (car lst) (list-to-stream (cdr lst)))))

(define (sign-change-detector current last)
  (cond ((or (and (>= current 0) (>= last 0))
             (and (< current 0) (< last 0))) 0)
        ((>= current 0) 1)
        (else -1)))

(define (display-inf-stream s n-terms)
  (define (iter s n-terms)
    (if (> n-terms 0)
      (begin (newline)(display (stream-car s))
             (iter (stream-cdr s) (- n-terms 1)))))
  (newline)
  (iter s n-terms))

(define sense-data (list-to-stream
  (list 1  2  1.5  1  0.5  -0.1  -2  -3  -2  -0.5  0.2  3  4)))

(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

(display-inf-stream zero-crossings 10)
; =>
; 0
; 0
; 0
; 0
; 0
; -1
; 0
; 0
; 0
; 0
