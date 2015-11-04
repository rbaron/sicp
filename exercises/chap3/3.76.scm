; Modolarizing the smoothing and zero-crossing detection procedures

; Let's define an `smooth` procedure that generates the
; filtered version of a given stream:

(define (smooth stream)
  (stream-map (lambda (a b) (/ (+ a b) 2))
              stream
              (cons-stream 0 stream)))

; Now we can use `make-zero-crossings` with generic streams (it should work
; with smoothed and non-smoothed values):

(define (make-zero-crossings input-stream)
  (stream-map sign-change-detector input-stream (cons-stream 0 input-stream)))


; ========================================
;    Begin copied code for testing
; ========================================

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

; ========================================
;    END OF copied code for testing
; ========================================

(define sense-data (list-to-stream
  (list 1  2  1.5  1  0.5  -0.1  -2  -3  -2  -0.5  0.2  3  4)))

;(define smoothed-data (smooth (cons-stream 0 sense-data)))
(define smoothed-data (smooth sense-data))

(define zero-crossings (make-zero-crossings smoothed-data))

(display-inf-stream zero-crossings 10)
; =>
; 0
; 0
; 0
; 0
; 0
; 0
; -1
; 0
; 0
; 0
