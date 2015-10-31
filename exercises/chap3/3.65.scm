; Approximating ln2 via three methods

; ln2 can be approximated by the series:

; ln2 = 1 - 1/2 + 1/3 - 1/4 + 1/5 - ...

; Let's implement a way of generating the terms
; of the approximated sequence:

(define (ln-summands start)
  (cons-stream (/ 1.0 start)
               (stream-map - (ln-summands (+ start 1)))))

; ========================================
;    Begin copied code for testing
; ========================================

(define (display-inf-stream s n-terms)
  (define (iter s n-terms)
    (if (> n-terms 0)
      (begin (newline)(display (stream-car s))
             (iter (stream-cdr s) (- n-terms 1)))))
  (newline)
  (iter s n-terms))

(define (partial-sums s1)
  (cons-stream (stream-car s1)
               (stream-map (lambda (el) (+ (stream-car s1) el))
                           (partial-sums (stream-cdr s1)))))

; ========================================
;    END OF copied code for testing
; ========================================

; Now we can define the approximation stream by
; summing all summands up to its place:
(define ln-stream (partial-sums (ln-summands 1.0)))

; a. Approximation 1: straight-forward summation

(display-inf-stream ln-stream 5)
; =>
; 1.
; .5
; .8333333333333333
; .5833333333333333
; .7833333333333333

; b. Using the `euler-transform` accelerator

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(display-inf-stream (euler-transform ln-stream) 5)
; =>
; .7
; .6904761904761905
; .6944444444444444
; .6924242424242424
; .6935897435897436

; c. Using the accelerated-accelerated, aka. tableau system

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(display-inf-stream (accelerated-sequence euler-transform ln-stream) 5)
; =>
; 1.
; .7
; .6932773109243697
; .6931488693329254
; .6931471960735491
