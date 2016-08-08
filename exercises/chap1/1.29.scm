; Integrating using Simpson's rule.

; Load this file with
; $ cat 1.29.scm - | mit-scheme

; Given procedures

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (cube n) (* n n n))

; Exercise solution
(define (simpsons-rule f a b n)
  (let ((h (/ (- b a) n)))

    (define (term k)
      (let ((yk (f (+ a (* k h)))))

        ; Uncomment for printing the terms
        (newline)(display yk)

        (cond ((or (= k 0) (= k n))
                yk)
              ((odd? k)
                (* 4 yk))
              ((even? k)
                (* 2 yk)))))

    (define (next k)
      (+ k 1))

    (* (/ h 3) (sum term 0 next n))))

; Testing

; 1. 100 intervals
(integral cube 0 1 0.01)
; => .24998750000000042
;(simpsons-rule cube 0 1 100)
; => 1/4

;2. 1000 intervals
(integral cube 0 1 0.001)
; => .249999875000001
;(simpsons-rule cube 0 1 1000)
; => 1/4

; This is very interesting. Scheme's rational numeric type garantees us
; that the integral is exact. In fact, using just two intervals yields
; the correct exact result:

(simpsons-rule cube 0 1 2)
; => 1/4
