; Iterative sum

; Load this file with
; $ cat 1.30.scm - | mit-scheme

; Given recursive sum procedure
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; Given iterative sum procedure skeleton
;(define (sum term a next b)
;  (define (iter a result)
;    (if <??>
;        <??>
;        (iter <??> <??>)))
;  (iter <??> <??>))

; In order to transform a recursive procedure into a iterative one,
; I usually think in terms of "accumulators". The accumulator is updated
; in every iteration and passed to the "tail recursable" procedure.
; In the case, the accumulator was named "result" by the authors.

(define (sum-it term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

; Testing. Summing natural numbers between 0 and 100

(define (id x) x)
(define (inc x) (+ x 1))

; Expected result
(* (/ 101 2) (+ 100 0))
; => 5050

(sum id 0 inc 100)
; => 5050

(sum-it id 0 inc 100)
; => 5050
