; Stream of all numbers that can be written as the sum of
; two squares in three different ways

; In a similar way to exercise 3.71, let's produce a stream
; of pairs ordered by a weighting function that calculates
; i^2 + j^2.

(define (weight-func pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* i i) (* j j))))

; `filter-three-same-weight` procudes a stream of _lists_. Each list
; contains the pairs having the same weight.
(define (filter-three-same-weight ordered-pair-stream weight-func)

  (define (inner current-weight same-weight-pairs pairs)
    (let ((new-weight (weight-func (stream-car pairs))))
      (if (= current-weight new-weight)
        (inner current-weight (cons (stream-car pairs) same-weight-pairs) (stream-cdr pairs))
        (if (>= (length same-weight-pairs) 3)
          (cons-stream same-weight-pairs
                       (inner new-weight (list (stream-car pairs)) (stream-cdr pairs)))
          (inner new-weight (list (stream-car pairs)) (stream-cdr pairs))))))

  (inner (weight-func (stream-car ordered-pair-stream))
         (list (stream-car ordered-pair-stream))
         (stream-cdr ordered-pair-stream)))


; ========================================
;    Begin copied code for testing
; ========================================

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (display-inf-stream s n-terms)
  (define (iter s n-terms)
    (if (> n-terms 0)
      (begin (newline)(display (stream-car s))
             (iter (stream-cdr s) (- n-terms 1)))))
  (newline)
  (iter s n-terms))

(define (merge-weighted s1 s2 weight-func)
  (if (< (weight-func (stream-car s1))
         (weight-func (stream-car s2)))
    (cons-stream (stream-car s1)
                 (merge-weighted (stream-cdr s1) s2 weight-func))
    (cons-stream (stream-car s2)
                 (merge-weighted (stream-cdr s2) s1 weight-func))))

(define (weighted-pairs s1 s2 weight-func)
  (merge-weighted
    (stream-map (lambda (e) (list (stream-car s1) e)) (stream-cdr s2))
    (cons-stream
      (list (stream-car s1) (stream-car s2))
      (weighted-pairs (stream-cdr s1) (stream-cdr s2) weight-func))
    weight-func))

; ========================================
;    END OF copied code for testing
; ========================================

; Testing

(define ordered-pairs (weighted-pairs integers integers weight-func))

(define same-sum-square-3-times (filter-three-same-weight ordered-pairs weight-func))

; In order to visualize the results, let's create a stream of lists in the format:
; (list (list of pairs with same weight) (list of weights from the pairs))
(display-inf-stream (stream-map (lambda (pairs) (list pairs (map weight-func pairs)))
                                same-sum-square-3-times)
                    10)

; =>
; (((10 15) (6 17) (1 18)) (325 325 325))
; (((13 16) (8 19) (5 20)) (425 425 425))
; (((17 19) (11 23) (5 25)) (650 650 650))
; (((14 23) (10 25) (7 26)) (725 725 725))
; (((19 22) (13 26) (2 29)) (845 845 845))
; (((15 25) (11 27) (3 29)) (850 850 850))
; (((21 22) (14 27) (5 30)) (925 925 925))
; (((20 25) (8 31) (1 32)) (1025 1025 1025))
; (((23 24) (12 31) (9 32) (4 33)) (1105 1105 1105 1105))
; (((25 25) (17 31) (5 35)) (1250 1250 1250))
