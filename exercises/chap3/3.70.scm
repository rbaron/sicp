; Ordering pairs with a weighting function

; Instead of using the `interleave` procedure to merge
; two streams, let's define a more controlled way of
; merging streams based on a weighting function.

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

; ========================================
;    END OF copied code for testing
; ========================================

; a. Pairs ordered by the sum i+j

(define (a-weight pair) (+ (car pair) (cadr pair)))
(define a-pairs (weighted-pairs integers integers a-weight))

(display-inf-stream a-pairs 10)
; =>
; (1 1)
; (1 2)
; (2 2)
; (1 3)
; (2 3)
; (1 4)
; (3 3)
; (1 5)
; (2 4)
; (1 6)

; b. Pairs where neither i nor j is divisible by 2, 3, or
; 5, and the pairs are ordered according to the sum 2 i + 3 j + 5 i j.

(define (b-weight pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* 2 i) (* 3 j) (* 5 i j))))

(define (b-filter pair)
  (define (not-divisible-by-any n)
    (and (not (= 0 (remainder n 2)))
         (not (= 0 (remainder n 3)))
         (not (= 0 (remainder n 5)))))

  (let ((i (car pair))
        (j (cadr pair)))
    (and (not-divisible-by-any i)
         (not-divisible-by-any j))))

(define b-pairs
  (stream-filter b-filter
                 (weighted-pairs integers integers b-weight)))

(display-inf-stream b-pairs 10)
; =>
; (1 1)
; (1 7)
; (1 11)
; (1 13)
; (1 17)
; (1 19)
; (1 23)
; (1 29)
; (1 31)
; (7 7)

