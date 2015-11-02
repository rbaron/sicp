; Generating streams of Ramanujan pairs!

; The idea is to produce a stream of pairs ordered
; by weight as i^3 + j^3 and look for pairs with the same
; weight.

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

(define (ram-weight pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* i i i) (* j j j))))

(define ordered-pairs (weighted-pairs integers integers ram-weight))

(define (make-rama-pair-stream ordered-pairs)

  (define (inner current-weight stream)
    (let ((new-weight (ram-weight (stream-car stream))))
      (if (= current-weight new-weight)
        (cons-stream (stream-car stream)
                     (inner current-weight (stream-cdr stream)))
        (inner new-weight (stream-cdr stream)))))

  (inner (ram-weight (stream-car ordered-pairs)) (stream-cdr ordered-pairs)))

(display-inf-stream ordered-pairs 10)

(display-inf-stream (stream-map ram-weight (make-rama-pair-stream ordered-pairs)) 10)
; =>
; 1729
; 4104
; 13832
; 20683
; 32832
; 39312
; 40033
; 46683
; 64232
; 65728
