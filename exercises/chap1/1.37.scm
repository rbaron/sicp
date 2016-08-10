; K-term finite continued fraction

; Load this file with
; $ cat 1.37.scm - | mit-scheme

; a. Approach #1: - recursive, very elegant but potentially nasty
(define (cont-frac-rec n d k)
  (define (inner counter)
    (if (= counter k)
      (/ (n counter)
         (d counter))
      (/ (n couter)
         (+ (d counter)
            (inner (+ counter 1))))))
  (inner 1))

; b. Approach #2 - going backwards - iterative
(define (cont-frac-iter n d k)
  (define (inner counter accumulated)
    (if (= counter 0)
      accumulated
      (inner (- counter 1)
             (/ (n counter)
                (+ (d counter)
                    accumulated)))))

  (inner (- k 1) (/ (n k) (d k))))

; Oh nooooooooooesssssss!!111!!
(/ 1. (cont-frac-rec
        (lambda (n) 1)
        (lambda (d) 1)
        100000))
; => ;Aborting!: maximum recursion depth exceeded

(/ 1. (cont-frac-iter
        (lambda (n) 1)
        (lambda (d) 1)
        100000))
; => 1.6180339887498947

; Let's find out the smallest k that yields an approximation of
; the golden ratio that is accurate up to the 4th decimal place:

(define (phi-estimate k)
  (/ 1. (cont-frac-iter
          (lambda (n) 1)
          (lambda (d) 1)
          k)))

(define (err phi-estimate)
  (abs (- phi-estimate 1.6180339887498)))

(define (monotonic-search k)
  (if (< (err (phi-estimate k)) 1e-4)
    k
    (monotonic-search (+ k 1))))

(monotonic-search 1)
; => 11

; Using k = 11, we get:
(phi-estimate 11)
; => 1.6179775280898876

; And the error is:
(- (err (phi-estimate 11)))
; => -5.646065991249394e-5
