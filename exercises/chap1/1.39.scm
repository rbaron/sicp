; Estimating tan using continued fractions

; Load this file with
; $ cat 1.39.scm - | mit-scheme

; Solution of exercise 1.37
(define (cont-frac-iter n d k)
  (define (inner counter accumulated)
    (if (= counter 0)
      accumulated
      (inner (- counter 1)
             (/ (n counter)
                (+ (d counter)
                    accumulated)))))

  (inner (- k 1) (/ (n k) (d k))))

; All we need to do is to come up with the procedures n and d:

; index  |    n    |    d
;---------------------------
;   1       x           1
;   2      -x^2         3
;   3      -x^2         5
;   4      -x^2         7
;   5      -x^2         9
; ...      ...        ...

; Using the above table, we can deduce the n and d procedures

(define (tan-cf x k)
  (define (n index)
    (if (= index 1)
      x
      (- (* x x))))

  ; for d, we get:
  (define (d index)
    (- (* 2 index) 1))

  (cont-frac-iter n d k))

; Testing

; (tan pi/4) ~ 1.
(tan-cf (/ 3.1415 4) 1000)
; => .9999536742781562

; built-in tan procedure for comparison
(tan (/ 3.1415 4))
; => .9999536742781563
