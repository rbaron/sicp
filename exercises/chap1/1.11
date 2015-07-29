; Recursive version
; =================

(define (f1 n)
  (if (< n 3)
    n
    (+
      (f1 (- n 1))
      (* 2 (f1 (- n 2)))
      (* 3 (f1 (- n 3))))))

; (f1 4) == 11
; (f1 5) == 25
; (f1 6) == 59


; Iterative version
; =================

; To get the yet new syntax out of the way, I started off with an intentionally
; unpythonic python implementation of the iterative algorithm.

;def f(n):
;  if n < 3:
;          return n
;    else:
;      i = 3
;      fnminus1 = 2
;      fnminus2 = 1
;      fnminus3 = 0
;      while i <= n:
;        f = fnminus1 + 2*fnminus2 + 3*fnminus3
;        fnminus3 = fnminus2
;        fnminus2 = fnminus1
;        fnminus1 = f
;        i += 1
;      return f

; Moving to scheme:
(define (f-iter fnminus3 fnminus2 fnminus1 i n)
  (if (> i n)
    fnminus1
    (f-iter
      fnminus2
      fnminus1
      (+ fnminus1 (* 2 fnminus2) (* 3 fnminus3))
      (+ i 1)
      n)))

(define (f2 n)
  (if (< n 3)
    n
    (f-iter 0 1 2 3 n)))

; (f2 2) == 2
; (f2 3) == 4
; (f2 4) == 11
; (f2 6) == 59
