; The exercise asks us to represent pais of nonnegative integers
; `(a, b)` using only the integer that is the product of 2^a * 3^b

; `cons` should produce the said integer
(define (cons a b)
  (*
    (expt 2 a)
    (expt 3 b)))

; `cdr` should receive the said integer `i` and return `b`.
; Initial idea: keep dividing `i` by 2 until the result is odd,
; at which point `i` will be 3^`b`. Take the log base 3
; of that and we have `b`.
(define (cdr i)
  (if (or (= i 1) (odd? i))
    (log3 i)
    (cdr (/ i 2))))

; `car` should receive `i` and return `a`.
; Since we already have the `cdr` procedure to extract `b`, we can
; use that to find 2^`a` = i/(3^`b`) and take the log base 2 of that
(define (car i)
  (log2 (/ i (expt 3 (cdr i)))))

; Helper functions
(define (odd? x) (not (eq? (remainder x 2) 0)))
(define (log3 x) (/ (log x) (log 3)))
(define (log2 x) (/ (log x) (log 2)))

(car (cons 7 8))
; => 7
(cdr (cons 7 8))
; => 8
