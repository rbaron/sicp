; Ben Bitdiddle's Pythagorean triples

; Original triples generator
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

; Ben's version
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

; Ben's version iterates over values of `i` and `j` and backtracks
; if either the sum of their squares is greater than the square of `high`
; or if the "hypothenuse" is not an integer.

; The original version iterates over values of `i`, `j` _and_ `k`. In this
; sense, Ben's version considers less values -- so it's arguably more efficient.
; On the other hand, Ben's version uses more expensive procedures, like `sqrt`.
; Granted, `sqrt` will not be called everytime, but it shows that the analysis
; can be a bit more tricky.
