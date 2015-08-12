(define (p i j)
  (cond
    ((= j 0) 1)
    ((= j (+ i 1)) 0)
    (else
      (+
        (p (- i 1) (- j 1))
        (p (- i 1) j)))))


; Helper function to print Pascal's triangle up to row `to-row`
(define (print-pascal-iter i j to-row)

  (if (= j 0) (display "\n"))
  (if (<= i to-row) (display (p i j)))
  (display " ")

  (if (> i to-row)
    0
    (if (< j i)
      (print-pascal-iter i (+ j 1) to-row)
      (print-pascal-iter (+ i 1) 0 to-row))))


(define (print-pascal to-row)
  (print-pascal-iter 0 0 to-row))


(print-pascal 5)
