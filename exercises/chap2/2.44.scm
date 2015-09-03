; In this exercise, we are asked to implement a `up-split` procedure,
; which is analogous to the given `right-split` procedure.

; Given procedure
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

; The `up-split` procedure recursively places two smaller verions of itself
; side by side, on top of the current `painter`, up to a maximum depth of `n`.
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter n)))
      (below painter (beside smaller smaller)))))
