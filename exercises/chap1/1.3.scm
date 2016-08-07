(define (square x) (* x x))

; Without min/max
(define (sum-quares-two-greatest a b c)
  (let ((greatest-first-2 (if (> a b) a b))
        (smallest-first-2 (if (> a b) b a)))
    (let ((second-greatest (if (> c smallest-first-2) c smallest-first-2)))
      (+ (square greatest-first-2)
         (square second-greatest)))))

; With min/max
(define (sum-quares-two-greatest a b c)
  (+ (square (max a b))
     (square (max c (min a b)))))

;Tests
;=====
;
(sum-quares-two-greatest 1 3 4)
;=> 25
;
(sum-quares-two-greatest  1 1 1)
;=> 2
;
(sum-quares-two-greatest 9 -1 2)
;=> 85
