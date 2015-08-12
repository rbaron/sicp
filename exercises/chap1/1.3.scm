(define (square x) (* x x))

(define (sum-quares-two-greatest a b c)
  (+ (square (if (> a b) a b))
     (square (if (> b c) b c))))

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
