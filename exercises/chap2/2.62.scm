; A O(n) `union-set` implementation for sets represented as ordered lists.

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else (let ((x1 (car s1)) (x2 (car s2)))
          (cond ((= x1 x2)
                  (cons x1 (union-set (cdr s1) (cdr s2))))
                ((< x1 x2)
                  (cons x1 (union-set (cdr s1) s2)))
                ((> x1 x2)
                  (cons x2 (union-set s1 (cdr s2)))))))))

; Trying it out
(define s1 '(1 2 3 4))
(define s2 '(3 4 5 6))

(union-set s1 s2)
; => (1 2 3 4 5 6)
