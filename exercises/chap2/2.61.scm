; Implementing `adjoin-set` using ordered lists to represent sets.

(define (adjoin-set x set)
  (cond ((< x (car set))
           (cons x set))
        ((= x (car set))
           set)
         ((> x (car set))
           (cons (car set)
                 (adjoin-set x (cdr set))))))

; Testing
(define s1 '(1 2 4 5))
(adjoin-set 3 s1)
; => (1 2 3 4 5)

(adjoin-set 4 s1)
; => (1 2 4 5)
