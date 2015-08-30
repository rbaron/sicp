; In this exercise, we are asked to analyze the `fold-left` procedure in
; comparison with the `accumulate` of `fold-right` procedure.

; Given procedures
(define (fold-right op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


;  Solution
(fold-right / 1 (list 1 2 3))
; => 3/2

(fold-left / 1 (list 1 2 3))
; => 1/6

(fold-right list () (list 1 2 3))
; => (1 (2 (3 ())))

(fold-left list () (list 1 2 3))
; => (((() 1) 2) 3)

; The proporty the operator `op` has to satisfy, so that both `fold-left` and
; `fold-right` produce the same result, is the _commutative_ property. That is,
; `(op a b) == (op b a)`.


; The exercise doesn't ask for this, but I was wondering if this implementation
; of `fold-left` is also valid. It's shorter and also tail recursive.
(define (fold-left2 op initial sequence)
  (if (null? sequence)
    initial
    (fold-left2 op (op initial (car sequence)) (cdr sequence))))

; Let's try a few calls:
(fold-left2 + 0 (list 1 2 3 4))
(fold-left + 0 (list 1 2 3 4))
; => Both yield 10

(fold-left2 list () (list 1 2 3 4))
(fold-left list () (list 1 2 3 4))
; => Both yield ((((() 1) 2) 3) 4)

(fold-left2 cons () (list 1 2 3 4))
(fold-left cons () (list 1 2 3 4))
; => Both yield ((((() . 1) . 2) . 3) . 4)

