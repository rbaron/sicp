; In this exercise, we are asked to implement a `unique-pairs` procedure,
; which generates the sequence of pairs `(i, j)` such that `1 <= j < i <= n`.

(define (enumerate-interval low high)
  (if (> low high)
    ()
    (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc sequence)
  (fold-right append () (map proc sequence)))

(define (unique-pairs n)
    (flatmap (lambda (i)
      (map (lambda (j) (list j i))
        (enumerate-interval 1 i)))
      (enumerate-interval 1 n)))

; Using `unique-pairs` to simply the definition of `prime-sum-pairs`:
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

; My own implementation of `prime?`
(define (prime? n)
  (define (iter n i)
    (if (= i 1)
      #t
      (if (= 0 (remainder n i))
        #f
        (iter n (- i 1)))))
  (iter n (- n 1)))

(define (prime-sum-pairs n)
  (map make-pair-sum
     (filter prime-sum?
       (unique-pairs n))))

; Testing it out
(prime-sum-pairs 10)

; Analysis of the call to `(fold-right append () (enumerate-interval 2))`
; (fold-right append () (((1 1)) ((1 2) (2 2))))
; (append ((1 1)) (fold-right append () ((1 2) (2 2))))
; (append ((1 1)) (fold-right append () ((1 2) (2 2))))
; (append ((1 1)) (append (1 2) (fold-right append () (2 2))))
; (append ((1 1)) (append (1 2) (append 2 (fold-right append () (2)))))
; (append ((1 1)) (append (1 2) (append 2 (append 2 (fold-right append () ())))))
; (append ((1 1)) (append (1 2) (append 2 (append 2 ()))))
; (append ((1 1)) (append (1 2) (append 2 (2))))
; (append ((1 1)) (append (1 2) (2 2)))
; (append ((1 1)) ((1 2) (2 2)))
; ((1 1) (1 2) (2 2))
