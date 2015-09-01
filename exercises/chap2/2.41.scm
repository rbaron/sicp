; In this exercise we should write a procedure `sum-of-triplets` that returns
; all the triplets `0 <= i < j < k <= n` that sums up to `s`.

(define (enumerate-interval low high)
  (if (> low high)
    ()
    (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-triplets n)
  (flatmap (lambda (i)
       (flatmap (lambda (j)
              (map (lambda (k) (list i j k))
                   (enumerate-interval (+ j 1) n)))
            (enumerate-interval (+ i 1) n)))
    (enumerate-interval 0 n)))

(define (flatmap proc seq)
  (fold-right append () (map proc seq)))

(define (sums-to? n seq)
  (= (fold-right + 0 seq) n))

(define (sum-of-triplets n k)
  (filter (lambda (trip)
      (sums-to? k trip))
    (enumerate-triplets n)))

; Testing it out. All distinct triplets up to 10 that sums up to 7:
(sum-of-triplets 10 7)
; => ((0 1 6) (0 2 5) (0 3 4) (1 2 4))
