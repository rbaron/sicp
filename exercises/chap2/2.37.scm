; This exercise asks us to implement some common operation on matrices and
; vectors, using the following given procedures:

; Given procedures
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; Solution

; Matrix * vector: for each row of the matrix, do a dot product with the vector
(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

; Matrix transposition: select `car` of each row to be the new row and
; recurse on the `cdr` of the rows. This is exactly what `accumulate-n` does!
(define (transpose mat)
  (accumulate-n cons () mat))

; Matrix * matrix: do a matrix * vetor multiplication between each row of
; the first matrix and the second matrix transposed
; and the second matrix columns
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row))  m)))


; Testing
(define m1 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define v1 (list 1 2 3))
(define v2 (list 5 5 5))

(matrix-*-vector m1 v1)
; => (14 32 50)

(transpose m1)
; => ((1 4 7) (2 5 8) (3 6 9))

(matrix-*-matrix m1 m1)
; => ((30 36 42) (66 81 96) (102 126 150))
