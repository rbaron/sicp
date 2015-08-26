; This exercise asks us to define some common sequence operations in
; terms of `accumulate`.

; Given prodecure
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

; Implementations
(define (map p sequence)
  (accumulate (lambda (acc, x) (cons (p x) acc))  sequence))

; append
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

; length
(define (length seq)
  (accumulate (lambda (acc, el) (+ acc 1)) 0 sequence))


; Trying them out
(map (lambda (x) (* x x)) (list 1 2 3 4))
; => (1 4 9 16)

(append (list 1 2 3) (list 4 5 6))
; => (1 2 3 4 5 6)

(length (list 1 2 3 4 5))
; => 5
