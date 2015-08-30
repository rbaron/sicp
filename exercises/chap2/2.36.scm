; In this exercise we are asked to write an `accumulate-n` procedure
; that accumulates the corresponding elements of sequences.

; Given procedure
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

; Solution
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; Testing it out
(define ss (list (list 1 2 3) (list 4 5 6) (list 6 7 8)))

(accumulate-n + 0 ss)
; => (11 14 17)
