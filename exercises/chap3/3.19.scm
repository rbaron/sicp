; Finding loops in lists using constant space

; Okay, I'm gonna be honest and say I've already
; seen a long time ago a very clever algorithm
; for finding loops in linked lists. I'll try to
; implement it by heart here.

; The idea is to use two pointers to traverse the
; list at different "speeds". If they ever meet,
; we can safely say there is a loop in the list.

(define (loop? lst)
  (define (loop-inner pointer-1 pointer-2)
    (cond ((eq? pointer-1 pointer-2) #t)
          ((null? (cdr pointer-1)) #f)
          ((null? (cddr pointer-2)) #f)
          (else (loop-inner (cdr pointer-1))
                (loop-inner (cddr pointer-2)))))
  (loop-inner lst (cdr lst)))


(define loopy-lst (cons 0 0))
(set-cdr! loopy-lst loopy-lst)

(loop? loopy-lst)
; => #t

(loop? (list 1 2 3))
; => #f
