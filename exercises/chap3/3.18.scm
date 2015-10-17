; A poor man's halting problem solution

; Let's try to find whether a list has a loop.
; The idea is to keep track of all `cdr`s visited
; so far. If we detect one `cdr` pointing to a pair
; we've already seen, there is a loop.

; First way: using a mutable internal state (`visited`)
(define (loop? lst)
  (let ((visited '()))
    (define (loop-internal? lst)
      (cond ((null? lst) #f)
            ((memq (car lst) visited) #t)
            (else (begin (set! visited (cons (car lst) visited))
                         (loop-internal? (cdr lst))))))
    (loop-internal? lst)))


(define loopy-lst (cons 0 0))
(set-cdr! loopy-lst loopy-lst)

(loop? loopy-lst)
; => #t

(loop? (list 1 2 3))
; => #f


; Second way: using an immutable state as argument
(define (loop? lst)
    (define (loop-internal? lst visited)
      (cond ((null? lst) #f)
            ((memq (car lst) visited) #t)
            (else (loop-internal? (cdr lst) (cons (car lst) visited)))))
    (loop-internal? lst '()))

(loop? loopy-lst)
; => #t

(loop? (list 1 2 3))
; => #f
