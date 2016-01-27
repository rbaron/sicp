; Load this file and drop yourself in the REPL with:
; $ cat 4.44.scm - | mit-scheme

(load "book-code/ch4-mceval.scm")
(load "book-code/ch4-ambeval.scm")

; Kick off amb interpreter.
(define the-global-environment (setup-environment))
(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
    '()
    (cons (cons (car l1) (car l2))
          (zip (cdr l1) (cdr l2)))))

(define (or clause1 clause2)
  (if clause1
    true
    clause2))

(define (and clause1 clause2)
  (if clause1
    clause2
    false))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (safe-board? cols)
  (if (null? (cdr cols))
    true
    (if (safe-to-rest? (car cols) (cdr cols))
      (safe-board? (cdr cols))
      false)))

(define (safe-to-rest? queen other-queens)
  (if (null? other-queens)
    true
    (if (safe-pair? queen (car other-queens))
      (safe-to-rest? queen (cdr other-queens))
      false)))

(define (row queen) (car queen))
(define (col queen) (cdr queen))

(define (safe-pair? q1 q2)
  (let ((row1 (row q1))
        (row2 (row q2))
        (col1 (col q1))
        (col2 (col q2)))

    (and (not (= row1 row2))
         (and (not (= col1 col2))
              (not (= (abs (- col2 col1))
                      (abs (- row2 row1))))))))

; Let's start with a 5x5 board for intuition
(define (queens-5x5)
  (let ((c0 (amb 0 1 2 3 4))
        (c1 (amb 0 1 2 3 4))
        (c2 (amb 0 1 2 3 4))
        (c3 (amb 0 1 2 3 4))
        (c4 (amb 0 1 2 3 4)))

    (let ((cols (list c0 c1 c2 c3 c4)))
      (require (distinct? cols))

      (let ((row-cols (zip (list 0 1 2 3 4) cols)))
        ;(display row-cols)(display "\n")
        (require (safe-board? row-cols))

        row-cols))))


(queens-5x5)
;((0 . 0) (1 . 2) (2 . 4) (3 . 1) (4 . 3))

;try-again
;;((0 . 0) (1 . 3) (2 . 1) (3 . 4) (4 . 2))
;
;try-again
;;((0 . 1) (1 . 3) (2 . 0) (3 . 2) (4 . 4))
;
;try-again
;;((0 . 1) (1 . 4) (2 . 2) (3 . 0) (4 . 3))
;
;try-again
;;((0 . 2) (1 . 0) (2 . 3) (3 . 1) (4 . 4))
;
;try-again
;;((0 . 2) (1 . 4) (2 . 1) (3 . 3) (4 . 0))
;
;try-again
;;((0 . 3) (1 . 0) (2 . 2) (3 . 4) (4 . 1))
;
;try-again
;;((0 . 3) (1 . 1) (2 . 4) (3 . 2) (4 . 0))
;
;try-again
;;((0 . 4) (1 . 1) (2 . 3) (3 . 0) (4 . 2))
;
;try-again
;;((0 . 4) (1 . 2) (2 . 0) (3 . 3) (4 . 1))
;
;try-again
;;;; There are no more values of


; So in the 5x5 case, there are 10 solutions.

; Now let's try to come up with an automatic way of implementing a NxN
; board size.

; We wanna come up with a way of making N albiguous values for a column.
(define (n-amb-cols n)
  (if (= n 1)
    (amb 1)
    (amb n (n-amb-cols (- n 1)))))

(define (n-amb-board n)
  (define (make-rows n-rows)
    (if (= n-rows 0)
      '()
      (cons (n-amb-cols n)
            (make-rows (- n-rows 1)))))
  (make-rows n))

(define (enumerate n)
  (if (= n 0)
    '()
    (cons n (enumerate (- n 1)))))

(define (queens-NxN n)
  (let ((cols (n-amb-board n)))
      (require (distinct? cols))

      (let ((row-cols (zip (enumerate n) cols)))
        (require (safe-board? row-cols))
        row-cols)))

(queens-NxN 6)
; ((6 . 5) (5 . 3) (4 . 1) (3 . 6) (2 . 4) (1 . 2))

try-again
; ((6 . 4) (5 . 1) (4 . 5) (3 . 2) (2 . 6) (1 . 3))

try-again
; ((6 . 3) (5 . 6) (4 . 2) (3 . 5) (2 . 1) (1 . 4))

try-again
; ((6 . 2) (5 . 4) (4 . 6) (3 . 1) (2 . 3) (1 . 5))

try-again
; ;;; There are no more values of (queens-nxn 6)
