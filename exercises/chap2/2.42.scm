; In this exercise we will analyze the famous n-queens problem. We should complete
; the given `queens` procedure.

; Base `queens` procedure

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; Given helper procedures

(define (enumerate-interval low high)
  (if (> low high)
    ()
    (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (fold-right append () (map proc seq)))

; Solution

; 1. Create a representation for the board.
; One efficient way of representing the board is simply by encoding the row
; in which the queens are in each column, such as:

; chessboard => (1, 2, 4, 7)

; Which means queens are in position (col row): (0 1), (1 2), (2 4), (3 7).

; Even though that is a more compact representation, I'm gonna go with the full
; (col row) representation, since it makes things easier to test whether queens
; are safe.

; empty-board is an empty list
(define empty-board ())

; adjoin-position
(define (adjoin-position row col rest-of-queens)
  (append (list (cons row col)) rest-of-queens))

(define (safe? _ positions)
  (define (iter row col positions)
    ; This means only the new (col row) pair, so we are safe
    (if (< (length positions) 1)
      #t
      (let ((current-row (car (car positions)))
            (current-col (cdr (car positions))))
         (if (or (= row current-row)
                 (= col current-col)
                 (= (abs (- col current-col))
                    (abs (- row current-row))))
           #f
           (iter row col (cdr positions))))))
     (iter (car (car positions)) (cdr (car positions)) (cdr positions)))

; Testing `safe?`
(define safe-4x4-board
  (adjoin-position 3 4
    (adjoin-position 1 3
      (adjoin-position 4 2
        (adjoin-position 2 1
          empty-board)))))

(define unsafe-4x4-board
  (adjoin-position 3 2
    (adjoin-position 1 3
      (adjoin-position 4 2
        (adjoin-position 2 1
          empty-board)))))

(safe? -1 safe-4x4-board)
(safe? -1 unsafe-4x4-board)

; Testing `queens`
(queens 4) ; =>  (((3 . 4) (1 . 3) (4 . 2) (2 . 1)) ((2 . 4) (4 . 3) (1 . 2) (3 . 1)))

(length (queens 5)) ; => 10
(length (queens 6)) ; => 4
(length (queens 7)) ; => 40
(length (queens 8)) ; => 92
(length (queens 9)) ; => 352


; The exercise is done, but I found the "backwards" solution too clever for me to have come up with
; in the first try. From this point on, I'm gonna try and develop a "forward" recursive solution for
; the problem.

; The idea is to develop a recursion tree in which each recursive call returns a list of valid
; boards with the current col filled in a different row.
(define (board-safe? board)
  (if (null? board)
    #t
    (if (safe? -1 board)
      (board-safe? (cdr board))
      #f)))

(define (queens2 board-size)
  (define (iter board board-size)
    (if (board-safe? board)
      (if (= (length board) board-size)
        (list board)
        (flatmap (lambda (new-row)
               (iter (adjoin-position new-row (length board) board)
                     board-size))
             (enumerate-interval 1 board-size)))
      ()))
  (iter empty-board board-size))

(length (queens2 5)) ; => 10
(length (queens2 6)) ; => 4
(length (queens2 7)) ; => 40
(length (queens2 8)) ; => 92
(length (queens2 9)) ; => 352

; As we can see, we get the same results! Cool!


