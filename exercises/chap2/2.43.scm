; This exercise asks us to discuss an anternative implementation of the
; `queens` procedure, with the order of the consecutive calls to `map`
; switched.

; In the original `queens`, we have the following nested mapping:

(flatmap
  (lambda (rest-of-queens)
    (map (lambda (new-row)
           (adjoin-position new-row k rest-of-queens))
         (enumerate-interval 1 board-size)))
  (queen-cols (- k 1)))

; In plain English, what this piece of code does is:
; 1. Outer map (flatmap): Generate all valid ways to display of (k-1) queens on the board
; 2. Inner map: For each one of those combinations from step 1., generate a new list of
;    boards by extending it with all possible ways of displaying a new queen.


; In the Louis' version of the algorithm, we have those too calls switched:

(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

; This time, what's happening is:
; 1. Outer map (flatmap): generate all ways of placing the queen on a column.
;    That is, a list of all possible rows a queen can be placed.
; 2. Inner map: For each possible row generated on step 1., calculate all
;    possible ways of placing (k-1) queens and then append that row to all
;    possible boards.


; It is clear that Loius' version of the `queens` procedure wastes a lot of
; computation by recalculating all ways of displaying (k-1) queens, given that
; those are going to be the same for all possible new rows.

; To analyze the time complexity of the second version, we can assume most of the time
; in both versions is spent on the recursive call to `queen-cols`. In the first versions,
; the recursive call happens once for each column (so board-size times)
; and the time it takes is `T`.
; In the second version, the recursive call to `queen-cols` happen once for every possible
; new _row_, which translates to (if board-size = 8) 8 + 8^2 + 8^3 + ... + 8^8 calls.

; Roughly, then, if the original procedure runs in time `T`, Louis's version will
; run in time `T*(board-size^board-size)`.
