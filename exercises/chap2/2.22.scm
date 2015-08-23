; This exercise asks us to discuss why Louis' implementations of `square-list`
; won't work.

; Implementation 1
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

; The problem with the first implemenetation is that the results are produced
; in a reverse order. The culprit for that is the call to:

; `(cons (square (car things)) answer)`

; which generates a new `answers` list by appending the square of `items`
; to the _beginning_ of the old answer. We can have a clue of what's happening
; by running a few iterations of the `iter` procedure by hand:

; it  |  things   | answer
; ------------------------
; 0   |  1 2 3 4  | nil
; 1   |  2 3 4    | (cons (1 nil))
; 2   |  3 4      | (cons 4 (cons 1 nil))
; 3   |  4        | (cons 9 (cons 4 (cons 1 nil)))
; 4   |  nil      | (cons 16 (cons 9 (cons 4 (cons 1 nil))))


; Implementation 2
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

; The problem with this implementation is that, by simplying switching the order
; of the arguments to `cons`, Louis created a "nested" list structure, which is not
; the result we were expecting. By running a few iterations of this version, we get:

; it  |  things   | answer
; ------------------------
; 0   |  1 2 3 4  | nil
; 1   |  2 3 4    | (cons (nil 1))
; 2   |  3 4      | (cons (cons (nil 1)) 4)
; 3   |  4        | (cons (cons (cons (nil 1)) 4) 9)
; 4   |  nil      | (cons (cons (cons (cons (nil 1)) 4) 9) 16)


; The exercise doesn't ask for this, but a correct implementation of `square-list`
; would, for example, use `append` instead of `cons`, like this:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                    (list (square (car things)))))))
  (iter items ()))

; Trying this out:
(square-list (list 1 2 3 4 5))
; => (1 4 9 16 25)
