; Reversing lists

; Load this file and drop yourself in the REPL with:
; $ cat 4.68.scm - | mit-scheme

(load "book-code/ch4-query")

(initialize-data-base '())
(query-driver-loop)

; Given `append-to-form` rules

(assert!
  (rule (append-to-form () ?y ?y)))

(assert!
  (rule (append-to-form (?u . ?v) ?y (?u . ?z))
        (append-to-form ?v ?y ?z)))

; Let's draw inspiration from our solution to 2.18:

; (define (rev2 lst)
;   (if (null? lst)
;     ()
;     (append (rev2 (cdr lst))  (cons (car lst) ()))))

; Our solution should be the result of appending the head of
; the list to the reversed tail of the list. The base case
; is reversing the empty list, which produces an empty list
; as well.

; Translating that to our logic programming language:

(assert!
  (rule (reverse (?head . ?tail) ?reversed)
    (and (reverse ?tail ?reversed-tail)
         (append-to-form
           ?reversed-tail
           (?head)
           ?reversed))))

(assert!
  (rule (reverse () ())))

; Testing

(reverse (1 2 3) ?x)
; => (reverse (1 2 3) (3 2 1))

(reverse ?x (1 2 3))
; => (reverse (3 2 1) (1 2 3))
; => program hangs.

; Why does it hang in the second case? For the same reason as exercise
; 4.64 -- there's an infinite loop going on with the recursive assertion.
; Since ?x will match (?head . ?tail), ?tail will also match (?head . ?tail)
; in the recursive assertion, which causes an infinite loop.
