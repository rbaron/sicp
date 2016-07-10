; Monitoring the stack #2

; Load this file and drop yourself in the REPL with:
; $ cat 5.27.scm - | mit-scheme

(load "book-code/load-eceval")

; Kickoff the evaluator
(define the-global-environment (setup-environment))
(start eceval)

(define (iter-factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(define (rec-factorial n)
  (if (= n 1)
      1
      (* (rec-factorial (- n 1)) n)))

(display "\nFor 1:\n")
(iter-factorial 1)
(total-pushes = 64 maximum-depth = 10)
(rec-factorial 1)
(total-pushes = 16 maximum-depth = 8)

(display "\nFor 2:\n")
(iter-factorial 2)
(total-pushes = 99 maximum-depth = 10)
(rec-factorial 2)
(total-pushes = 48 maximum-depth = 13)


(display "\nFor 3:\n")
(iter-factorial 3)
(total-pushes = 134 maximum-depth = 10)
(rec-factorial 3)
(total-pushes = 80 maximum-depth = 18)

(display "\nFor 4:\n")
(iter-factorial 4)
(total-pushes = 169 maximum-depth = 10)
(rec-factorial 4)
(total-pushes = 112 maximum-depth = 23)

(display "\nFor 5:\n")
(iter-factorial 5)
(total-pushes = 204 maximum-depth = 10)
(rec-factorial 5)
(total-pushes = 144 maximum-depth = 28)


; For iter-factorial we have already found the max depth and
; the total pushes as linear functions of n.

; total-pushes-iter(n) = 35*n + 29
; maximum-depth-iter(n) = 10

; For the recursive version, we have:

; total-pushes-rec(n) = a*n + b

; Substituting 1 and 2 yields:

; a  + b = 16
; 2a + b = 48

; => total-pushes-rec(n) = 32*n - 16

; maximum-depth-rec(n) = c*n + d

; Substituting 1 and 2 yields:

; c  + d = 8
; 2c + d = 13

; => maximum-depth-rec(n) = 5*n + 3

; Summarizing:

;                |  maximum-depth  | total-pushes
;---------------------------------------------------
;rec-factorial   |      5n + 3     |    32n - 16
;iter-factorial  |      10         |    35n + 29

