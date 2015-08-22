; This exercise asks us to discuss Eva's argument. She claims that formulas
; to computer with Alyssa's system will produce tighter errors if it can be
; written in way that no variable that represents and uncertain number is repeated.

; Let's recall our results from exercise 2.14 (not all procedures are reprocuced here):

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define r1 (make-center-percent 1 .01))

(show-int (par1 r1 r1)) ; => Displays .5002000200020003 +- .02999200239928024
(show-int (par2 r1 r1)) ; => Displays .5 +- 1.0000000000000009e-2

; The procedure `par2` seems to be more reliable regarding the precision of the computation.

; Originally, I thought the reason for such disparity lied on the fact that, since every
; operation on floating points is carried with limited precision, we are often better off
; avoiding dealing with small numbers where possible.
; The procedure `par2` has the advantage of having less multiplications between numbers with
; small percent tolerance. `par1`, on the other hand, multiply its arguments `r1` and `r2`, which
; results in an interval with a small tolerance, and then computes the division between that
; and another interval with another small tolerance.

; After further research [0][1][2], thought, it seems to be a more fundamental problem in the interval
; arithmetic itself. Having formulas that repeat its arguments more than once seem to cause the resulting
; interval to be wrong. In this sense, `par2` is indeed better, since it avoids applying procedures
; on repeated arguments (it applies procedures on the input and a helper variable `one`).

; Bottom line is interval arithmetic is a different world in which my unfamiliar intuition does not
; hold well. Multiplication, for instance, depends on non-linear `max` and `min` procedures, for God's sake!

; [0] http://www.billthelizard.com/2010/12/sicp-212-216-extended-exercise-interval.html
; [1] http://wiki.drewhess.com/wiki/SICP_exercise_2.16
; [2] https://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem
