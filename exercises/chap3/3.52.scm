; Analyzing the interactions between streams and global variables

; Given procedures for testing

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

; Original map
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-enumerate-interval a b)
  (if (> a b)
    the-empty-stream
    (cons-stream a
                 (stream-enumerate-interval (+ a 1) b))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

; END OF given procedures for testing

; Let's analyze the proposed procedures:

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))

; `seq` would be the following stream (as list):
; `stream-map` has evaluated `proc` at the head of the list, 1,
; thus causing `sum` to take value of 1:

sum
; => 1


(define y (stream-filter even? seq))

; `stream-filter` has tested the predicate `even?` on every
; element of `seq` _until_ (including) the point where it's true.
; That means the following computation happened:

; step old-sum   evaluate-seq-el?  accum-arg  new-sum  even-arg even?
;  1      1           no                -       1         1      no
;  2      1           yes               2       3         3      no
;  3      4           yes               3       6         6      yes

sum
; => 6

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))

; This would couse `seq` to be evaluated until the first element
; returns a true predicate. In this case, until an element of
; `seq` is divisible by 5.

; step old-sum   evaluate-seq-el? seq  accum-arg  new-sum  mul-5?
;  1      6           no           1       -       6        no
;  2      6           no           3       -       6        no
;  3      6           no           6       -       6        no
;  4      6           yes         10       4       10       yes

sum
; => 10

(stream-ref y 7)

; This will cause `y` to be evaluated up to its 8th element.
; This, in its turn, will cause `seq` to be evaluated up to:

; step old-sum   evaluate-seq-el? seq  accum-arg  new-sum  seq-even?
;  1      10           no           1       -       10          no
;  2      10           no           3       -       10          no
;  3      10           no           6       -       10          yes 0
;  4      10           no          10       -       10          yes 1
;  5      10          yes          15       5       15          no
;  6      15          yes          21       6       21          no
;  7      21          yes          28       7       28          yes 2
;  8      28          yes          36       8       36          yes 3
;  9      36          yes          45       9       45          no
; 10      45          yes          55      10       55          no
; 11      55          yes          66      11       66          yes 4
; 12      66          yes          78      12       78          yes 5
; 13      78          yes          91      13       91          no
; 14      91          yes         105      14      105          no
; 15     105          yes         120      15      120          yes 6
; 16     120          yes         136      16      136          yes 7

sum
; => 136

(display-stream z)

; This will cause all z to be evaluated. This means _all_ elements
; of `seq` will be computed. In the end, we expect sum to match the
; sum of a arithmetic progression of 1, from 1 to 20:

; sum = (1 + 20)*20/2 = 210

sum
; => 210
