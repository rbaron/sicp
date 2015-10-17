; Counting pairs the wrong way

; Let's investigate ways to make the following procedure fail:

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; The exercise asks us to come up with 3-pair structures that causes
; `count-pairs` to return:

; a. 3

(define a (cons 1 (cons 2 (cons 3 '()))))
(count-pairs a)
; => 3

; b. 4

(define x (cons 0 0))
(define b (cons x (cons 0 x)))
(count-pairs b)
; => 4

; c. 7

(define x (cons 0 0))
(define y (cons x x))
(define c (cons y y))
(count-pairs c)
; => 7

; d. Inf
(define d (cons 0 0))
(set-cdr! d d)
(count-pairs d)
; => Aborting!: maximum recursion depth exceeded
