; In this exercise, we are asked to complete two implementations of `frame`.

; Given procedures
(define (make-vect x y) (cons x y))
(define (xcor-vect vect) (car vect))
(define (ycor-vect vect) (cdr vect))

; Implementation 1:
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

; Selectors:
(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (car (cdr (cdr frame))))

; Testing:
(define frame1
  (make-frame
    (make-vect 0 0)
    (make-vect 1 1)
    (make-vect 2 2)))

(origin-frame frame1) ; => (0 . 0)
(edge1-frame frame1)  ; => (1 . 1)
(edge2-frame frame1)  ; => (2 . 2)


; Implementation 2:
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

; Same as the implementation 1
(define (origin-frame frame)
  (car frame))

; Same as the implementation 1
(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (cdr (cdr frame)))

(origin-frame frame1) ; => (0 . 0)
(edge1-frame frame1)  ; => (1 . 1)
(edge2-frame frame1)  ; => (2 . 2)
