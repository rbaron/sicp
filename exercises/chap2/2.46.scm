; In this exercise, we are supposed to implement some procedures that abstracct
; the `vector` entity.

; Let's assume 2D vectors for now. Here are the constructor and selectors

(define (make-vect x y) (cons x y))

(define (xcor-vect vect) (car vect))

(define (ycor-vect vect) (cdr vect))

; Let's implement some common vector operations using these abstractions:

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v k)
  (make-vect (* (xcor-vect vect) k)
             (* (ycor-vect vect) k)))
