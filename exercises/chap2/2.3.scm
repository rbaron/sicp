; Let's create a representation for rectangles using, as hinted, the
; representations for line segments from exercise 2.2. A rectangle is formed
; by putting together four line segments.

; Useful constructors and extractors from 2.2.scm:

(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment seg) (car seg))

(define (end-segment seg) (cdr seg))

(define (make-point x y) (cons x y))

(define (x-point point) (car point))

(define (y-point point) (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (print-seg seg)
  (newline)
  (print-point (start-segment seg))
  (print-point (end-segment seg)))

; Let's implement the rectangle constructor
(define (make-rect s1 s2 s3 s4)
  (cons s1 (cons s2 (cons s3 s4))))

(define (fst-seg rect) (car rect))

(define (snd-seg rect) (car (cdr rect)))

(define (seg-len seg)
  (sqrt
    (+
      (square (- (x-point (start-segment seg)) (x-point (end-segment seg))))
      (square (- (y-point (start-segment seg)) (y-point (end-segment seg)))))))

; Let's define the `perimeter` and `area` in terms of our representation
(define (rect-perimeter rect)
  (* 2
    (+ (seg-len (fst-seg rect)) (seg-len (snd-seg rect)))))
;(define (rect-perimeter rect)
;  (seg-len (fst-seg rect)))

(define (rect-area rect)
  (* (seg-len (fst-seg rect)) (seg-len (snd-seg rect))))

; Trying a rect out. This rect `r1` has the left lower corner on (0,0),
; height 3 and length 2
(define (r1) (make-rect
  (make-segment (make-point 0 0) (make-point 0 3))
  (make-segment (make-point 0 3) (make-point 2 3))
  (make-segment (make-point 2 3) (make-point 2 0))
  (make-segment (make-point 2 0) (make-point 0 0))))

(rect-perimeter (r1))

(rect-area (r1))

; The exercise asks us to design abstraction barriers, so we can still use our
; `rect-perimeter` and `rect-area` procedures.

;-------[ rect-perimeter, rect-area ]----------------------------------
;      Rectangles as list of segments or points
;-------[ make-rect, rect-heigt rect-width ]---------------------------
;      Segments as pais of points
;-------[ make-segment, start-segment, end-segment, seg-len ]----------
;      Points as (x, y) coordinates
;-------[ make-point, x-point, y-point ]-------------------------------
;      Points, segments, rectangles as lists
;-------[ cons, car, cdr ]---------------------------------------------
;      Whatever pairs are implemented

; By designing the program this way, `rect-perimeter` and `rect-area` don't
; care about how `make-rect` is implemented, since it can use `rect-height`
; and `rect-width` in a higher-level to compute the perimeter and area.

; Let's write `rect-perimeter` and `rect-area` in terms of `rect-height` and
; `rect-width`:
(define (rect-perimeter rect)
  (* 2 (+
    (rect-height rect)
    (rect-width rect))))

(define (rect-area rect)
  (* (rect-height rect)
     (rect-width rect)))

; Now let's use these procedures in two different `rect` representations.

; In the original representation, we would have:
(define (rect-height rect)
  (seg-len (car rect)))

(define (rect-width rect)
  (seg-len (car (cdr rect))))

(define (rect1) (make-rect
  (make-segment (make-point 0 0) (make-point 0 3))
  (make-segment (make-point 0 3) (make-point 2 3))
  (make-segment (make-point 2 3) (make-point 2 0))
  (make-segment (make-point 2 0) (make-point 0 0))))

(rect-perimeter (r1))
(rect-area (r1))

; Let's create a second representation for `rect`.)
)
; We can see that our rect representation is somewhat)
; redundant. We have represented each corner of the triangle twice, since we are
; defining the triangle in terms os line segments. Let's try and represent a rectangle
; directly by it's corners:

(define (make-rect c1 c2 c3 c4)
  (cons c1 (cons c2 (cons c3 c4))))

; Now let's implement the required `rect-height` and `rect-width` procedures:
(define (rect-height rect)
  (seg-len
    (make-segment
      (car rect)
      (car (cdr rect)))))

(define (rect-width rect)
  (seg-len
    (make-segment
      (car (cdr rect))
      (car (cdr (cdr rect))))))

(define (r2) (make-rect
  (make-point 0 0)
  (make-point 0 3)
  (make-point 2 3)
  (make-point 2 0)))

(rect-perimeter (r2))
(rect-area (r2))

; As we can see, we have used the same implementation of `rect-perimeter` and
; `rect-area` despite having different representations of `rect`.
