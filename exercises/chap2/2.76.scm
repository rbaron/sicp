; Discussing the three types of generic operations.

; => Suppose we're analyzing the implementation of a geometry
;    system. We will discuss the three implementations.


; 1. Generic operations with explicit dispatch

; => In this case, we'd have:

(define (get-area shape)
  (cond ((circle? shape)) (circle-area shape))
        ((rectangle? shape)) (rectangle-area shape))

(define (get-perimeter shape)
  (cond ((circle? shape)) (circle-area shape))
        ((rectangle? shape)) (rectangle-area shape))

(define (rectangle? shape)
  (eq? (type-tag shape) 'rectangle))

(define (rectangle? shape)
  (eq? (type-tag shape) 'rectangle))


; Adding a new data type, in this scheme, will force us
; to edit every function of the geometry package and also
; implement constructors and predicates for the new type.


; 2. Data-directed programming

; => We'd have:

(define (install-circle-package)
  ; Internal procedures
  (define (area circle) ( ... ))
  (define (perimeter circle) ( ... ))
  ...
  (put 'area 'circle area)
  (put 'perimeter 'circle perimeter)
)

(define (install-rectangle-package)
  ; Internal procedures
  (define (area rectangle) ( ... ))
  (define (perimeter rectangle) ( ... ))
  ...
  (put 'area 'rectangle area)
  (put 'perimeter 'rectangle perimeter)
)

; Adding new data types is as simple as creating a new package
; for that type that provides the `area` and `perimeter` for
; the new type. No need to refactor existing other procedures.
; Adding new operations, on the other hand, would inevitably
; force us to edit every implementation. Unless we could write
; the new operation in terms of existing operations.


; 3. Message passing programming

(define (make-circle center radius)
  (define (dispatch op)
    (cond ((eq? op 'area) ( ... ))
          ((eq? op 'perimeter) ( ... ))))
  dispatch)

(define (make-rectingle vector1 vector2)
  (define (dispatch op)
    (cond ((eq? op 'area) ( ... ))
          ((eq? op 'perimeter) ( ... ))))
  dispatch)


; Adding new data types should be as easy as creating new
; procedure for it. Adding new operations would require
; all previous procedures to be mofidified.


; For systems in which new data types or new operations
; are often introduced, both data-directed programming
; and message passing are well-suited.
