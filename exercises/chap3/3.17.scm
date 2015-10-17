; Counting pairs the right way

; Let's create a better version of `count-pairs` that returns
; the number of unique pairs in a given structure. To do that,
; let's use a set to keep track of the structures that were
; already accounted for.

; Let's come up with a simple set implementation using lists. A
; smarter choice would be using a binary tree, but since we're
; leveraring the wonders of data abstraction, we could easily
; change how sets are implemented later!
(define (make-empty-set) '())
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-element x set)
  (cons x set))
(define (n-elements set)
  (if (null? set)
    0
    (+ 1 (n-elements (cdr set)))))

; Helper function
(define (maybe-adjoin x set)
  (if (element-of-set? x set)
    set
    (adjoin-element x set)))

; Solution:
(define (count-pairs struct)
  (define (collect-pairs struct set)
    (cond ((not (pair? struct)) set)
          (else
            (let ((left-pairs (collect-pairs (car struct) (maybe-adjoin struct set))))
              (let ((right-pairs (collect-pairs (cdr struct) left-pairs)))
                right-pairs)))))
  (n-elements (collect-pairs struct (make-empty-set))))


; Testing with inputs from exercise 3.16:

(define a (cons 1 (cons 2 (cons 3 '()))))
(count-pairs a)
; => 3

(define x (cons 0 0))
(define b (cons x (cons 1 x)))
(count-pairs b)
; => 3

(define x (cons 0 0))
(define y (cons x x))
(define c (cons y y))
(count-pairs c)
; => 3


; This solution works, but there is a better way of doing it, without
; having to pass around the whole set between calls to `collect-pairs`.
; This is possible because we're now allowing mutation on our data, so
; we don't have to explicitly pass new "visited" sets around anymore,
; since we can simply modify it on the inner eviroment.

(define (count-pairs struct)
  (let ((visited (make-empty-set)))
    (define (inner-count-pairs struct)
      (cond ((not (pair? struct)) 0)
            ((element-of-set? struct visited) 0)
            (else (begin (set! visited (cons struct visited))
                         (+ (inner-count-pairs (car struct))
                            (inner-count-pairs (cdr struct))
                            1)))))
    (inner-count-pairs struct)))

(count-pairs a)
; => 3

(count-pairs b)
; => 3

(count-pairs c)
; => 3

; Now we can even handle the "infinite" case:
(define d (cons 0 0))
(set-cdr! d d)
(count-pairs d)
; => 1
