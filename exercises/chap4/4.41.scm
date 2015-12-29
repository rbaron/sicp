; Let's solve the housing problem with regular Scheme.

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (flat-map f l)
  (if (null? l)
    '()
    (append (f (car l)) (flat-map f (cdr l)))))

(define (enumerate from to)
  (if (> from to)
    '()
    (cons from (enumerate (+ from 1) to))))

; Generates all lists (i, j, k, l, m) with 1 <= i, j, k, l, m <= 5
(define (make-lists)
  (define (inner lst)
    (if (= (length lst) 5)
        (list lst)
        (flat-map (lambda (i) (inner (cons i lst)))
                  (enumerate 1 5))))
  (inner '()))

; Predicate. Will filter out all invalid answers
(define (satisfies-puzzle? solution)
  (let ((baker (car solution))
        (cooper (cadr solution))
        (fletcher (caddr solution))
        (miller (cadddr solution))
        (smith (car (cddddr solution))))
    (and
      (distinct? (list baker cooper fletcher miller smith))
      (not (= baker 5))
      (not (= cooper 1))
      (not (= fletcher 5))
      (not (= fletcher 1))
      (> miller cooper)
      (not (= (abs (- smith fletcher)) 1))
      (not (= (abs (- fletcher cooper)) 1)))))

(length (make-lists))
; => 3125

(length (filter satisfies-puzzle? (make-lists)))
; => 1

(filter satisfies-puzzle? (make-lists))
; => ((3 2 4 5 1))
