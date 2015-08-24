; In this exercise we should complete a procedure to generate all
; subsets of a given subset.

; Before looking at the provided skeleton for the procedure, I'm gonna
; try to come up with my own implementation.

; This problem is somewhat analogous to the change-counting problem, in
; which we should investigate all possible combinations of elements.
; For a given set, say (1 2 3), we will generate a binary tree. The
; left tree will return all combinations in which `1` appears and the
; right branch will return all combinations in which `1` does not appear.
(define (append-to-all value sets)
  (map (lambda (s) (cons value s)) sets))

(define (subsets set)
  (if (null? set)
    (list ())
    (append (append-to-all (car set) (subsets (cdr set)))
            (subsets (cdr set)))))

(subsets (list 1 2 3))
; => ((1 2 3) (1 2) (1 3) (1) (2 3) (2) (3) ())
; Cool!

; Now let's take a look at the given implementation and finish that up:
(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))
; =>  (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

; We've come really close! The only differences on the given skeleton are:
; - We avoid the duplicated call to `subsets` by using `let`;
; - We use `map` instead of callind `subsets` on `car` and `cdr`.

; The reason it works is commented above our first implementation.
