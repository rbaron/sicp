; Building a balanced tree from an ordered list.

; The given procedure:
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; a. Write a short paragraph explaining how `partial-tree` works.

; `partial-tree` works by recursively creating left trees with
; the first half of `elts` and right trees with the second half
; of it. The trees are constructed by calling `cons` on the
; newly created tree with

; `(make-tree this-entry left-tree right-tree)`

; The return value of each recursive call is

; `(cons "newly-created-tree" remaining-elts)`.

; The recursion tree for the call `(list->tree (1 3 5 7 9 11))`
; is available on 2.64.a.jpg.

; b. What's the order of growth?

; The order of growth is given by the famous recursion equation:

; T(n) = 2*T(n/2)

; On solution for it is: O(n) (since we visit each state once).

