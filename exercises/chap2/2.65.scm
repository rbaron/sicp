; Union and intersection of sets (represented as balanced binary trees)
; in linear time.

; From 2.63, we know how to transform a binary tree into a ordered
; list in O(n), with `tree->list-2`. Here it is:
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; From 2.64, we know how to transform an ordered list into
; a balanced binary tree in linear time, too, with `list->tree`.
; Here it is:
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

; By gluing those two procedures together, we now have a way of:
; - Taking two sets, turning them into ordered lists;
; - "Merge" those two ordered lists in linear time;
; - Transform the merged list into a balanced binary tree in linear time.
; That means we can do union and intersection of sets by altering how
; the two ordered lists are "merged".

; Merge strategy for union of sets:
(define (union-merge list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else
          (let ((a (car list1))
                (b (car list2)))
            (cond ((= a b) (cons a (union-merge (cdr list1) (cdr list2))))
                  ((< a b) (cons a (union-merge (cdr list1) list2)))
                  ((> a b) (cons b (union-merge list1 (cdr list2)))))))))

; Merge strategy for intersection of sets:
(define (intersection-merge list1 list2)
  (cond ((or (null? list1) (null? list2)) '())
        (else
          (let ((a (car list1))
                (b (car list2)))
            (cond ((= a b) (cons a (intersection-merge (cdr list1) (cdr list2))))
                  ((< a b) (intersection-merge (cdr list1) list2))
                  ((> a b) (intersection-merge list1 (cdr list2))))))))


; Helper function
(define (set-operation merge-strategy s1 s2)
  (list->tree (merge-strategy
    (tree->list-2 s1)
    (tree->list-2 s2))))

(define (union-set s1 s2)
  (set-operation union-merge s1 s2))

(define (intersection-set s1 s2)
  (set-operation intersection-merge s1 s2))


; Given procedure
(define (make-tree entry left-tree right-tree)
  (list entry left-tree right-tree))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

; Testing
(define set1 (list->tree (list 1 2 3 4)))
(define set2 (list->tree (list 3 4 5 6)))

(tree->list-2 (union-set set1 set2))
; => (1 2 3 4 5 6)

(tree->list-2 (intersection-set set1 set2))
; => (3 4)
