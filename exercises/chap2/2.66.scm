; Implementing `lookup` for sets represented as binary trees.

(define (lookup given-key record-tree)
  (cond ((null? record-tree) #f)
        ((equal? given-key (key (entry record-tree)))
          (entry record-tree))
        ((< given-key (key (entry record-tree)))
          (lookup given-key (left-tree record-tree)))
        ((> given-key (key (entry record-tree)))
          (lookup given-key (right-tree record-tree)))))


; Helper procedures
(define (key record)
  (car record))

(define (make-tree entry left-tree right-tree)
  (list entry left-tree right-tree))

(define (entry tree) (car tree))

(define (left-tree tree) (cadr tree))

(define (right-tree tree) (caddr tree))


; Testing

;     4
;    /  \
;  3     6
;      /
;     5
(define tree1
  (list (cons 4 "employee4")
    (list (cons 3 "employee3") '() '())
    (list (cons 6 "employee6")
      (list (cons 5 "employee5") '() '())
      '())))

(lookup 5 tree1)
; => (5 . "employee5")
