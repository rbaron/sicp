; Implementing tables with binary trees

; So far, we have used a list representation for keys.
; To search for an entry in our table, we had to iterate
; over the list, which takes linear time. Using a binary
; tree to represent our values can speed up the process,
; since - if the tree is balanced - operations take O(logn).

; For simplicity, let's consider the case in which keys are
; integers. To support symbols as keys, for instance, all we
; need to do is come up with a way of implement some sort of
; "ordering" for symbols. A sufficient idea would be to convert
; the symbols to string and compare them lexographically.
; Let's also consider the case of a one dimensional table.

; Now, instead of having values pointing to the "next" values,
; we're gonna have values pointing to their left and right values,
; forming a binary tree structure.


; Tree implementation
(define (make-tree entry left right)
  (list entry left right))
(define (tree-entry tree) (car tree))
(define (tree-left tree) (cadr tree))
(define (tree-right tree) (caddr tree))
(define (set-tree-left! tree new-left) (set-car! (cdr tree) new-left))
(define (set-tree-right! tree new-right) (set-car! (cddr tree) new-right))
(define (tree-empty? tree) (null? tree))


; An entry is a key-value pair
(define (make-entry key value) (cons key value))
(define (entry-key entry) (car entry))
(define (entry-value entry) (cdr entry))
(define (set-entry-key! entry new-key) (set-car! entry new-key))
(define (set-entry-value! entry new-value) (set-cdr! entry new-value))


; Table implementation using trees
(define (make-table)
  (let ((local-table (list '*table*)))

    ; `assoc` now has to search for the key in a binary tree
    ; It returns the whole entry (a tree itself)
    (define (assoc key tree)
      (cond ((tree-empty? tree) #f)
            ((= key (entry-key (tree-entry tree)))
              (tree-entry tree))
            ((> key (entry-key (tree-entry tree)))
              (assoc key (tree-right tree)))
            (else
              (assoc key (tree-left tree)))))

    ; Lookup returns the value associated with a key or false
    ; if the key does not exist
    (define (lookup key)
      (let ((entry (assoc key (cdr local-table))))
        (if entry
          (entry-value entry)
          #f)))

    (define (insert! value key)
      (let ((new-tree (make-tree (make-entry key value) '() '())))

        (define (insert-in-tree! tree)
          (cond ((tree-empty? tree) (error "Inserting in empty tree -- INSERT-IN-TREE!"))
                ((= key (entry-key (tree-entry tree)))
                  (set-entry-value! (tree-entry tree) value))
                ((> key (entry-key (tree-entry tree)))
                  (if (tree-empty? (tree-right tree))
                    (set-tree-right! tree new-tree)
                    (insert-in-tree! (tree-right tree))))
                ((< key (entry-key (tree-entry tree)))
                  (if (tree-empty? (tree-left tree))
                    (set-tree-left! tree new-tree)
                    (insert-in-tree! (tree-left tree))))))

        ; Create first entry or start recursing to find the place to insert
        ; the new entry
        (if (tree-empty? (cdr local-table))
          (set-cdr! local-table new-tree)
          (insert-in-tree! (cdr local-table))))

      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'debug-print-tree) (newline)(display (cdr local-table)))
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))

; Exposing functions as interface

(define (insert! table value key)
  ((table 'insert-proc!) value key))

(define (lookup table key)
  ((table 'lookup-proc) key))

(define (debug-print-tree table)
  (table 'debug-print-tree))

; Testing

(define t1 (make-table))

(insert! t1 "foo" 0)

(lookup t1 0)
; => "foo"

(lookup t1 1)
; => #f

(insert! t1 "bar" 0)

(lookup t1 0)
; => "bar"

(insert! t1 "baz" 2)

(lookup t1 2)
; => "baz"

(insert! t1 "ham" -1)
(insert! t1 "spam" 5)

(debug-print-tree t1)
; => ((0 . bar) ((-1 . ham) () ()) ((2 . baz) () ((5 . spam) () ())))

; Which corresponds to:

;       0: bar
;     /     \
; -1:ham    2: baz
;           /     \
;         '()    5: spam

; Just like we'd expect!
