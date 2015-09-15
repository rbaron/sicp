; Generating a Huffman tree.

; Given procedures:

; 1. Sets as lists ordered by their weights
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))


; 2. Tree implementation
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; 3. Solution

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (eq? (length leaf-set) 1)
    (car leaf-set)
    (let ((right (car leaf-set))
          (left (cadr leaf-set)))
      (successive-merge
        (adjoin-set (make-code-tree left right) (cddr leaf-set))))))


; Testing:

(define pairs '((a 4) (b 1) (c 2)))

(generate-huffman-tree pairs)
; => ((leaf a 4) ((leaf c 2) (leaf b 1) (c b) 3) (a c b) 7)

; The result corresponds to the tree:

;    ((a c b) 7)
;       /    \
;  ((a), 4)   ((c b) 3)
;                /    \
;            ((c) 2)  ((b) 1)

; which is just what we expected!
