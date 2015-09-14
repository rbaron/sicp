; Encoding a message with a Huffman tree.

; We are asked to design a `encode-symbol` procedure, that receives
; a symbol and a tree and outputs the list of bits representing that
; symbol.

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symbol (symbols (right-branch tree)))
          (cons 1 (encode-symbol symbol (right-branch tree))))
        ((element-of-set? symbol (symbols (left-branch tree)))
          (cons 0 (encode-symbol symbol (left-branch tree))))
        (else
          (error "Symbol not found in tree -- " symbol))))

; Given procedures
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

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

; Given encoding tree
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

; Given message
(define sample-message '(a d a b b c a))

; Result
(encode sample-message sample-tree)
; => (0 1 1 0 0 1 0 1 0 1 1 1 0)

; That is the same encoded string as the one in exercise 2.67.
