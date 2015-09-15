; Encoding a 50's rock song lyrics!

; Given procedures

; 1. Sets as lists ordered by their weights
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

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

; 3. Huffman tree creation, from 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (eq? (length leaf-set) 1)
    (car leaf-set)
    (let ((right (car leaf-set))
          (left (cadr leaf-set)))
      (successive-merge
        (adjoin-set (make-code-tree left right) (cddr leaf-set))))))

; 4. Encoding, from 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symbol (symbols (right-branch tree)))
          (cons 1 (encode-symbol symbol (right-branch tree))))
        ((element-of-set? symbol (symbols (left-branch tree)))
          (cons 0 (encode-symbol symbol (left-branch tree))))
        (else
          (error "Symbol not found in tree -- " symbol))))

; 5. Decoding, from 2.67

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

; Solution

; Given pairs
(define pairs '((A 2) (NA 16) (BOOM 1) (SHA  3) (GET  2) (YIP  9) (JOB  2) (WAH  1)))

; Given lyrics
(define lyrics '(
Get a job Sha na na na na na na na na
Get a job
Sha na na na na na na na na
Wah yip yip yip yip yip yip yip yip yip
Sha boom
))

; Generated tree
(define tree (generate-huffman-tree pairs))

; Encoding
(define encoded-lyrics (encode lyrics tree))
encoded-lyrics
; => (0 0 0 0 0 0 0 1 1 0 0 0 0 1 0 0 0 1 1 1 1 1 1
;     1 1 1 0 0 0 0 0 0 0 1 1 0 0 0 0 1 0 0 0 1 1 1
;     1 1 1 1 1 1 0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1
;     0 1 0 1 0 1 0 0 0 1 0 0 1 0 0)

; Let's try to decode it:
(define decoded-lyrics (decode encoded-lyrics tree))
decoded-lyrics
; => (get a job sha na na na na na na na na get a job
;     sha na na na na na na na na wah yip yip yip yip
;     yip yip yip yip yip sha boom)

; How many bits are required by the encoding?
(length encoded-lyrics)
; => 84 bits

; What is the smallest number of bits that would be needed to encode
;this song if we used a fixed-length code for the eight-symbol alphabet?
(length lyrics)
; => 36

; There are 36 symbols of 8 kind. The minimum number of bits to distin-
; guish between 8 symbols is 3, since 2^3 = 8. The total number of bits
; would be 36 * 8 = 288 bits.
