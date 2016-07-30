; Lexical addressing

; Load this file
; $ cat 5.38.scm - | mit-scheme

(load "book-code/ch5-eceval-support.scm")

(define (lexical-address-lookup lex-addr env)
  (let ((frame-offset (lex-addr-frame-offset lex-addr))
        (var-offset (lex-addr-var-offset lex-addr)))
    (let* ((frame (list-ref env frame-offset))
           (value (list-ref (frame-values frame) var-offset)))
      (if (eq? value '*unassigned*)
        (error "Unassigned variable -- LEXICAL-ADDRESS-LOOKUP")
        value))))

(define (lexical-address-set! lex-addr env value)
  (let ((frame-offset (lex-addr-frame-offset lex-addr))
        (var-offset (lex-addr-var-offset lex-addr)))
    (let ((frame (list-ref env frame-offset)))
      (set-at-index! (frame-values frame) var-offset value))))

(define (lex-addr frame-offset var-offset)
  (cons frame-offset var-offset))
(define lex-addr-frame-offset car)
(define lex-addr-var-offset cdr)

(define (set-at-index! lst index value)
  (if (= index 0)
    (set-car! lst value)
    (set-at-index! (cdr lst) (- index 1) value)))

; Testing

(define test-env
  (extend-environment '(a b c)
                      '(0 1 2)
                      (extend-environment '(x y z)
                                          '(3 4 5)
                                          the-empty-environment)))

; Looking up y
(lexical-address-lookup (lex-addr 1 1) test-env)
; => 4

; Looking up a
(lexical-address-lookup (lex-addr 0 0) test-env)
; => 0

(lexical-address-set! (lex-addr 1 1) test-env 8)
test-env
; => (((a b c) 0 1 2) ((x y z) 3 8 5))
