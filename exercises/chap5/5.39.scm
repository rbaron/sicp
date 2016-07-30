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

(define (lex-addr frame-offset var-offset)
  (cons frame-offset var-offset))
(define lex-addr-frame-offset car)
(define lex-addr-var-offset cdr)

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
