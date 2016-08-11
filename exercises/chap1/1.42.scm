; Composing procedures

; Load this file with
; $ cat 1.42.scm - | mit-scheme

; Let's implement a compose procedure that receives two procedures as
; arguments and returns a procedure that is the composition of them:

(define (compose f g)
  (lambda (x)
    (f (g x))))

; Testing

(define (square x) (* x x))
(define (inc x) (+ x 1))

((compose square inc) 6)
; => 49
