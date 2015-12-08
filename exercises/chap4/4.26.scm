; Implementing `unless` as a special form

; Ben is correct about the ability to implement `unless` in an
; applicative order language. The only catch is that is has to
; be implemented as a derived expression and should be done at
; the interpreter's level as oposed to a normal program.

; We can plug it in out `eval` procedure as a simple syntax
; transormation:

(define (unless->if exp)
  (make-if (unless-cond exp)
           (unless-exceptional-value exp)
           (unless-usual-value exp)))

(define (unless? exp)
  (tagged-list exp 'unless))

(define (unless-cond exp)
  (cadr exp))

(define (unless-usual-value exp)
  (caddr exp))

(define (unless-exceptional-value exp)
  (cadddr exp))

; And we can plut it into `eval`.

; Alyssa's point is also valid, since implementing `unless` as a
; special form makes it impossible to use it with high-order pro-
; cedures.

; An example of case in which `unless` as special form does not
; work is:

; (map unless "cond-list" "usual-value-list" "exceptional-value-list")
