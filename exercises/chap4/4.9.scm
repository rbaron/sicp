; Designing some iteration constructs

(load "book-code/ch4-mceval.scm")

; 1. `while`

; With recursion
(define (while predicate procedure)
  (if (predicate)
    (begin (procedure)
           (while predicate procedure))))

; With named `let`
(define (while predicate procedure)
  (let
    proc
    ()
    (if (predicate)
      (begin (procedure)
             (proc)))))

; Testing

(define i 0)
(define predicate
  (lambda () (< i 10)))
(define procedure
  (lambda () (begin (display i)
                    (display " ")
                    (set! i (+ i 1)))))

(while predicate procedure)
; => 0 1 2 3 4 5 6 7 8 9

; We can leverage the use of named `let`s in order
; to implement the while construct into `eval` as
; a derived expression

(define (while? exp)
  (tagged-list? exp 'while))

(define (while-predicate exp)
  (cadr exp))

(define (while-procedure exp)
  (caddr exp))

(define (while->name-let exp)
  (make-named-let 'proc
                  '()
                  (make-if (while-predicate exp)
                    (make-begin (list (while-procedure exp)
                                      (list 'proc)))
                    'done)))

(define (make-named-let name bindings body)
  (list 'let name bindings body))


; Testing

(define exp '(while (< i 10)
                    (begin (display i)
                           (display " ")
                           (set! i (+ i 1)))))

(while->name-let exp)
; => (let proc
;         ()
;         (if (< i 10)
;           (begin (begin (display i)
;                         (display " ")
;                         (set! i (+ i 1)))
;                  (proc))
;         done))

; Let's try to evaluate the output:

(define i 0)
(let proc
     ()
     (if (< i 10)
       (begin (begin (display i)
                     (display " ")
                     (set! i (+ i 1)))
              (proc))
     'done))
; => 0 1 2 3 4 5 6 7 8 9
