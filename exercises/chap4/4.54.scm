; Implementing `require` as a special form

; Load this file and drop yourself in the REPL with:
; $ cat 4.53.scm - | mit-scheme

(load "book-code/ch4-mceval.scm")
(load "book-code/ch4-ambeval.scm")

; Book supplied
(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))

; Let's finish the supplied require implementation:
;(define (analyze-require exp)
;  (let ((pproc (analyze (require-predicate exp))))
;    (lambda (env succeed fail)
;      (pproc env
;             (lambda (pred-value fail2)
;               (if <??>
;                   <??>
;                   (succeed 'ok fail2)))
;             fail))))

; require should call success if predicate is true
; or backtrack (call fail) if the predicate is false
(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not pred-value)
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))

; Adding rule to analyze
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((require? exp) (analyze-require exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

; Kicking off the evaluator
(define the-global-environment (setup-environment))
(driver-loop)

; Testing

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(let ((a (an-element-of '(1 2 3 4 5))))
  (require (= (remainder a 2) 0))
  a)

; => 2

try-again
; => 4

try-again
; => ;;; There are no more values of
; => (let ((a (an-element-of (quote (1 2 3 4 5))))) (require (= (remainder a 2) 0)) a)
