; Adding support to `let` expressions in our new interpreter

; We can approach this problem in the exact same way we did it
; in exercise 4.6. We'll treat `let` as derived expressions and
; define a syntatic transformation from `let` to `lambda` expressions.

; As a matter of fact, we can use the exact same procedure again:

(define (let->combination exp)
  (list
    (make-lambda (let-argument-names exp)
                 (let-body exp))
    (let-argument-values exp)))

(define (let-arguments exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))

(define (let-argument-names exp)
  (map car (let-arguments exp)))

(define (let-argument-values exp)
  (map car (let-arguments exp)))

(define (let? exp)
  (tagged-list? exp 'let))

; The only differece is that now, instead of modifying `eval`,
; we add the rule to `analyze`:

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))
