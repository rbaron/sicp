; Implementing `if-fail`

; Load this file and drop yourself in the REPL with:
; $ cat 4.52.scm - | mit-scheme

(load "book-code/ch4-mceval.scm")
(load "book-code/ch4-ambeval.scm")

; if-fail should backtrack the execution to the alternative procedure
(define (analyze-if-fail exp)
  (let ((proc (analyze (cadr exp)))
        (alternative (analyze (caddr exp))))
    (lambda (env success fail)
      ; Call proc. The failure of if willbe the call to alternative
      (proc
        env
        (lambda (val fail2)         ; => proc success handler
          (success val fail2))
        (lambda ()                  ; => proc fail handler
          (alternative
            env
            (lambda (val fail3)     ; => alternative success handler
              (success val fail3))
            fail))))))              ; => laternative fail handler (original fail)

(define (if-fail? exp)
  (tagged-list? exp 'if-fail))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

; BEGIN CODE COPY

(define the-global-environment (setup-environment))
(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

; END CODE COPY

; Testing
(if-fail (let ((x (an-element-of '(1 3 5))))
           (require (even? x))
           x)
         'all-odd)

; => 'all-odd

(if-fail (let ((x (an-element-of '(1 3 5 8))))
           (require (even? x))
           x)
         'all-odd)

; => 8
