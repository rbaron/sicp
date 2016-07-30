; Scanning out internal defines

; Load this file with
; $ cat 5.43.scm - | mit-scheme

(load "book-code/load-eceval-compiler.scm")
(load "book-code/ch5-compiler.scm")

; Helper procedures
(define (get-instructions compiled-code)
  (caddr compiled-code))

(define (pprint-instrs instrs)
  (if (null? instrs)
    'done
    (begin (newline)
           (display (car instrs))
           (pprint-instrs (cdr instrs)))))

; I'm gonna use some of the procedures for scanning out and generating the
; initial bindings for internal defines that I developed in exercise
; 4.16.
; The idea is to transform lambdas like:

;((lambda (a)
;  (define b 2)
;  (+ a b)) 1)

; into:

;((lambda (a)
;  ((lambda (b)
;    (set! b 2)
;    (+ a b)) '*unassigned*))
;  1)

(define (scan-out-defines procedure-body)
  (let ((bindings (definitions->bindings procedure-body))
        (filtered-body (filter-out-definitions procedure-body)))
    (if (null? bindings)
      procedure-body
      (make-bindings-lambda bindings filtered-body))))

(define (make-bindings-lambda bindings procedure-body)
  (let ((lambda-args (bindings-vars bindings))
        (lambda-body (append (bindings-set! bindings)
                             procedure-body))
        (lambda-call-args (repeat-n ''*unassigned* (length bindings))))
    (newline)
    (display (list "LAMBDA-ARGS:" lambda-args))
    (newline)
    (display (list "LAMBDA-BODY" lambda-body))
    (newline)
    (display (list "LAMBDA-CALL-ARGS" lambda-call-args))
    (let ((lamb `(,(append `(lambda ,lambda-args) lambda-body))))
      `( ,(append lamb lambda-call-args) ))))

(define (bindings-vars bindings)
  (map car bindings))

(define (repeat-n what how-many)
  (if (= 0 how-many)
    '()
    (cons what (repeat-n what (- how-many 1)))))

(define (definitions->bindings procedure-body)
  (if (null? procedure-body)
    '()
    (let ((exp (car procedure-body)))
      (if (definition? exp)
        (cons (list (definition-variable exp)
                    (definition-value exp))
              (definitions->bindings (cdr procedure-body)))
        (definitions->bindings (cdr procedure-body))))))

(define (bindings-init bindings)
  (map (lambda (binding) (list (car binding) '*unassigned*)) bindings))

(define (bindings-set! bindings)
  (map (lambda (binding) (list 'set! (car binding) (cadr binding))) bindings))

(define (filter-out-definitions procedure-body)
  (filter (lambda (exp) (not (definition? exp))) procedure-body))

(define (transform-lambda-exp exp)
  (make-lambda (lambda-parameters exp)
               (scan-out-defines (lambda-body exp))))

; Modifying compile-lambda to apply the scan-out-defines transformation
(define (compile-lambda exp target linkage)
  ; Debug
  ;(newline)(display "ORIGINAL LAMBDA:\n")(display exp)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda))

        ; Added
        (exp (transform-lambda-exp exp)))
    ; Debug
    (newline)(display "TRANSFORMED LAMBDA:\n")(display exp)
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry))
       after-lambda))))

; Testing
(define code
  '((lambda (b)
      (define a 2)
      (+ a b))
    1))

(define compiled-code (compile
  code
  'val
  'next))

; Uncomment to see instructions
;(pprint-instrs (get-instructions compiled-code))

; Let's simulate the generated code:

(define (simulate instructions)
  (let ((test-machine (make-machine all-regs
                                    eceval-operations
                                    instructions)))
    (set-register-contents! test-machine 'env (setup-environment))
    (start test-machine)
    test-machine))

(define machine (simulate (get-instructions compiled-code)))
(get-register-contents machine 'val)
; => 3

; It... works? :O

; Debug output:

;ORIGINAL LAMBDA:
;(lambda (b)
;  (define a 2)
;  (+ a b))
;
;TRANSFORMED LAMBDA:
;((lambda (b)
;  ((lambda (a)
;    (set! a 2)
;    (+ a b)) 0)) 1)
