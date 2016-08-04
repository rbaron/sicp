; Compiling and simulating the metacircular evaluator

; Load this file with
; $ cat 5.50.scm - | mit-scheme

; There were a few challenges in order to make everything work as expected:

; - apply primitive: apply is a particular kind of primitive, because the metacircular
;   evaluator calls it itself, instead of delegating it to... apply. Applying a primitive
;   is done through a direct call to apply-in-underlying-scheme, which expects the real apply
;   procedure, and not the onw exposed as primitive by the reg sim. Thus, we need to change the
;   exposed apply so it can deal with primitives represented as (list 'primitive #proc) instead
;   of simply #proc. This is done via apply-two-level-primitive;

; - let: the compiler doesn't know how to handle let-expressions. I used a let->combination
;   syntax transformation procedure so we can transform let into lambdas;

; - map: we can't use the primitive map, because the metacircular evaluator's representation
;   of procedures is incompatible with scheme's procedure representation. I implemented map
;   as a function in the metacircular evaluator's scope.


(load "book-code/load-eceval-compiler.scm")
(load "book-code/ch5-compiler.scm")

; The complete code for the metacircular evaluator is defined as mceval-code, wrapped
; in a begin expression. The parts I modified are commented in the beginning of the file
(load "5.50.quoted-mceval.scm")

(define apply-two-level-primitive
  (lambda (fn args) (apply (cadr fn) args)))

; Extended list of primitive procedures for the register machine simulator
(define primitive-procedures
  (append (list (list 'apply apply-two-level-primitive)
                (list 'read read)
                (list 'newline newline)
                (list 'list list)
                (list 'length length)
                (list 'eq? eq?)
                (list 'number? number?)
                (list 'string? string?)
                (list 'symbol? symbol?)
                (list 'pair? pair?)
                (list 'error error)
                (list 'set-car! set-car!)
                (list 'set-cdr! set-cdr!)
                (list 'display display)
                (list '+ +)
                (list '- -)
                (list '= =)
                (list 'not not)
                (list 'cadr cadr)
                (list 'cddr cddr)
                (list 'cdddr cdddr)
                (list 'cdadr cdadr)
                (list 'caadr caadr)
                (list 'cadddr cadddr)
                (list 'caddr caddr))
          primitive-procedures))

; Teaching the compiler to understand let-expressions
(define (let? exp) (tagged-list? exp 'let))

(define (let-bindings exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (let-var binding) (car binding))

(define (let-val binding) (cadr binding))

(define (make-combination operator operands) (cons operator operands))

(define (let->combination exp)
  (let ((bindings (let-bindings exp)))
    (cons (make-lambda (map let-var bindings)
                                   (let-body exp))
          (map let-val bindings))))


; Installing the let->combination transformation
(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))

        ; Added
        ((let? exp) (compile (let->combination exp ) target linkage))

        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

; Compiling the meta circular evaluator
(define compiled-code
  (compile mceval-code 'val 'next))

; Helper procedures for simulating the compiled code
(define (get-instructions compiled-code)
  (caddr compiled-code))

(define (simulate instructions)
  (let ((test-machine (make-machine all-regs
                                    eceval-operations
                                    instructions)))
    (set-register-contents! test-machine 'env (setup-environment))
    (start test-machine)
    test-machine))

; Run the machine with the compiled code installed
(simulate (get-instructions compiled-code))

; The following will run in the compiled metacircular evaluator:

(+ 1 2)
; => ;;; M-Eval value:
; => 3

(define (inc n) (+ n 1))
; => ;;; M-Eval value:

(inc 1)
; => ;;; M-Eval value:
; => 2

(define (fib n)
  (if (= n 1)
    1
    (if (= n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2))))))
; => ;;; M-Eval value:
; => ok

(fib 5)
; => ;;; M-Eval value:
; => 5

(fib 6)
; => ;;; M-Eval value:
; => 8
