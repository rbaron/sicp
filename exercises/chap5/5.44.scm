; Implementing find-variable in compile time environments

; Load this file with
; $ cat 5.44.scm - | mit-scheme

; Loading the compilation system with compile time env we
; set up in exercise 5.42:
(load "5.42.scm")

; Loading the open code generation from exercise 5.38:
;(load "5.38.scm")

; Since both exercise 5.42 and 5.38 override the definition for compile,
; we need to merge both into a compatible implementation that knows how
; to deal both with compile time environments and open coding generation:

(define (compile exp target linkage compile-time-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))

        ; Added
        ((open-codeable? exp)
         (open-code-generator exp target linkage compile-time-env))

        ((variable? exp)
         (compile-variable exp target linkage compile-time-env))
        ((assignment? exp)
         (compile-assignment exp target linkage compile-time-env))
        ((definition? exp)
         (compile-definition exp target linkage compile-time-env))
        ((if? exp) (compile-if exp target linkage compile-time-env))
        ((lambda? exp) (compile-lambda exp target linkage compile-time-env))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
                           compile-time-env))
        ((cond? exp) (compile (cond->if exp) target linkage compile-time-env))
        ((application? exp)
         (compile-application exp target linkage compile-time-env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

; Open code generator from exercise 5.38 augmented with compile-time-env

(define (spread-arguments argument-codes compile-time-env)
  (let ((arg1-comp (compile (car argument-codes) 'arg1 'next compile-time-env))
        (arg2-comp (compile (cadr argument-codes) 'arg2 'next compile-time-env)))
    (list arg1-comp arg2-comp)))

(define (open-code-generator exp target linkage compile-time-env)
  (let* ((operand-code (car exp))
         (arg-codes (cdr exp))
         (comp-arg-codes (spread-arguments arg-codes compile-time-env)))
    (end-with-linkage linkage
      (append-instruction-sequences
        (car comp-arg-codes)
         (preserving '(arg1)
           (cadr comp-arg-codes)
           (make-instruction-sequence '(arg1 arg2)
                                      `(,target)
                                      `((assign ,target (op ,operand-code) (reg arg1) (reg arg2)))))))))

(define (open-codeable? exp)
  (and (pair? exp)
       (memq (car exp) '(+ - / *))))

; Testing. Let's install the extra primitive operations in our
; simulator and run the compiled code using our virtual machine:

(define all-regs '(arg1 arg2 env proc val argl continue))

(define primitive-operations
  (cons (list '+ +)
    (cons (list '* *)
      (cons (list 'lexical-address-lookup lexical-address-lookup)
        (cons (list 'lexical-address-set! lexical-address-set!)
            eceval-operations)))))

(define (simulate instructions)
  (let ((test-machine (make-machine all-regs
                                    primitive-operations
                                    instructions)))
    (set-register-contents! test-machine 'env (setup-environment))
    (start test-machine)
    test-machine))

(define code
  '(begin
    (define (alternative+ arg1 arg2)
      (+ arg1 arg2))
    (define lin-comb (lambda (+ * a b x y)
      (+ (* a x) (* b y))))
    (lin-comb alternative+ * 1 2 3 4)))

(define compiled-code (compile
  code
  'val
  'next
  empty-compile-time-env))

(define machine (simulate (get-instructions compiled-code)))
(get-register-contents machine 'val)
; => 11

; Uncomment for debugging
; (pprint-instrs (get-instructions compiled-code))

; If we check the ouput for the above pprint-instrs, we can see, as the authors
; suggest, that lin-comb is still using the open coded + operations, instead of
; the user-supplied alternative+. The relevant part is the code generated for lin-comb:

;    (assign env (op compiled-procedure-env) (reg proc))
;    (assign env (op extend-environment) (const (+ * a b x y)) (reg argl) (reg env))
;    (assign arg1 (op lexical-address-lookup) (const (0 . 2)) (reg env))
;    (assign arg2 (op lexical-address-lookup) (const (0 . 4)) (reg env))
;    (assign arg1 (op *) (reg arg1) (reg arg2))
;    (assign arg1 (op lexical-address-lookup) (const (0 . 3)) (reg env))
;    (assign arg2 (op lexical-address-lookup) (const (0 . 5)) (reg env))
;    (assign arg2 (op *) (reg arg1) (reg arg2))

     ; Here. The procedure uses the open coded + regardless.
;    (assign val (op +) (reg arg1) (reg arg2))
;    (goto (reg continue))

; In order to fix this, we need to teach the open-code-generator how to lookup
; a binding in the compile-time-env. If a binding exists, we much dispatch the
; compilation to the refular compile-application procedure, so it generates the
; regular procedure call code instead of the open-code, optimized version.
(define (open-code-generator exp target linkage compile-time-env)
  (let* ((operand-code (car exp))
         (arg-codes (cdr exp))
         (comp-arg-codes (spread-arguments arg-codes compile-time-env)))

    ; Added if
    (if (not (eq? 'not-found (find-variable operand-code compile-time-env)))
      ; Dispatching to the regular compile-application code generation procedure
      (compile-application exp target linkage compile-time-env)

      ; Generating open-code for the primitive that was not bound in compile-time-env
      (end-with-linkage linkage
        (append-instruction-sequences
          (car comp-arg-codes)
           (preserving '(arg1)
             (cadr comp-arg-codes)
             (make-instruction-sequence '(arg1 arg2)
                                        `(,target)
                                        `((assign ,target (op ,operand-code) (reg arg1) (reg arg2))))))))))
(define compiled-code (compile
  code
  'val
  'next
  empty-compile-time-env))

(define machine (simulate (get-instructions compiled-code)))
(get-register-contents machine 'val)
; => 11

; Uncomment for debugging
(pprint-instrs (get-instructions compiled-code))

; Now, if we uncomment the above pprint-instrs, we can see that the overridden primitives
; + and * are now being called as regular procedures instead of the open-coded primitive.
; The code is a bit long, but the relevant parts are:

; Inside the code for lin-comb:

; Looks up the bound + procedure (in lexical address (0 0)
;    (assign proc (op lexical-address-lookup) (const (0 . 0)) (reg env))

; ...saves proc...

; Looks up the bound * (lexical address (0 1)
;    (assign proc (op lexical-address-lookup) (const (0 . 1)) (reg env))

; Build argument list for bound *
;    (assign val (op lexical-address-lookup) (const (0 . 5)) (reg env))
;    (assign argl (op list) (reg val))
;    (assign val (op lexical-address-lookup) (const (0 . 3)) (reg env))
;    (assign argl (op cons) (reg val) (reg argl))

; ...jumps to the execution of *, then for +...

; As we can see, now the bound (or overridden) procedures work as expected!
