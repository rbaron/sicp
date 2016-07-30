; Using lexical addresses in compile-variable and compile-assignment

(load "book-code/load-eceval-compiler.scm")
(load "book-code/ch5-compiler.scm")

; Loading the lexical-address-lookup procedure from exercise 5.39
(load "5.39.scm")

; Loading the code generators we wrote in exercise 5.40
(load "5.40.scm")

; Loading the find-variable procedure from exercise 5.41
(load "5.41.scm")

; Let's change compile-variable and compile-assignment to take
; advantage of the compile time environment system we created:

(define (compile-variable exp target linkage compile-time-env)
  (let ((found-lexical-address (find-variable exp compile-time-env)))
    (if (eq? found-lexical-address 'not-found)

      ; Usual lookup-variable-value scheme
      (end-with-linkage linkage
       (make-instruction-sequence '(env) (list target)
        `((assign ,target
                  (op lookup-variable-value)
                  (const ,exp)
                  (reg env)))))

      ; Using the compile-time-env optimization
      (end-with-linkage linkage
       (make-instruction-sequence '(env) (list target)
        `((assign ,target
                  (op lexical-address-lookup)
                  (const ,found-lexical-address)
                  (reg env))))))))

(define (compile-assignment exp target linkage compile-time-env)
  (let* ((var (assignment-variable exp))
         (get-value-code
          (compile (assignment-value exp) 'val 'next compile-time-env))
         (found-lexical-address (find-variable var compile-time-env)))
    (if (eq? found-lexical-address 'not-found)

      ; Usual set-variable-value! scheme
      (end-with-linkage linkage
       (preserving '(env)
        get-value-code
        (make-instruction-sequence '(env val) (list target)
         `((perform (op set-variable-value!)
                    (const ,var)
                    (reg val)
                    (reg env))
           (assign ,target (const ok))))))

      ; Using the compile-time-env optimization
      (end-with-linkage linkage
       (preserving '(env)
        get-value-code
        (make-instruction-sequence '(env val) (list target)
         `((perform (op lexical-address-set!)
                    (const ,found-lexical-address)
                    (reg env)
                    (reg val))
           (assign ,target (const ok)))))))))

; Testing
(define code
  '(((lambda (a)
     (lambda (b)
      (set! b 3)
      (+ a b)))
    1)
   2))

(define result (compile
  code
  'val
  'next
  empty-compile-time-env))

; Uncomment to see instructions
;(pprint-instrs (get-instructions result))

; Let's simulate the generated code.

; First, let's install the lexical-address-lookup and lexical-address-set! procedures
; in our virtual machine:

(define new-operations
  (cons (list 'lexical-address-lookup lexical-address-lookup)
    (cons (list 'lexical-address-set! lexical-address-set!)
        eceval-operations)))

(define (simulate instructions)
  (let ((test-machine (make-machine all-regs
                                    new-operations
                                    instructions)))
    (set-register-contents! test-machine 'env (setup-environment))
    (start test-machine)
    test-machine))

(define machine (simulate (get-instructions result)))
(get-register-contents machine 'val)
; => 4

; It works!
