; Compiling factorial

; Load this file
; $ cat 5.34.scm - | mit-scheme

(define (pprint result)
  (newline)
  (display (car result))
  (newline)
  (display (cadr result))
  (for-each
    (lambda (inst)
      (begin (newline)
             (display inst)))
    (caddr result)))

(load "book-code/load-eceval-compiler.scm")
(load "book-code/ch5-compiler.scm")

(define result (compile
  '(define (factorial n)
     (define (iter product counter)
       (if (> counter n)
           product
           (iter (* counter product)
                 (+ counter 1))))
     (iter 1 1))
  'val
  'next))

(pprint result)

    ; Used registers
    (env)

    ; Modified registers
    (val)

    ; We start with the compilation of a definition, which in turns executes
    ; the compilation of a lambda expression.
    (assign val (op make-compiled-procedure) (label entry2) (reg env))
    (goto (label after-lambda1))

    ; Entry point from factorial
    entry2
    (assign env (op compiled-procedure-env) (reg proc))
    (assign env (op extend-environment) (const (n)) (reg argl) (reg env))

    ; lambda-body is another definition (from iter), whose entry point is entry7
    (assign val (op make-compiled-procedure) (label entry7) (reg env))
    (goto (label after-lambda6))

    ; Beginning of compilation of iter
    entry7
    (assign env (op compiled-procedure-env) (reg proc))
    (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))

    ; iter's body is an if, so we start with the compilation of the predicate (> counter n)
    ; Application starts by preserving continue and env
    (save continue)
    (save env)

    ; Compilation of the procedure > is a simple lookup
    (assign proc (op lookup-variable-value) (const >) (reg env))

    ; Construction of the argl list as a lookup of n and counter symbols
    (assign val (op lookup-variable-value) (const n) (reg env))
    (assign argl (op list) (reg val))
    (assign val (op lookup-variable-value) (const counter) (reg env))
    (assign argl (op cons) (reg val) (reg argl))

    ; Compile procedure call for (> counter n). Proc holds > and argl holds the arguments
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch22))

    compiled-branch21
    (assign continue (label after-call20))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))

    ; > is a primitive procedure, to we simply execute it
    primitive-branch22
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

    ; End of (> counter n). Result is in val register.
    after-call20
    (restore env)
    (restore continue)

    ; Testing if's predicate
    (test (op false?) (reg val))
    (branch (label false-branch9))

    ; If's true branch simple puts product in the val register and returns
    true-branch10
    (assign val (op lookup-variable-value) (const product) (reg env))
    (goto (reg continue))

    ; If's false branch recurses into (iter (* counter product) (+ counter 1))
    false-branch9

    ; Compilation of the procedure iter is a simple lookup
    (assign proc (op lookup-variable-value) (const iter) (reg env))

    ; Start the compilation of the arguments
    (save continue)
    (save proc)
    (save env)

    ; Compilation of (+ counter 1)
    (assign proc (op lookup-variable-value) (const +) (reg env))
    (assign val (const 1))
    (assign argl (op list) (reg val))
    (assign val (op lookup-variable-value) (const counter) (reg env))
    (assign argl (op cons) (reg val) (reg argl))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch16))
    compiled-branch15
    (assign continue (label after-call14))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch16
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call14
    ; End of (+ counter 1). Result in val.

    ; Initialize the argl for iter call
    (assign argl (op list) (reg val))
    (restore env)

    ; Save the argl to evaluate the second argument (* counter product)
    (save argl)

    ; Compilaion of * is a simple lookup
    (assign proc (op lookup-variable-value) (const *) (reg env))

    ; Start the compilation of *'s arguments
    (assign val (op lookup-variable-value) (const product) (reg env))
    (assign argl (op list) (reg val))
    (assign val (op lookup-variable-value) (const counter) (reg env))
    (assign argl (op cons) (reg val) (reg argl))

    ; Application of (* counter product)
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch13))
    compiled-branch12
    (assign continue (label after-call11))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch13
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call11
    ; End of (* counter product). Result is in val

    ; We then restore the previous argl (that contains the result of (+ counter 1)
    ; and cons the result of (+ counter product) to it.
    (restore argl)
    (assign argl (op cons) (reg val) (reg argl))

    ; Application of iter
    (restore proc)
    (restore continue)
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch19))

    ; iter is a compiled procedure, so we jump there. Note that we don't save nor
    ; restore the continue register arount this call! By doing so, we made the
		; call tail recursive. This is what saves us from growing our stack!
    compiled-branch18
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))

    primitive-branch19
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (goto (reg continue))
    after-call17
    after-if8
    after-lambda6
    ; End of iter's lambda definition. We assign it to the iter symbol
    ; and assing 'ok to val.
    (perform (op define-variable!) (const iter) (reg val) (reg env))
    (assign val (const ok))

    ; Now we prepare to call iter from the outer factorial procedure.
    ; We look up the iter variable and construct the argument list as '(1 1)
    (assign proc (op lookup-variable-value) (const iter) (reg env))
    (assign val (const 1))
    (assign argl (op list) (reg val))
    (assign val (const 1))
    (assign argl (op cons) (reg val) (reg argl))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch5))

    ; Since iter is a compiled procedure, we simply jump to its entry point
    compiled-branch4
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))

    primitive-branch5
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (goto (reg continue))

    after-call3
    after-lambda1

    ; End of factorial's lambda definition. We associate this lambda to the
    ; factorial symbol and 'ok to val
    (perform (op define-variable!) (const factorial) (reg val) (reg env))
    (assign val (const ok))

