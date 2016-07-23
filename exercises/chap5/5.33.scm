; Compiling factorial

; Load this file and drop yourself in the REPL with:
; $ cat 5.33.scm - | mit-scheme

(load "book-code/load-eceval-compiler.scm")
(load "book-code/ch5-compiler.scm")

(define (pprint result)
  (newline)
  (display (car result))
  (newline)
  (display (cadr result))
  (pprint-instrs (caddr result)))

(define (pprint-instrs instrs)
  (if (null? instrs)
    'done
    (begin (newline)
           (display (car instrs))
           (pprint-instrs (cdr instrs)))))

(define result (compile
  '(define (factorial-alt n)
     (if (= n 1)
     1
     (* n (factorial-alt (- n 1)))))
  'val
  'next))

; Initially, the compile procedure will receive the full expression and
; the predicate definition? will be true. It will cause the compilation of
; the definition-value and results will be put in the val register. Linkage
; is 'next. This triggers the compilation of a lambda expression.

(pprint result)

    ; Used registers
    (env)

    ; Modified registers
    (val)

    ; The compilation of the lambda starts using target val and 'next linkage.
    ; It generates two labels: 'after-lambda(1) and 'entry(2).
    ; make-compiled-procedure creates a list that indicates the entrypoint and
    ; the procedures' environment.
    ; The 'next linkage causes the execution to jump to after-lambda1.
    (assign val (op make-compiled-procedure) (label entry2) (reg env))
    (goto (label after-lambda1))

    ; Start of compile-lambda-body
    ; It starts by setting the entrypoint label entry2, then restoring
    ; the procedure environment and extending is the the procedure's arguments.
    ; It is expected that the arguments are already in the register argl.
    entry2
    (assign env (op compiled-procedure-env) (reg proc))
    (assign env (op extend-environment) (const (n)) (reg argl) (reg env))

    ; Start of compile-sequence with lambda-body. Targer: val, linkage: 'return

    ; Preserving continue and env
    (save continue)
    (save env)

    ; The lambda-body contains a single expression to be compiled:
    ; The if expression. It is compiled using target val and linkage 'next

    ; compile-if starts by generating 3 labels (true-branch, false-branch, after-if).
    ; It also changes the linkage to the consequence branch to be 'after-if, so it
    ; "exits" to the correct place. It starts by compiling the predicate with target
    ; val and linkage 'next.

    ; if's predicate compilation is a procedure application (= n 1)
    ; Compiling an application first compiles the procedure itself.
    ; In this case, procedure is a symbol, which is looked up in the current
    ; env and assigned to proc.
    (assign proc (op lookup-variable-value) (const =) (reg env))

    ; Now the arguments of the procedure application are compiled and adjoined with
    ; construct-arglist. It causes the execution of the operand-codes and construction
    ; of those values from the val register into the argl register.
    (assign val (const 1))
    (assign argl (op list) (reg val))
    (assign val (op lookup-variable-value) (const n) (reg env))
    (assign argl (op cons) (reg val) (reg argl))

    ; Start of compile-procedure-call
    ; Makes three labels (primitive-branch, compiled-branch and after-call)
    ; Adjusts linkage to compiled-branch compilation to after-call
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch17))

    ; If the procedure is a compiled procedure, we put the entry-point for the
    ; procedure into val (it's a label), put after-call into continue and jump
    ; to the label.
    compiled-branch16
    (assign continue (label after-call15))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))

    ; Else, the procedure is a primitive procedure, which we simply execute and
    ; assign to target, which is val
    primitive-branch17
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call15

    (restore env)
    (restore continue)

    ; Now we executed the if's predicate and the result is in val.
    (test (op false?) (reg val))
    (branch (label false-branch4))

    ; True branch simple assigns 1 to val and returns
    true-branch5
    (assign val (const 1))
    (goto (reg continue))

    ; False branch is the compilation of the if's alternative branch
    ; (* n (factorial-alt (- n 1))). It starts by compiling the procedure
    ; application, which is a primitive procedure.
    false-branch4
    ; Loads the procedure into proc register
    (assign proc (op lookup-variable-value) (const *) (reg env))

    ; Starts the evaluation of the last argument (factorial-alt (- n 1))
    (save continue)
    (save proc)
    (save env)
    (assign proc (op lookup-variable-value) (const factorial-alt) (reg env))

    ; Evaluation of (- n 1)
    (save proc)
    (assign proc (op lookup-variable-value) (const -) (reg env))
    (assign val (const 1))
    (assign argl (op list) (reg val))
    (assign val (op lookup-variable-value) (const n) (reg env))
    (assign argl (op cons) (reg val) (reg argl))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch8))
    compiled-branch7
    (assign continue (label after-call6))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch8
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call6

    ; End of evaluation of (- n 1). Result is in val register.

    ; Constructing the argl list for factorial-alt
    (assign argl (op list) (reg val))

    ; Restoring factorial-alt
    (restore proc)

    ; compile-procedure-call for factorial-alt. At this point, arguments are
    ; loaded into argl
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch11))

    ; Since factorial-alt is a compiled procedure, we jump there with continue
    ; set to after-call9.
    compiled-branch10
    (assign continue (label after-call9))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch11
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call9

    ; Now (factorial-alt (- n 1)) has been called. the result are in the val register.
    ; We start building up the argument list for evaluating * from (* n (factorial-alt (- n 1)))
    (assign argl (op list) (reg val))
    (restore env)
    (assign val (op lookup-variable-value) (const n) (reg env))
    (assign argl (op cons) (reg val) (reg argl))

    ; Restoring * to proc register
    (restore proc)
    (restore continue)

    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch14))
    compiled-branch13
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))

    ; Since * is a primitive procedure, we apply it and jump to continue.
    primitive-branch14
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (goto (reg continue))

    after-call12
    after-if3
    after-lambda1

    ; We actually associate the recently createad lambda with the symbol factorial-alt
    ; in the current environment and assign 'ok to val.
    (perform (op define-variable!) (const factorial-alt) (reg val) (reg env))
    (assign val (const ok))

