; RCPL - Read Compile Print Loop

; Load this file and drop yourself in the REPL with
; $ cat 5.49.scm - | mit-scheme

(load "book-code/load-eceval-compiler.scm")
(load "book-code/ch5-compiler.scm")

; The idea is to modify the explicit control evaluator in order to
; compile every expression instead of dispatching to the code section
; that evaluates it.

; The evaluator itself is greatly simplified, since it now delegates all
; the work to the compile-and-run primitive. The evaluator's dispatch code
; section' only entry is the one that branches to the ev-compile-and-run.

; ev-compile-and-run, in its turn, simple calls the compile-and-run primitive,
; which compiles the expression in the exp register using target val and
; linkage return.


(define (compile-and-run? exp)
  (tagged-list? exp 'compile-and-run))

(define (compile-and-run exp)
  (let ((instructions
         (assemble (statements
                    (compile exp 'val 'return))
                   eceval)))
    instructions))

; Installing the new primitives
(define eceval-operations
  (cons (list 'compile-and-run compile-and-run)
    (cons (list 'compile-and-run? compile-and-run?)
         eceval-operations)))

; Mofifying the evaluator. As we can see, a _lot_ of code could be removed.
(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
  '(

read-eval-print-loop
  (perform (op initialize-stack))
  (perform
   (op prompt-for-input) (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label eval-dispatch))
print-result
  (perform (op print-stack-statistics))
  (perform
   (op announce-output) (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

eval-dispatch
  ;; Added
  (test (op compile-and-run?) (reg exp))
  (branch (label ev-compile-and-run))

; Added
ev-compile-and-run
  (assign val (op compile-and-run) (reg exp))
  (assign continue (label print-result))

  ; Jump to newly-compiled code's entry point
  (goto (reg val))
   )))

; Kickoff the interpreter
(set! the-global-environment (setup-environment))
(set-register-contents! eceval 'flag false)
(start eceval)

; Testing

(+ 1 2)
; => 3

(define (inc x)
  (+ x 1))
; => ok

(inc 6)
; => 7

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))
; => ok

(factorial 5)
; => 120
