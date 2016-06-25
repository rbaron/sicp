; Enforcing that operations run only on registers and constants

; Load this file and drop yourself in the REPL with:
; $ cat 5.9.scm - | mit-scheme

(load "book-code/ch5-regsim.scm")

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (if (or (constant-exp? e) (register-exp? e))
                  (make-primitive-exp e machine labels)
                  (error "Invalid operand -- MAKE-OPERATION-EXP" e)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define test-machine
  (make-machine
    '(a)
    (list (list '- -))
    '(controller
      here
        (assign a (op -) (const 1) (label here))
      )))

(start test-machine)
; => ;Invalid operand -- MAKE-OPERATION-EXP (label here)

(define test-machine-2
  (make-machine
    '(a)
    (list (list '- -))
    '(controller
      here
        (assign a (op -) (const 1) (const 2))
      )))

(start test-machine-2)
(display (get-register-contents test-machine-2 'a))
; => -1
