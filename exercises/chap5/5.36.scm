; Order of evaluation

; Load this file
; $ cat 5.36.scm - | mit-scheme

(load "book-code/load-eceval-compiler.scm")
(load "book-code/ch5-compiler.scm")

; Helper procedures
(define (get-instructions compiled-code)
  (caddr compiled-code))

; Procedure to append one element at the end of the list. O(n).
(define (cons-end item lst)
  (append lst (list item)))

(define (simulate instructions)
  (let ((test-machine (make-machine all-regs
                                    (cons (list 'cons-end cons-end) eceval-operations)
                                    instructions)))
    (set-register-contents! test-machine 'env (setup-environment))
    (start test-machine)
    test-machine))

(define (pprint-instrs instrs)
  (if (null? instrs)
    'done
    (begin (newline)
           (display (car instrs))
           (pprint-instrs (cdr instrs)))))

; Our compiler produces code that evaluates application arguments
; from right to left. This order is determined by construct-arglist:

; (define (construct-arglist operand-codes)
;   (let ((operand-codes (reverse operand-codes)))   ; note the call to reverse
;     (if (null? operand-codes)
;         (make-instruction-sequence '() '(argl)
;          '((assign argl (const ()))))
;         (let ((code-to-get-last-arg
;                (append-instruction-sequences
;                 (car operand-codes)
;                 (make-instruction-sequence '(val) '(argl)
;                  '((assign argl (op list) (reg val)))))))
;           (if (null? (cdr operand-codes))
;               code-to-get-last-arg
;               (preserving '(env)
;                code-to-get-last-arg
;                (code-to-get-rest-args
;                 (cdr operand-codes))))))))

; As we can see, the operand codes are reversed (through a call to reverse)
; before being appended together to form a sequence. The reason for that is
; that by evaluating them from right to first, we can simply call `cons` to
; accumulate the arguments in the correct order in the argl register.

; Example:
(define compiled-code
  (compile
    '(+ 1 2)
    'val
    'next))

(define instructions (get-instructions compiled-code))

(pprint-instrs instructions)
; =>    (assign proc (op lookup-variable-value) (const +) (reg env))
; =>    (assign val (const 2))   ; => rightmost argument is evaluated first
; =>    (assign argl (op list) (reg val))
; =>    (assign val (const 1))
; =>    (assign argl (op cons) (reg val) (reg argl))
; =>    (test (op primitive-procedure?) (reg proc))
; =>    (branch (label primitive-branch6))
; =>    compiled-branch5
; =>    (assign continue (label after-call4))
; =>    (assign val (op compiled-procedure-entry) (reg proc))
; =>    (goto (reg val))
; =>    primitive-branch6
; =>    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
; =>    after-call4

(define machine (simulate instructions))

(get-register-contents machine 'val)
; => 3

; Changing the order of evaluation is as simple as removing the call
; to rever in construct-arglist:
(define (construct-arglist operand-codes)
  (if (null? operand-codes)
      (make-instruction-sequence '() '(argl)
       '((assign argl (const ()))))
      (let ((code-to-get-last-arg
             (append-instruction-sequences
              (car operand-codes)
              (make-instruction-sequence '(val) '(argl)
               '((assign argl (op list) (reg val)))))))
        (if (null? (cdr operand-codes))
            code-to-get-last-arg
            (preserving '(env)
             code-to-get-last-arg
             (code-to-get-rest-args
              (cdr operand-codes)))))))

; And making sure the accumulation of arguments into argl now adds
; new elements to the end of argl:
(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence '(val argl) '(argl)
           '((assign argl (op cons-end) (reg val) (reg argl)))))))   ; changed cons to cons-end
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

; Testing

(define compiled-code
  (compile
    '(+ 1 2)
    'val
    'next))

(define instructions (get-instructions compiled-code))

(pprint-instrs instructions)
; =>    (assign proc (op lookup-variable-value) (const +) (reg env))
; =>    (assign val (const 1))
; =>    (assign argl (op list) (reg val))
; =>    (assign val (const 2))
; =>    (assign argl (op cons-end) (reg val) (reg argl))
; =>    (test (op primitive-procedure?) (reg proc))
; =>    (branch (label primitive-branch6))
; =>    compiled-branch5
; =>    (assign continue (label after-call4))
; =>    (assign val (op compiled-procedure-entry) (reg proc))
; =>    (goto (reg val))
; =>    primitive-branch6
; =>    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
; =>    after-call4

(define machine (simulate instructions))
(get-register-contents machine 'val)
; => 3

; Using cons-end instead of cons adds complexity to the runtime. Adding elements
; to the end of lists take O(n) while adding elements to the head take O(1).
