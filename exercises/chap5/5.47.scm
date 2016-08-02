; Measuring the performance of the compiled factorial procedure

; Load this file and drop yourself in the REPL with
; $ cat 5.46.scm - | mit-scheme

(load "book-code/load-eceval-compiler.scm")
(load "book-code/ch5-compiler.scm")

(define (get-instructions compiled-code)
  (caddr compiled-code))

(define (pprint-instrs instrs)
  (if (null? instrs)
    'done
    (begin (newline)
           (display (car instrs))
           (pprint-instrs (cdr instrs)))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))

        ; Added
        (compound-branch (make-label 'compound-branch))
        (after-call (make-label 'after-call)))
    (let* ((compiled-linkage
            (if (eq? linkage 'next) after-call linkage))

           ; Added. compound-linkage is the same as compiled-linkage
           (compound-linkage compiled-linkage))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))
          (test (op compound-procedure?) (reg proc))
          (branch (label ,compound-branch))))

        ; Added
        (parallel-instruction-sequences
          (append-instruction-sequences
            compound-branch
            (compile-compound-appl target compound-linkage))
         (parallel-instruction-sequences
          (append-instruction-sequences
           compiled-branch
           (compile-proc-appl target compiled-linkage))
          (append-instruction-sequences
           primitive-branch
           (end-with-linkage linkage
            (make-instruction-sequence '(proc argl)
                                       (list target)
             `((assign ,target
                       (op apply-primitive-procedure)
                       (reg proc)
                       (reg argl))))))))
       after-call))))

; compile-compound-appl is a modified verson of compile-proc-appl
(define (compile-compound-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           ; Changed from
           ;`((assign continue (label ,linkage))
           ;  (assign val (op compiled-procedure-entry) (reg proc))
           ;  (goto (reg val)))))

           ; To
           `((assign continue (label ,linkage))
             ; Note that we have to save the continue register, since
             ; the code under compound-apply label  does not do it for us
             (save continue)
             (goto (reg compapp)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            ; Changed from
            ;`((assign continue (label ,proc-return))
            ;  (assign val (op compiled-procedure-entry) (reg proc))
            ;  (goto (reg val))
            ;  ,proc-return
            ;  (assign ,target (reg val))
            ;  (goto (label ,linkage))))))

            ; To
            `((assign continue (label ,proc-return))
                ; Again, saving continue before jumping to compound-apply
                (save continue)
                (goto (reg compapp))
                ,proc-return
                (assign ,target (reg val))
                (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs

          ; Changed from
          ;'((assign val (op compiled-procedure-entry) (reg proc))
          ;  (goto (reg val)))))

          ; To
          '((save continue)
            (goto (reg compapp)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

; Testing. Let's define the more intricate case of mixing compiled, primitive
; and interpreted code. Jesus. Here it is:
(define code
  '(define (compiled-f n)
    (+ 1 (interpreted-g n))))


; Uncomment for debugging the compiled instructions
;(pprint-instrs (get-instructions (compile code 'val 'next)))

; f is a compiled procedure, while g is an interpreted procedure
(compile-and-go code)

(define (interpreted-g n)
  (+ n 1))

(compiled-f 1)
; => 3
