; Implementing register machine for recursive procedures

; Load this file and drop yourself in the REPL with:
; $ cat 5.21.scm - | mit-scheme

(load "book-code/ch5-regsim.scm")

; a. Recursive count-leaves

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(define machine-a
  (make-machine
    '(continue tmp tree val)
    (list (list 'not not) (list 'car car) (list 'cdr cdr)
          (list 'pair? pair?) (list 'null? null?) (list '+ +))
    '(controller
        (assign continue (label done))
      base
        (test (op null?) (reg tree))
        (branch (label base-0))
        (assign tmp (op pair?) (reg tree))
        (test (op not) (reg tmp))
        (branch (label base-1))
        (goto (label left))

      left
        (save continue)
        (assign continue (label right))
        (save tree)
        (assign tree (op car) (reg tree))
        (goto (label base))

      right
        (restore tree)
        (assign continue (label sum))
        (save val)
        (assign tree (op cdr) (reg tree))
        (goto (label base))

      sum
        (restore tmp) ; Restore left val to tmp
        (assign val (op +) (reg val) (reg tmp))
        (restore continue)
        (goto (reg continue))

      base-0
        (assign val (const 0))
        (goto (reg continue))

      base-1
        (assign val (const 1))
        (goto (reg continue))

      done)))

(define tree
  (cons
    (cons
      1
      (cons 1 1))
    (cons 1 1)))

(set-register-contents! machine-a 'tree tree)
(start machine-a)
(get-register-contents machine-a 'val)
; => 5


;b. Recursive count-leaves with explicit counter:

(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n)))))
  (count-iter tree 0))

(define machine-b
  (make-machine
    '(continue tmp tree val)
    (list (list 'not not) (list 'car car) (list 'cdr cdr)
          (list 'pair? pair?) (list 'null? null?) (list '+ +))
    '(controller
        (assign continue (label done))
        (assign val (const 0))
      base
        (test (op null?) (reg tree))
        (branch (label ret-n))
        (assign tmp (op pair?) (reg tree))
        (test (op not) (reg tmp))
        (branch (label ret-n1))

        ; Prepare recursion for inner call
        (save continue)
        (save tree)
        (assign continue (label after-inner))
        (assign tree (op car) (reg tree))
        (goto (label base))

      after-inner
        (restore tree)
        (assign tree (op cdr) (reg tree))

        ; Prepare recursion for outer call
        (assign continue (label after-outer))
        (goto (label base))

      after-outer
        (restore continue)
        (goto (reg continue))

      ret-n
        (goto (reg continue))

      ret-n1
        (assign val (op +) (reg val) (const 1))
        (goto (reg continue))

      done)))

(set-register-contents! machine-b 'tree tree)
(start machine-b)
(get-register-contents machine-b 'val)
; => 5
