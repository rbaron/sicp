; Implementing append and append! using the register machine language

; Load this file and drop yourself in the REPL with:
; $ cat 5.22.scm - | mit-scheme

(load "book-code/ch5-regsim.scm")

; a. append

(define (append x y)
  (if (null? x)
    y
    (cons (car x) (append (cdr x) y))))

(define append-machine
  (make-machine
    '(continue x y tmp)
    (list (list 'cons cons) (list 'car car)
          (list 'cdr cdr) (list 'null? null?))
    '(controller
        (assign continue (label done))

      append
        (test (op null?) (reg x))
        (branch (label x-null))

        ; Prepare recursion
        (save continue)
        (save x)
        (assign continue (label after-append))
        (assign x (op cdr) (reg x))
        (goto (label append))

      after-append
        (restore tmp) ; tmp now holds the original x
        (assign tmp (op car) (reg tmp))
        (restore continue)
        (assign y (op cons) (reg tmp) (reg y)) ; resulting cons is stored in y
        (goto (label append))

      x-null
        (goto (reg continue))

      done)))

(set-register-contents! append-machine 'x '(1 2 3))
(set-register-contents! append-machine 'y '(4 5 6))
(start append-machine)
(get-register-contents append-machine 'x)
; => ()

(get-register-contents append-machine 'y)
; => (1 2 3 4 5 6)


; b. append!

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define append!-machine
  (make-machine
    '(x y tmp)
    (list (list 'cons cons) (list 'cdr cdr)
          (list 'set-cdr! set-cdr!) (list 'null? null?))
    '(controller
        (save x) ; save original x so we can return it after setting its cdr

      last-pair
        (assign tmp (op cdr) (reg x))
        (test (op null?) (reg tmp))
        (branch (label set-cdr!))

        ; Prepare recursion
        (assign x (op cdr) (reg x))
        (goto (label last-pair))

      set-cdr!
        (perform (op set-cdr!) (reg x) (reg y))
        (restore x))))

(set-register-contents! append!-machine 'x '(1 2 3))
(set-register-contents! append!-machine 'y '(4 5 6))
(start append!-machine)
(get-register-contents append!-machine 'x)
; => (1 2 3 4 5 6)

(get-register-contents append!-machine 'y)
; => (4 5 6)
