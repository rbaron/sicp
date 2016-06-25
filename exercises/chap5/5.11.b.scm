; Investigating alternative behaviors for push/pop operations

; Load this file and drop yourself in the REPL with:
; $ cat 5.11.b.scm - | mit-scheme

(load "book-code/ch5-regsim.scm")

; b. (restore y) puts into y the last value saved on the stack, but only if that
; value was saved from y; otherwise, it signals an error.

; Let's modify the save operation in order to store the name of the register
; whose value was pushed:

(define (make-save inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (lambda ()
        (push stack (cons reg-name (get-contents reg)))
        (advance-pc pc)))))

(define (make-restore inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (lambda ()
        (let ((name-value (pop stack)))
          (if (eq? (car name-value) reg-name)
            (begin
              (set-contents! reg (cdr name-value))
              (advance-pc pc))
            (error "Cannot restore to different register -- RESTORE")))))))

(define b-test-machine-ok
  (make-machine
    '(a b)
    '()
    '(controller
      (assign a (const 1))
      (save a)
      (restore a))))

(start b-test-machine-ok)
(display (get-register-contents b-test-machine-ok 'a))
;  => 1

(define b-test-machine-err
  (make-machine
    '(a b)
    '()
    '(controller
      (assign a (const 1))
      (save a)
      (restore b))))

(start b-test-machine-err)
; => Cannot restore to different register -- RESTORE
