; Making sure each label has a unique name

; Load this file and drop yourself in the REPL with:
; $ cat 5.8.scm - | mit-scheme

(load "book-code/ch5-regsim.scm")

; What's the value of register a when there is reached?

; start
;   (goto (label here))
; here
;   (assign a (const 3))
;   (goto (label there))
; here
;   (assign a (const 4))
;   (goto (label there))
; there

; Right now, when a label is looked up in lookup-label, the first
; match will be used. This means the first here label will be jumped to
; and thus register a will hold the value 3. The rationale behind it is:

(assoc 'q (list (cons 'q 1) (cons 'q 2)))
; => Value 14: (q . 1)

; Testing

(define amb-machine
  (make-machine
    '(a)
    '()
    '(controller
      start
        (goto (label here))
      here
        (assign a (const 3))
        (goto (label there))
      here
        (assign a (const 4))
        (goto (label there))
      there)))

(start amb-machine)
(display (get-register-contents amb-machine 'a))
; => 3

; Let's fix this so that having multiple labels with the same name will yield
; an error:

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                        (if (assoc next-inst labels)
                          (error "Duplicated label -- EXTRACT-LABELS" next-inst)
                          (cons (make-label-entry next-inst
                                                  insts)
                                labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

(define amb-machine
  (make-machine
    '(a)
    '()
    '(controller
      start
        (goto (label here))
      here
        (assign a (const 3))
        (goto (label there))
      here
        (assign a (const 4))
        (goto (label there))
      there)))

; => Duplicated label -- EXTRACT-LABELS here
