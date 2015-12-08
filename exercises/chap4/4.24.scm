(define (time-proc proc)
  (let ((t0 (get-universal-time)))
    (proc)
    (define t1 (get-universal-time))
    (display "\nProc took ")
    (display (- t1 t0))))

; Let's start benchmarking the unoptimized version of `eval`:
(load "book-code/ch4-mceval.scm")

; Let's extend the `primitive-procedures` with some
; basic integer procedures:
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '= =)
        (list '- -)
        (list '* *)
        ))

(define the-global-environment (setup-environment))

(eval '(define (factorial-iter acc n)
         (if (= n 0) acc
             (factorial-iter (* acc n) (- n 1))))
      the-global-environment)

(eval '(define benchmark-proc
         (lambda () (factorial-iter 1 200000)))
      the-global-environment)

(time-proc (lambda ()
              (eval '(benchmark-proc) the-global-environment)))

; => Proc took 56

; Let's now analyze the optimized version with `analyze`:
(load "book-code/ch4-analyzingmceval.scm")

(eval '(define (factorial-iter acc n)
         (if (= n 0) acc
             (factorial-iter (* acc n) (- n 1))))
      the-global-environment)

(eval '(define benchmark-proc
         (lambda () (factorial-iter 1 200000)))
      the-global-environment)

(time-proc (lambda ()
              (eval '(benchmark-proc) the-global-environment)))

; => Proc took 45

; Roughly speaking, we can conclude that the time spent analyzing
; is (56-45)/56 ~ 19%.
