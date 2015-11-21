; Let's analyze how internal procedures are evaluated

; Example from exercise 4.19:

(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))
; => Premature reference to reserved name: a

; That's weird. Let's change the order of the internal
; defines:

(let ((a 1))
  (define (f x)
    (define a 5)
    (define b (+ a x))
    (+ a b))
  (f 10))
; => 20
; It now works as expected.


; Example from exercise 4.20
(define (f x)
  (letrec ((even2?
            (lambda (n)
              (if (= n 0)
                  true
                  (odd2? (- n 1)))))
           (odd2?
            (lambda (n)
              (if (= n 0)
                  false
                  (even2? (- n 1))))))
    (even2? x)))

(f 10)
; => true

; Using `let` instead of `letrec`:
(define (f x)
  (let ((even2?
            (lambda (n)
              (if (= n 0)
                  true
                  (odd2? (- n 1)))))
        (odd2?
            (lambda (n)
              (if (= n 0)
                  false
                  (even2? (- n 1))))))
    (even2? x)))

(f 10)
; => Unbound variable: odd2?

; Why? Let's desugar it:
(define (f x)
  ((lambda (even2? odd2?)
     (even2? x))
    (lambda (n)
      (if (= n 0)
          true
          (odd2? (- n 1))))
    (lambda (n)
      (if (= n 0)
          false
          (even2? (- n 1))))))
(f 10)
; => Unbound variable: odd2?

; When `(f 10)` is executed, a new environment E1
; is created and `x` is bound to 10. The body of `f` is then
; evaluated inside E1. The body consists of an application.
; The arguments of that application are evaluated to procedures
; whose closing environment is E1.
; When the application itself is finally evaluated, E2 is created
; and those procedure arguments are bound to `even2?` and `odd2?`
; in E2.
; The problem is `even2?` makes a call to `odd2?`, which is _not_
; defined in E1, only E2. The same goes for `odd2?`.

; How `letrec` avoids this problem?
(define (f x)
  (letrec ((even2?
            (lambda (n)
              (if (= n 0)
                  true
                  (odd2? (- n 1)))))
           (odd2?
            (lambda (n)
              (if (= n 0)
                  false
                  (even2? (- n 1))))))
    (even2? x)))

; Is desugared to
(define (f x)
  (let ((even2? '*unassigned*)
        (odd2? '*unassigned*))
      (set! even2?
            (lambda (n)
              (if (= n 0)
                  true
                  (odd2? (- n 1)))))
      (set! odd2?
            (lambda (n)
              (if (= n 0)
                  false
                  (even2? (- n 1)))))
    (even2? x)))

(f 10)
; => true

; And further desugared to:
(define (f x)
  ((lambda (even2? odd2?)
    (set! even2?
          (lambda (n)
            (if (= n 0)
                true
                (odd2? (- n 1)))))
    (set! odd2?
          (lambda (n)
            (if (= n 0)
                false
                (even2? (- n 1)))))
    (even2? x))
      '*unassigned*
      '*unassigned*))

(f 10)
; => true

; `(f 10)` creates E1, binds `x` to 10. The body of `f` is evaluated,
; beginning with the values '*unassigned*, which evaluate to themselves.
; Evaluating the inner lambda creates E2, in which we set the values of
; `even2?` and `odd2?` to procedures, but now these procedures have as
; enclosing environment E2, so everything works!
