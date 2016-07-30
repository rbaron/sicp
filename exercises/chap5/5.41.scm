; Implementing find-variable in compile time environments

; Load this file
; $ cat 5.41.scm - | mit-scheme

(define (find-variable var compile-time-env)
  (define (search-frame current-env frame-counter)
    (if (null? current-env)
      'not-found
      (let ((index-in-frame (index-of var (car current-env))))
        (if (eq? index-in-frame 'not-found)
          (search-frame (cdr current-env) (+ frame-counter 1))
          (list frame-counter index-in-frame)))))
  (search-frame compile-time-env 0))

(define (index-of elem lst)
  (define (inner counter current-lst)
    (cond ((null? current-lst) 'not-found)
          ((eq? (car current-lst) elem) counter)
          (else (inner (+ counter 1) (cdr current-lst)))))
  (inner 0 lst))

; Testing

(index-of 'x '(1 2 x b d))
; => 2
(index-of 'x '(1 2 b d))
; => not-found

(find-variable 'c '((y z) (a b c d e) (x y)))
; => (1 2)

(find-variable 'x '((y z) (a b c d e) (x y)))
; => (2 0)

(find-variable 'w '((y z) (a b c d e) (x y)))
; => not-found
