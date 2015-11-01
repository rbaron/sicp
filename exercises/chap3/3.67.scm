; Producing _all_ pairs by dropping the i <= j requirement

; Let's try to reason about this in the same way used
; by the authors for the previous case. Now we have
; the following table:

; (s0 t0) | (s0 t1) (s0 t2) ...
; --------|-----------------------
; (s1 t0) | (s1 t1) | ...
; (s2 t0) |---------|-------------
;   ...   |   ...   | (s2 t2) | ...

; Comparing with the earlier table, which was:

; (s0 t0) | (s0 t1) (s0 t2) ...
; --------|----------------------
;         | (s1 t1) | ...
;         |---------|------------
;         |         | (s2 t2) | ...

; We can see that now we have the "columns" below
; (si ti) also "growing downwards". Let's try to interleave
; that stream with the other two as well:

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list x (stream-car s)))
                (stream-cdr s))
     (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t))))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

; ========================================
;    Begin copied code for testing
; ========================================

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (display-inf-stream s n-terms)
  (define (iter s n-terms)
    (if (> n-terms 0)
      (begin (newline)(display (stream-car s))
             (iter (stream-cdr s) (- n-terms 1)))))
  (newline)
  (iter s n-terms))

; ========================================
;    END OF copied code for testing
; ========================================

(define p (pairs integers integers))

(display-inf-stream p 10)
; =>
; (1 1)
; (2 1)
; (1 2)
; (3 1)
; (2 2)
; (4 1)
; (1 3)
; (5 1)
; (3 2)
; (6 1)

; Cool! We can see that now we have, for instance,
; both (3 1) and (1 3)!

