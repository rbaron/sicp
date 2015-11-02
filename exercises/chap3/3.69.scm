; An infinite stream of triples (s t u) such that s <= t <= u

; Let's follow the logic devised for creating an infinite
; stream of pairs (s t) such that s <= t. Intuitively, for
; integers, it worked like:

; Start with (s t). Generate all pairs (s t+k) for k=1, 2, 3, ....
; Recurse on (s+1 t+1).

; Let's recall the implementation of `pairs`:

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (e) (list (stream-car s) e))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

; For triples, all we need to do is to add another dimension
; in which the stream can vary. Let's try it:

(define (triples s t u)
  (cons-stream

    ; (1 1 1)
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave

      ; (1 1 2), (1 1 3), ...
      (stream-map (lambda (e) (list (stream-car s) (stream-car t) e))
                  (stream-cdr u))

      (interleave
        ; (1 2 2) (recurse)
        (triples s (stream-cdr t) (stream-cdr u))

        ; (2 2 2) (recurse)
        (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))


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

(display-inf-stream (triples integers integers integers) 20)
; =>
; (1 1 1)
; (1 1 2)
; (1 2 2)
; (1 1 3)
; (2 2 2)
; (1 1 4)
; (1 2 3)
; (1 1 5)
; (2 2 3)
; (1 1 6)
; (1 3 3)
; (1 1 7)
; (2 3 3)   *
; (1 1 8)
; (1 2 4)
; (1 1 9)
; (2 2 4)
; (1 1 10)
; (2 3 3)   *
; (1 1 11)

; Wait... there is a problem here. Some entries are duplicated. (2 3 3)
; for instance appears twice. The procedure `triples` is producing re-
; peated states. A better approach to recurse only on the "most signi-
; ficant" place and generate all pairs for the two least significant
; places using the `pairs` procedure:


(define (triples s t u)
  (cons-stream

    ; (1 1 1)
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave

      ; (1 1 2), (1 1 3), (1 2 2), (1 2 3)...
      (stream-cdr (stream-map (lambda (pair) (cons (stream-car s) pair))
                  (pairs t u)))

      ; (2 2 2) (recurse)
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(display-inf-stream (triples integers integers integers) 20)
; =>
; (1 1 1)
; (1 1 2)
; (2 2 2)
; (1 2 2)
; (2 2 3)
; (1 1 3)
; (3 3 3)
; (1 2 3)
; (2 3 3)
; (1 1 4)
; (3 3 4)
; (1 3 3)
; (2 2 4)
; (1 1 5)
; (4 4 4)
; (1 2 4)
; (2 3 4)
; (1 1 6)
; (3 4 4)
; (1 3 4)

; Let's now filter for pythagorean triplets:

(define (py-triplet? a b c)
  (= (+ (square a) (square b))
     (square c)))


(define py-stream
  (stream-filter
    (lambda (lst) (apply py-triplet? lst))
    (triples integers integers integers)))


(display-inf-stream py-stream 6)
; =>
; (3 4 5)
; (6 8 10)
; (5 12 13)
; (9 12 15)
; (8 15 17)
; (12 16 20)
