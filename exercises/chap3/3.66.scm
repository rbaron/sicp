; What is the order pairs are produce on the `pairs` procedure?

; The prodecure is given by:

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))


; In order to have a better look at what we're up against,
; I find it useful to draw the `pairs` procedure as a tree
; representing the ordering on the stream. I have drawn
; this tree and deduced the indexing for elements of the
; proposed stream on the figure 3.66.jpg.

; Using the logic deduced on 3.66.jpg, we can calculate
; the mentioned indices (in addition to some I proposed):

; #(1 100) = 197
; #(10 10) = -2*(1-2^9) = 1022
; #(10 11) = #(10 10) + 2^9 = 1534
; #(10 20) = #(10 11) + (20 - 11)*2^10 = 10750
; #(99 99) = -2*(1-2^98) = 6.338253e+29 (Dang it!)
; #(99 100) = #(99 99) + 2^98 = 9.5073795e+29 (Dang it #2!)
; #(100 100) = -2*(1-2^99) = 1.2676506e+30 (Dang it #3!)

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

(define (stream-count-until stream element)
  (if (equal? (stream-car stream) element)
    0
    (+ 1 (stream-count-until (stream-cdr stream) element))))

(stream-count-until p (list 1 100))
; => 197

(stream-count-until p (list 10 10))
; => 1022

(stream-count-until p (list 10 11))
; => 1534

(stream-count-until p (list 10 20))
; => 10750

; Great! Our pattern deduction seems to have worked. Let's try
; it for some of the bigger numbers...

(stream-count-until p (list 99 99))
; => Maximum depth reached :( Let's try to come up with a
; iterative way of computing indices on streams to use instead
; of our recursive one:

(define (stream-count-until-it stream element)
  (define (inner stream element accumulator)
    (if (equal? (stream-car stream) element)
      accumulator
      (inner (stream-cdr stream) element (+ accumulator 1))))
  (inner stream element 0))

; (stream-count-until-it p (list 99 99))
; => Aborting!: out of memory :(

; I guess we can't even visit the stream up to this element. If only there
; was a way of not allocating memory for visited values... ;P This is
; one of the downsides of the stream memoization strategy right here.

; Unfortunatelly we won't be able to calculate for sure the indices of
; (99 100) and (100 100), but if we trust the deduction (which seems
; to be correct), we can expect they to be around 9e29 and 1e30.
