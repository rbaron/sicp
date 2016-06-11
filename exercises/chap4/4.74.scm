; Alternate flatmap implementation

; Load this file and drop yourself in the REPL with:
; $ cat 4.74.scm - | mit-scheme

; Alyssa suggests that the stream-flatmap operation could be simplified
; in procedures that produce either a singleton stream or the empty stream.

; She proposes:
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

;(define (simple-flatten stream)
;  (stream-map <??>
;              (stream-filter <??> stream)))

; a. We can complete her definition with:
(define (simple-flatten stream)
  (stream-map car-stream
              (stream-filter (lambda (s) (not (stream-null? s)))
                              stream)))

; b. Does the system behavior change with this alteration?

; => No. As long we use simple-stream-flatmap only in cases
;    where the produced output is a stream of singleton
;    streams or empty streams, the behavior should be the
;    same.

; Original stream-flatmap for reference:

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed (force delayed-s2)
                           (delay (stream-cdr s1))))))
