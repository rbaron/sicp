; Completing the `steam-map` implementation

; Let's complete this given skeleton:

; (define (stream-map proc . argstreams)
;   (if (<??> (car argstreams))
;       the-empty-stream
;       (<??>
;        (apply proc (map <??> argstreams))
;        (apply stream-map
;               (cons proc (map <??> argstreams))))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

; Copied code

(define (force delayed-object)
  (delayed-object))

(define (delay exp)
  (lambda () exp))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-cons a b) (cons a (delay b)))

(define (stream-null? a) (null? a))

; END OF copied code

; Print helper
(define (print-stream stream)
  (define (print-stream-els stream level)
    (display level)
    (if (stream-null? stream)
      'done
      (if (pair? (stream-car stream))
        (begin (newline)
               (print-stream-els (stream-car stream) (string-append level " "))
               (print-stream-els (stream-cdr stream) level))

        (begin (display (stream-car stream))
               (display " ")
               (print-stream-els (stream-cdr stream) level)))))
  (newline)
  (display "Stream values: ")
  (print-stream-els stream ""))


; Let's test this out:

(define stream1
  (stream-cons 1
    (stream-cons 2
      (stream-cons 3
        the-empty-stream))))

(print-stream stream1)
; => Stream values: 1 2 3

(define stream1-mapped
  (stream-map (lambda (e) (+ e 1)) stream1))

(print-stream stream1-mapped)
; => Stream values: 2 3 4

; A more complex case: streams of streams

(define stream2
  (stream-cons (stream-cons 11 (stream-cons 12 the-empty-stream))
    (stream-cons (stream-cons 21 (stream-cons 22 the-empty-stream))
      (stream-cons (stream-cons 31 (stream-cons 32 the-empty-stream))
        the-empty-stream))))

(print-stream stream2)
; => Stream values:
; 11  12
; 21  22
; 31  32

(define stream2-mapped
  (stream-map stream-car stream2))

(print-stream stream2-mapped)
; => Stream values: 11 21 31
