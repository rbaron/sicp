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
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

; Copied code

;(define (force delayed-object)
;  (delayed-object))
;
;(define (delay exp)
;  (lambda () exp))
;
;(define (stream-car stream) (car stream))
;
;(define (stream-cdr stream) (force (cdr stream)))

;(define (cons-stream a b) (cons a (delay b)))

(define (stream-null? a) (null? a))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

; Basic map
; (define (stream-map proc s)
;   (if (stream-null? s)
;       the-empty-stream
;       (cons-stream (proc (stream-car s))
;                    (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

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
  (cons-stream 1
    (cons-stream 2
      (cons-stream 3
        the-empty-stream))))

(print-stream stream1)
; => Stream values: 1 2 3

(define stream1-mapped
  (stream-map (lambda (e) (+ e 1)) stream1))

(print-stream stream1-mapped)
; => Stream values: 2 3 4

; A more complex case: streams of streams

(define stream2
  (cons-stream (cons-stream 11 (cons-stream 12 the-empty-stream))
    (cons-stream (cons-stream 21 (cons-stream 22 the-empty-stream))
      (cons-stream (cons-stream 31 (cons-stream 32 the-empty-stream))
        the-empty-stream))))

stream2
(print-stream stream2)
; => Stream values:
; 11  12
; 21  22
; 31  32

(define stream2-mapped
  (stream-map stream-car stream2))

(print-stream stream2-mapped)
; => Stream values: 11 21 31
