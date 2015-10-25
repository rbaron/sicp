; Analyzing the delayed evaluation of streams

; Given procedure:
(define (show x)
  (display-line x)
  x)

; Given procedures for testing

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

; Original map
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

; Better map (handles nested streams)
; (define (stream-map proc . argstreams)
;   (if (stream-null? (car argstreams))
;       the-empty-stream
;       (cons-stream
;        (apply proc (map stream-car argstreams))
;        (apply stream-map
;               (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-enumerate-interval a b)
  (if (> a b)
    the-empty-stream
    (cons-stream a
                 (stream-enumerate-interval (+ a 1) b))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

; Print helper
; (define (print-stream stream)
;   (define (print-stream-els stream level)
;     (display level)
;     (if (stream-null? stream)
;       'done
;       (if (pair? (stream-car stream))
;         (begin (newline)
;                (print-stream-els (stream-car stream) (string-append level " "))
;                (print-stream-els (stream-cdr stream) level))
;
;         (begin (display (stream-car stream))
;                (display " ")
;                (print-stream-els (stream-cdr stream) level)))))
;   (newline)
;   (display "Stream values: ")
;   (print-stream-els stream ""))

; END OF given procedures for testing

; Let's execute a few given test cases:

(define x (stream-map show (stream-enumerate-interval 0 10)))

(stream-ref x 5)
; => 1
; => 2
; => 3
; => 4
; => 5
; => Value: 5

(stream-ref x 7)
; => 6
; => 7
; => Value: 7

; That's interesting! The values from the first call to the stream
; were cached! This means `show` won't be rerun. Its return values
; will only be looked up. That's why the `display` on the body of
; `show` only runs once for each input!
