; Parametrized equality for table keys

; As the the exercise points out, using `equal?` to test equality
; of keys isn't always the optimal choice. Let's add a parameter
; `same-key?` to `make-table` so we can use custom procedures
; for that.

(define (make-table same-key?)
  (let ((local-table (list '*table*)))

    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))

; Exposing procedures
(define (insert! table key1 key2 value)
  ((table 'insert-proc!) key1 key2 value))

(define (lookup table key1 key2)
  ((table 'lookup-proc) key1 key2))

; Testing

; Let's pretend our keys are floating point numbers and we
; want them to be "equal" if they fall into a tolerance range:
(define (custom-equal? n1 n2)
  (< (abs (- n1 n2)) .1))

(define t1 (make-table custom-equal?))

(insert! t1 1.0 1.0 "Hello, world")

(insert! t1 1.05 1.05 "Goodbye, world")

(lookup t1 1.0 1.0)
; => "Goodbye, world"

; This means the keys "1.0" and "1.05" were considered to be the
; same under our `custom-equal?` procedure.
