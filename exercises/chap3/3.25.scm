; Arbitrarily multi-dimensional tables

; Let's try and generalize our table implementation
; so we're able to insert and lookup values with an
; arbitrary number of keys.

(define (make-table)
  (let ((local-table (list '*table*)))

    (define (assoc key records)
      (cond ((null? records) #f)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (lookup keys)
      (let ((record (assoc (car keys) (cdr local-table))))
        (if record
            (if (null? (cdr keys))
              (cdr record)
              (((cdr record) 'lookup-proc) (cdr keys)))
            #f)))

    (define (insert! value keys)
      (if (null? (cdr keys))
        ;; Lookup and insert new value at this table at
        ;; `(car keys)`
        (let ((record (assoc (car keys) (cdr local-table))))
          (if record
              (set-cdr! record value)
              (set-cdr! local-table
                        (cons (cons (car keys) value)
                              (cdr local-table)))))

        ;; Create a new table and store it at this
        ;; table under `(car keys)`
        (let ((subtable (assoc (car keys) (cdr local-table))))
          (if subtable
            ((subtable 'insert-proc!) value (cdr keys))
            (let ((new-subtable (make-table)))
              (set-cdr! local-table
                        (cons (cons (car keys) new-subtable)
                              (cdr local-table)))
              ((new-subtable 'insert-proc!) value (cdr keys))))))

      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))


; Exposing procedures with variable number of keys

(define (insert! table value . keys)
  ((table 'insert-proc!) value keys))

(define (lookup table . keys)
  ((table 'lookup-proc) keys))


; Testing

(define t1 (make-table))

(insert! t1 "Top level value" 'top-level-key)

(lookup t1 'top-level-key)
; => "Top level value"

(insert! t1 "Second level value" 'second-level-key1 'second-level-key2)

(lookup t1 'second-level-key1 'second-level-key2)
; => "Second level value"

(insert! t1 "Third level value" 0 0 0)

(lookup t1 0 0 0)
; => "Third level value"

