; Successive raising in `apply-generic`.


(define (get-index type)
  (cond ((= type 'integer) 1)
        ((= type 'rational) 2)
        ((= type 'real) 3)
        ((= type 'complex) 4)
        (else (error "Element not in list --" el))))


(define (get-super-sub a b)
  (let ((ia (get-index (type-tag a)))
        (ib (get-index (type-tag b))))
    (if (> ia ib)
      (cons a b)
      (cons b a))))


(define (raise-until datum type)
  (if (= (type-tag datum) type)
    datum
    (raise-until (apply-generic 'raise datum) type)))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((super-sub (get-super-sub (car args) (cdr args))))
                (let ((super (car super-sub))
                      (sub (cdr super-sub)))
                  (if (= (type-tag super) (type-tag sub))
                    (error "There is no such procedure for this type --" type1)
                    (apply-generic op super (raise-until sub (type-tag super))))))
              (error "No method for these types --" (list op type-tags)))))))
