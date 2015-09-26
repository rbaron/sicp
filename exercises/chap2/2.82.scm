; Generalizing type coercion for multiple arguments

; Let's modify `apply` generic to deal with arbitrary number of arguments.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((coerced (make-first-valid-coercion args type-tags)))
      (if (= coerced #f)
         (error "No coercion found for every type")
        (let ((proc (get op (map type-tag coerced))))
          (if proc
              (apply proc (map contents coerced))
              (error "No method for these types")))))))


(define (coerce-datum datum type)
  (if (= (type-tag datum) type)
    datum
    (let ((d->t (get-coercion (type-tag datum) type)))
      (if (d->t)
        (attach-type-tag type (d->t (content datum)))
        #f))))


; Were there valid coercions for every argument in lst?
(define (success-coercion? lst)
  (cond ((null? lst) #t)
        (= (car lst) #f) #f
        (else (success-coercion? (cdr lst)))))


(define (make-first-valid-coercion args type-tags)
  (if (null? type-tagss)
    #f
    (let ((coerced (map (lambda (arg) (coerce-datum arg (car types))))))
      (if (success-coercion? coerced)
        coerced
        (make-first-valid-coercion args (cdr types))))))
