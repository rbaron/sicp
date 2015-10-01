; Generic term list implementation.

; We want to be able to represent polynomials term lists in
; two fashions:

; 1. Efficient for dense polys, using a simple list of coefficients
; 2. Efficient for sparse polys, using a list of (order, coefficient) tuples.

; Strategy: add type-tags for `term-list`. Dispatch the corresponding
; procedures (previously installed on our op-type table) with `apply-generic`.

; Dense poly term list implementation
(define (install-dense-poly-package)
  ;; Internal procedures
  (define (adjoin-term-dense term term-list)
    ; Same procedure from book
  )

  (define (first-term-dense term-list)
    ; Same procedure from book
  )

  ;;
  ;; ... All other dense poly procedures ...
  ;;

  (define (tag content)
    (attach-tag 'dense-poly content))

  ; Now we will also want to tag our term lists, so we
  ; will add a type tag to the empty term list, wich will
  ; work as our constructor
  (define (the-empty-term-list-dense)
    (tag '()))

  ;; Install procedures
  (put 'adjoin-term 'dense-poly adjoin-term-dense)

  (put 'first-term 'dense-poly first-term-dense)

  (put 'the-empty-list 'dense-poly the-empty-term-list-dense)

'done)


; Sparse poly term list implementation
(define (install-sparse-poly-package)
  ;; Internal procedures
  (define (adjoin-term-sparse term term-list)
    ;; Our implementation from 2.89
  )

  (define (first-term-dense term-list)
    ;; Our implementation from 2.89
  )

  ;;
  ;; ... All other sparse poly procedures ...
  ;;

  (define (tag content)
    (attach-tag 'sparse-poly content))

  ; Now we will also want to tag our term lists, so we
  ; will add a type tag to the empty term list, wich will
  ; work as our constructor
  (define (the-empty-term-list-sparse)
    (tag '()))

  ;; Install procedures
  (put 'adjoin-term 'sparse-poly adjoin-term-sparse)

  (put 'first-term 'sparse-poly first-term-sparse)

  (put 'the-empty-list 'sparse-poly the-empty-term-list-sparse)

'done)


; We can now expose our constructors inside our 'poly' package
(define (install-poly-package)
  ;...

  (define (the-empty-term-list-dense)
    ((get 'the-empty-list 'dense-poly)))

  (define (the-empty-term-list-sparse)
    ((get 'the-empty-list 'sparse-poly)))

  ; We could go even further and use our implementations
  ; in a transparent way inside this poly package. For instance:

  (define (first-term term-list)
    (apply-generic 'first-term term-list))

  ; This one is a bit trickier, since we can't use `apply-generic`, since
  ; we haven't bothered type tagging the `term` argument
  (define (adjoin-term term term-list)
    ((get 'adjoin-term (type-tag term-list)) term term-list))

'done)

