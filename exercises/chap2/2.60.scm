; Alternative set representation with unordered lists:
; Now we're gonna allow the lists representing a set to have
; duplicate entries. Let's modify our predicates and selectors
; to handle that.

; `element-of-set?` stays the same:
(define (element-of-set? x set)
  (if (null? set)
    #f
    (if (equal? x (car set))
      #t
      (element-of-set? x (cdr set)))))

; `adjoin-set` is more efficient, since we can immediately insert
; new elements without worrying whether they're already present or not:
(define (adjoin-set x set)
  (cons x set))

; Union can also leverage from the allowance of duplicates:
(define (union s1 s2)
  (append s1 s2))

; Intersection stays the same:
(define (intersection-set s1 s2)
  (if (null? s1)
    '()
    (if (element-of-set? (car s1) s2)
      (cons (car s1)
            (intersection-set (cdr s1) s2))
      (intersection-set (cdr s1) s2))))

; The `adjoin-set` operation is clearly faster. It runs in constant time.
; `union-set` also has had its performance recuded from O(n^2) to O(n),
; since the `append` operation has to find the end of `s1`.

; The downside is that now we have a space penalty, since we are storing
; duplicate information.

; This might be a useful implementation for systems where the adjoin and
; union operations are used often and we don't care too much about space.
