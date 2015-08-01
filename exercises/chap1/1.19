; Usind the definition of the transformation `T(a, b) => (a', b')`, we can
; find `T(T(a, b)) = (a'', b'')` by applytion `T` to the results of a previous
; call to `T`, as follows:

; b'' = b(p^2 + q^2) + a(q^2 + 2pq) = bp' + aq'

; With the values of `p'` and `q'`, we have defined `T(T(a, b)) = (a'', b'')`.
; Let's use those values to fill out the given skeleton for `fib-iter`:

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
          (fib-iter a
                    b
                    (+ (* p p ) (* q q)) ; compute p'
                    (+ (* q q) (* 2 p q)) ; compute q'
                    (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; Tests
(fib 0)
; => 0

(fib 1)
; => 1

(fib 5)
; => 5

(fib 6)
; => 8
