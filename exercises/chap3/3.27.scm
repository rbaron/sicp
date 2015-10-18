; Analyzing the memoization of the Fibonacci procedure

; The exercise asks us to reason about the enviromental structure
; created for the computation of:

; (memo-fib 3)

; Given procedures

; (define (fib n)
;   (cond ((= n 0) 0)
;         ((= n 1) 1)
;         (else (+ (fib (- n 1))
;                  (fib (- n 2))))))
;
; (define (memoize f)
;   (let ((table (make-table)))
;     (lambda (x)
;       (let ((previously-computed-result (lookup x table)))
;         (or previously-computed-result
;             (let ((result (f x)))
;               (insert! x result table)
;               result))))))
;
; (define memo-fib
;   (memoize (lambda (n)
;              (cond ((= n 0) 0)
;                    ((= n 1) 1)
;                    (else (+ (memo-fib (- n 1))
;                             (memo-fib (- n 2))))))))

; Evaluating the `define` of `memo-fib`, will cause the execution of:

; (memoize (lambda (n) ...))

; This means, as with any procedure evaluation, a new enviroment E1 will
; be created. The formal parameters of `memoize` will be bound and its
; body will be evaluated. The result of this call is a procedure which
; will be bound to `memo-fib` in the global frame:

; +------------------------+
; |                        |
; |  memoize: ...          |
; |                        |  Global env
; |  memo-fib: +-+         |
; |              |         | <-----------+
; +------------------------+             |
;                |                +------+----------------+
;                v                |  f: (lambda (n) ...)  |
;              +-+-+    +-------> |  table: ...           |
;          +----|I|-----+         +-----------------------+
;          |   +---+                       E1
;          v
;     parameters: x
;     body: (let previously...)


; Once we have our initial structure, let's trace what happens when
; we actually call

; (memo-fib 3)

; Again, a new enviroment E2 will be created, poiting to E1, in which
; `x` will be bound to 3:

; +------------------------+
; |                        |
; |  memoize: ...          |
; |                        |  Global env
; |  memo-fib: +-+         |
; |              |         | <-----------+
; +------------------------+             |   E1
;                |                +------+----------------+
;                v                |  f: (lambda (n) ...)  |
;              +-+-+    +-------> |  table: ...           |
;          +----|I|-----+         +--+--------------------+
;          |   +---+                 ^
;          v                         |
;     parameters: x                  |
;     body: (let previously...)      |
;                                +---+--+
;                                | x: 3 |
;                                +------+
;                                   E2

; Since there is no entry in the `table` in E1 for the input `x = 3`, the function
; `f` will be executed. Let's recall that `f` itself is the original lambda
; function from the `memo-fib` definition. It has two recursive calls. Let's recurse
; on the first, which will be:

; (memo-fib 2)

; +------------------------+
; |  fib: ...              |
; |  memoize: ...          |
; |                        |  Global env
; |  memo-fib: +-+         |
; |              |         | <-----------+
; +------------------------+             |   E1
;                |                +------+----------------+
;                v                |  f: (lambda (n) ...)  |
;              +-+-+    +-------> |  table: ...           |
;          +----|I|-----+         +--+--------+-----------+
;          |   +---+                 ^        ^
;          v                         |        |
;     parameters: x                  |        |
;     body: (let previously...)      |        |               
;                                +---+--+ +---+--+
;                                | x: 3 | | x: 2 |
;                                +------+ +------+
;                                   E2       E3
;

; This, again, will recurse twice, once on

; (memo-fib 1)

; Which will create E4 in fact return 1 and put that result on
; the table, and on

; (memo-fib 0)

; Which will create E5 in fact return 0 and put that result on
; the table.

; Both these results are returned to the call to `(memo-fib 2)`,
; which will return to `(memo-fib 3)`. We can now recurse the second
; time on `(memo-fib 3)` on:

; (memo-fib 1)

; This will create E6, but now we already have the return value
; for this call set on our memoization table! A simple lookup
; will return it.


; The exercise also asks if defining `memo-fib` as:

; (define memo-fib (memoize fib))

; would result in the same behavior. Let's see:

; Defining `memo-fib` would cause `memoize` to be executed with `fib`
; as its argument. We'd have:

; +------------------------+
; |  fib: ...              |
; |  memoize: ...          |
; |                        |  Global env
; |  memo-fib: +-+         |
; |              |         | <-----------+
; +------------------------+             |   E1
;                |                +------+------------------+
;                v                |  f: fib (ptr to global) |
;              +-+-+    +-------> |  table: ...             |
;          +----|I|-----+         +-------------------------+
;          |   +---+
;          v
;     parameters: x
;     body: (let previously...)

; Calling

; (memo-fib 3)

; Would actually memoize the _result_ of the call, but the internal recursive
; calls on `fib` would _not_ be memoized.
