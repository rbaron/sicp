; To figure out the number of times the `remainder` procedure is called,
; let's try and simulate the call stack:
;
;
; First case: normal-order evaluation
; ===================================
;
; (gcd 206 40)
; ; b == 40, so recurse (0 calls to remainder)
;
; (gcd 40 (remainder 206 40))
; ; b == 6, so recurse (1 call to remainder)
;
; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; ; b == 4, so recurse (1+2 calls to remainder)
;
; (gcd (remainder 40 (remainder 206 40))
;      (remainder (remainder 206 40)
;        (remainder 40 (remainder 206 40))))
; ; b == 2, so recurse (1+2+4 calls to remander)
;
; (gcd
;      (remainder (remainder 206 40)
;        (remainder 40 (remainder 206 40)))
;      (remainder
;        (remainder 40 (remainder 206 40))
;        (remainder (remainder 206 40)
;          (remainder 40 (remainder 206 40)))))
; ; b == 0, so return a (1+2+4+7 calls to remainder + 4 to evaluate `a`)
;
; Total calls to `remainder`: 18
;
;
; Second case: applicative-order evaluation
; =========================================
;
; (gcd 206 40)
; ; b == 40, so recurse (1 call to remainder to evaluate new args)
;
; (gcd 40 6)
; ; b == 6, so recurse (1+1 call to remainder to evaluate new args)
;
; (gcd 6 4)
; ; b == 2, so recurse (1+1+1 call to remainder to evaluate new args)
;
; (gcd 4 2)
; ; b == 2, so recurse (1+1+1+1 call to remainder to evaluate new args)
;
; (gcd 2 0)
; ; returns 2
;
; Total calls to `remainder`: 4
