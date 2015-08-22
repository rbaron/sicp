; This exercise asks us to modify the change-counting program from section 1.2.2
; so that we can use it with arbitrary coin denominations.

; Given procedures

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;  The exercise asks us to implement the following procedures:

(define (first-denomination coin-values) (car coin-values))

(define (except-first-denomination coin-values) (cdr coin-values))

(define (no-more? coin-values) (null? coin-values))

; Let's try this out with a few inputs:

(cc 100 us-coins)
(cc 50 uk-coins)
(cc 50 (reverse uk-coins))

; The order of the coins denomination shouldn't produce different results,
; since we are doing an exhaustive sweep on the search space.
