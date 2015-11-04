; Smoothing the input signal to filter noisy sensor readings

; There is a subtle problem with Louis' implementation:
(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-value)
                 (make-zero-crossings (stream-cdr input-stream)
                                      avpt))))

; As we can see in the recursive call, `last-value` is the
; previously soothed value. In the next iteration, the procedure
; will use `last-value` to smooth the next value. This is
; incorrect behavior, since the _original_ last value should
; be used for smoothing, and not the previously smoothed value.

; In order to find crossings between the _smoothed_ stream, we
; need to use the last averaged data point as well:

(define (make-zero-crossings input-stream last-value last-avg)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avg)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))
