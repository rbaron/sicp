; Infinite stream

; If we choose the following suggested implementation:

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
        (cons-stream assertion THE-ASSERTIONS))
  'ok)

; We'd be generating an infinite stream, just like we did
; with the `ones` stream:

(define ones (cons-stream 1 ones))

; The reason is that, once the stream-cdr is lazily evaluated, it will
; lookup the `THE-ASSETIONS` reference, which will now be pointing to
; the stream itself.
