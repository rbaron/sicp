; Leveraging the scheme built-in type system

; Let's modify some procedures from out type system to take
; the internal, built-in, scheme type system into account as well.
; We need to be able to use our arithmetic package in the same
; way as before, but representing numbers as "pure" scheme numbers.

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((symbol? datum) 'symbol)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
        ((symbol? contents) contents)
        (else (cons type-tag contents))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((symbol? datum) datum)
        (else (cdr datum))))

; Now the type tag for numbers and symbols are computed "on the fly"
; instead of carried around inside the data.
