; Ben's problematic `accumulation-function`

; Ben's idea of implementing a general funciton for accumulating over
; the stream resulting from a query, such as:

; (accumulation-function <variable>
;                        <query pattern>)

; Won't work for the exact reason we saw on exercise 4.65. For compound
; queries, we can have multiple instantiations returning the same final
; assertion.
; In other words, the resulting stream might have multiple assertions
; that represent, in fact, a single valid assertion.

; A possible solution for this is building a feature into the query language
; that filters the resulting stream for duplicate final instatiations. One
; possible way is to pass along the stream a list of instantiations and
; discard frames that have an instantiation that already appears on that
; list.
