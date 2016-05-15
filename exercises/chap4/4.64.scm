; Bad alternative to the `outranked-by` rule

; Original outranked-by:
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))


; Louis' version:
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))


; the problem arises from the fact that the two assertions
; inside the `and` compound assertion are switched in order.
; In Louis' version, the recursive assertion to `outranked-by`
; has an unbound `?middle-manager` variable, which will recurse
; exactly in the same way as the original assertion. Thil will cause
; an infinite loop to happen.
