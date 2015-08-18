; This exercise asks us to show that some operations on intervals
; can be written as functions of their width only.

; Let's take the segments:

; s1 = (a1, b1)
; s2 = (a2, b2)
; s3 = (a3, b3)

; Such that s3 = s1 + s2 = (a3, b3) = (a1+a2, b1+b2)

; Taking the width of s3, we get:

; w3 = (b3-a3)/2 = [(b1+b2) - (a1+a2)]/2 = (b1-a1)/2 + (b2-a2)/2 = w1 + w2

; This means that the width of the result of adding two segments
; can be written as a function only of their widths.

; Now the exercise asks us to show that the same does not work, for example,
; for multiplication.

; Let's take now seg3 = seg1 * seg2 = (a3, b3) = (a1*a2, b1*b2). We have:

; w3 = (b3-a3)/2 = [(b1*b2) - (a1*a2)]/2

; As we can see, we cannot write those terms in terms of w1 and w2.
