; This exercise asks us to show that the percent tolerance of the product `int3`
; of two intervals `int1` and `int2` can be written as a simple function of the tolerances of
; the those terms, namely `int1` and `int2`, given that the tolerance percentages are small.
;
; One possible strategy to derive such simple function is to derive the "complicated" version
; of the function and see if we can neglect terms due to them being orders of magniture smaller
; than the other terms.
;
; To derive the "complicated" function, let's start by representing our intervals `int1` and
; `int2` in terms of their center and tolerance:
;
; int1 = c1 +- t1
; int2 = c2 +- t2
;
; Assuming, as hinted, all terms to be positive, we can deterministically write the product
; `int3` as:
;
; int3 = int1 * int2 = ( (c1-t1*c1)*(c2-t2*c2), (c1+t1*c1)*(c2+t2*c2) )
;
; where:
;
; width(int3)  = ((c1+t1*c1)*(c2+t2*c2) - (c1-t1*c1)*(c2-t2*c2)) / 2
;              = ((c1c2 + c1c2t2 + c1c2t1 + c1c2t1t2) - (c1c2 - c1c2t2 - c1c2t1 + c1c2t1t2)) / 2
;              = (c1c2(1 + t1 + t2 + t1t2) - c1c2(1 - t1 - t2 + t1t2)) / 2
;
; Cool! Here's the first place where we can leverage the fact that `t1` and `t2` are small. Since
; `t1 << 1` and `t2 << 1`, and since I'm an engineer and not a mathematician, the product `t1*t2`
; can safely be neglected for any real world computation. That means the `width(int3)` becomes:
;
; width(int3) = (c1c2(1 + t1 + t2) - c1c2(1 - t1 - t2)) / 2
;             = c1c2(2*t1 + 2*t2) / 2
;             = c1c2(t1 + t2)
;
; Analogously for `center(int3)`:
;
; center(int3) = ((c1+t1*c1)*(c2+t2*c2) + (c1-t1*c1)*(c2-t2*c2)) / 2
;              = (c1c2(1 + t1 + t2) + c1c2(1 - t1 - t2)) / 2
;              = 2*c1c2 / 2
;              = c1c2
;
; and, finally:
;
; percentage(int3) = width(int3)/center(int3)
;                  = c1c2(t1 + t2) / c1c2
;                  = t1 + t2
;
; In plain English, the percent tolerance of the product of two intervals can be calculated
; by simply summing the percent tolerance of the two intervals.
