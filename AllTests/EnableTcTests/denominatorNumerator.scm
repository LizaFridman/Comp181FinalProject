(numerator 5)
(numerator -5)
(numerator 5/7)
(numerator -6/7)
(denominator 5/7)
(denominator 7)
(denominator -5/7)
(denominator -8)
(denominator (numerator 6/7))
(numerator (denominator 5/7))
(numerator 5/7)
(define checker (lambda (x) (lambda (y) x y)))
(define to-num (lambda (x) (numerator x)))
(define to-denom (lambda (x) (denominator x)))
(to-num 5/7)
(to-denom 5/7)
(to-num -5)
(to-denom -5)
(to-denom 5/7)
(eq? (to-num 6/7) (to-denom 6/7))
(eq? (to-num 6) 6)
(eq? (to-denom 4/7) 7)
(eq? (to-denom 7) 7)
(eq? (to-denom 6) 1)
