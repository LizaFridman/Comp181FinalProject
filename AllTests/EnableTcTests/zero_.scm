0/1
3/1
9
3/3
1
0
5
(zero? 5)
(zero? 0)
(zero? 5/7)
(zero? 9/9)
(zero? 0/8)
(zero? 0/1)
(zero? -0/1)
(zero? -0/9)
(zero? -55)

(define zeroer 0)
(define checker (lambda (x) (lambda (y) (x y)))) 
((checker zero?) zeroer)
((checker zero?) 555)
((checker zero?) 5/7)
((checker zero?) 3/1)
((checker zero?) (begin 1 2 0))
((checker zero?) (begin 0 1 2))