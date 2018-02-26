(cons 5 3)
(cons #\a '())
(cons #\b #\a)
(cons 30 #\7)
(cons 5/7 #\a)
(cons '() '())
(cons #\x53 "maor")
(cons #\a #t)
(cons 17 0)
(cons 17 6/7)
(define koko (cons 6 7))
koko
(define koko (cons 3 #\x52))
koko

(car koko)
(define truer #t)
(cons 6 truer)
(define checker (lambda (x y) (cons x y)))
(define result (checker 10 truer))
result 
(pair? result)
(define result (checker truer '()))
result
(pair? result)
(car result)
