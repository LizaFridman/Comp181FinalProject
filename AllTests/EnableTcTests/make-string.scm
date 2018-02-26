(make-string 3 #\a)
(make-string 17 #\z)
(make-string 17 #\x53)
(make-string 0 #\a)
(define koko (make-string 5 #\return))
koko
(define koko (make-string 3 #\x52))
koko
(define charer #\a)
(make-string 3 charer)
(define checker (lambda (x y ) (make-string x y)))
(define result (checker 3 charer))
result
(define result (checker 0 #\x53))
result 
(string? result)