(define koko '#(1 2 3 #t #f #\a #\b #\x53 5/7 -7))
koko
(vector-set! koko 0 3)
koko
(vector-set! koko 1 #\y)
koko
(vector-set! koko 1 #t)
koko
(vector-set! koko 1 #f)
koko

(vector-set! koko 0 #\a)
koko
(vector-set! koko 6 #\a)
koko
(vector-set! koko 7 2/3)
koko
(vector-set! '#(1 2 3) 0 #\a)
(vector-set! '#(1 2 3) 1 #\t)
koko
(define vectorer koko)
vectorer
(vector-set! vectorer 3 #\3)
vectorer
(define checker (lambda (x y z) (vector-set! x y z)))
(checker vectorer 1 #\1)
vectorer