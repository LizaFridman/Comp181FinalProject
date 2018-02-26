(define koko "maor")
(string-set! koko 1 #\y)
koko
(string-set! koko 3 #\y)
koko
koko
(define who "who")
(string-set! (symbol->string 'who) 0 #\k)
who

(string-set! "\t" 0 #\a)
(string-set! "\x53;" 0 #\a)
(string-set! "\f" 0 #\a)
(string-set! "\"" 0 #\a)
(string-set! " hh jj " 0 #\t)
(define stringer "string")
(string-set! stringer 3 #\3)
(define checker (lambda (x y z) (string-set! x y z)))
(checker stringer 1 #\1)
stringer