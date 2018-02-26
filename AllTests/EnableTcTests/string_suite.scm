(make-string 3 #\a)
(make-string 17 #\z)
(string-ref (make-string 5 #\x53) (string-length "ab"))
(string? (make-string 33 #\c))
(define koko (make-string 4 #\m))
(string? koko)
(string-set! koko 1 #\a)
koko
(string-length koko)
(string-set! koko 2 #\o)
koko
(string-set! koko 3 #\r)
koko
(string-length koko)
(string? koko)
(define sym-string (symbol->string 'symbol))
(string? sym-string)
(string-set! sym-string 1 #\t)
sym-string
(string-set! sym-string 2 #\r)
sym-string
(string-set! sym-string 3 #\i)
sym-string
(string-set! sym-string 4 #\n)
sym-string
(string-set! sym-string 5 #\g)
sym-string
(string-length sym-string)
(define string-sym (string->symbol sym-string))
(string? string-sym)
(string? sym-string)