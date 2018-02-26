'a
'b
"a"
"b"
"c"
'c

(define maor 'a)
(symbol? maor)
(symbol? 'a)
(symbol? "#t")
(symbol? #t)
(symbol? #f)
(symbol? 't)
(symbol? 'f)
(define maor "a")
(symbol? maor)
(define maor "maor")
(symbol? maor)
(define maor #f)
(symbol? maor)
(symbol? 'maor)
(symbol? "\x53;")
(symbol? #\a)
(symbol? #\x53)
(symbol? #\newline)
(symbol? "\n")
(symbol? 53/17)
(symbol? '())
(symbol? '(1 2))
(symbol? (and))
(symbol? (or))
(symbol? '#(1 2))



(symbol? (string->symbol "maor"))
(symbol? (symbol->string 'maor))
(symbol? (symbol->string (string->symbol "maor")))
(symbol? (string->symbol (symbol->string 'pnina)))

(define symboler 'symboler)
(define checker (lambda (x) (lambda (y) (x y))))
((checker symbol?) "string")
((checker symbol?) 'symbol)
((checker symbol?) symboler) 
((checker symbol?) #t)
((checker symbol?) 555)
((checker symbol?) 5/7)
((checker symbol?) 3/1)
((checker symbol?) '())
((checker symbol?) '(1 2) )
((checker symbol?) '#(1 2 #t))
((checker symbol?) (begin 1 2 'a))
((checker symbol?) (begin 'a 1 2))