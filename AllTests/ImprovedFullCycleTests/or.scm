(or 1 2 3)
(or)
(or 1)
(or #f #f)
(or #t #f)
(or #t #t)
(or (begin 1 2) 2)
(or "maor" "\x51;")
(or "maor" "maor")
(or "\x51;" "maor")
(or "\x51;" "\x51;")
(or 4/7 65/7)
(or 65/7 4/7)
(or 65/7 65/7)
(or #\x51 #\x52)
(or #\x51 #\x51)
(or #\x52 #\x51)
(or #\newline #\newline)
(or #\newline #\page)
(or #\page #\newline)
(or '(1 . ( 2 . ())) '(1 2) )
(or '(1 . ( 2 . ())) '(3 . ( 4 . ())))
(or '(1 . ( 2 . ())) '(1 . ( 2 . ())) )
(or '(1 2) '(1 . ( 2 . ())))
(or '() '() )
(or 'maor 'pnina)
(or 'maor 'maor)
(or 'pnina 'maor)
(or 'pnina 'pnina)

(or (or 1 2) (or 3 4))
(or (or 3 4) (or 1 2))
(or (or 1 2) (or 1 2))
(or (or 1 2) 1)
(or (or 1))
(or (or))
(or 11 (or 1 2))

(define orer (or 1 2))
orer 
(define orer2 (or 1 1))
orer2
(define orer3 (or 2 3))
orer3