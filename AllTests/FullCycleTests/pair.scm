1
2
3
4
'()
'(1 2 3)
'(1 2 3)
1
2
3
4
'(1 . (2 . (3 . ())))
'(1 . (2 . (3 . (2 . ( 3 . ( 1 . ( 4 . ())))))))
'((1 2) (3 4))
'()

(define nothing '())
nothing
(define something '(1 . ()))
something
nothing
(define something '(1 2))
something 
nothing
(define pnina '(77 . ()))
pnina
(define pnina '(77 . ( 66 . () )))
pnina
(define something pnina)
pnina
something

