
    
(if #t #f)
(if 1 2 3)
(if #f 2 3)
(if 1 2)
(if 3 '(1 2 3) 5)
(if #t 5 6)
(if #f 5 6)
(if #f 6)
(if #t "maor" "pnina")
(if #t 'maor 'pnina)
(if #f 'maor 'pnina)
(if "maor" "maor" "pnina")
(if 't 'true 'false)
(define ifer (if 1 2 3))
ifer
(define no-elser (if 1 2))
no-elser
(define positivirator (if #t 'true 'false))
positivirator
(define negativirator (if #f 'true 'false))
negativirator
(if (if 1 2 3) 4)
(if 2 (if 1 2 3))
(if 1 9 (if 1 2 3))

(define conder 
   (lambda (x)
       (cond 
            ((number? x) 5)
           ((string? x) "maor")
            ((char? x) #\a)
       )
    )
)
(conder 7)

(define ifer (lambda (x y z) (if x y z)))
(ifer 1 2 3)