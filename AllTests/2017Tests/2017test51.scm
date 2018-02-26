
(define (c set1 set2)
  (define s1 (car set1))
  (define s2 (car set2))
  (cond
    ((or (null? set1) (null? set2)) (append set1 set2))
    (else
       (cond
       ((= s1 s2) (cons s1 (cons (cdr set1) (cdr set2))))
       ((> s1 s2) (cons s2 (cons set1 (cdr set2))))
       ((< s1 s2) (cons s1 (cons (cdr set1) set2)))))))	   
(c '(1 2 3) '(4 6))