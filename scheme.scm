(define testFunc
  (lambda args
    (if (list? args)
      1
      0)))
(define testFunc2
  (lambda (a b)
    (and 1 0)))
