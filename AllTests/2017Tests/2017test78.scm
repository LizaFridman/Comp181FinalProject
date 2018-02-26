
(define foo9 (lambda (x y) (
                            begin
                            (define y x)
                            (eq? y x))
                 )
    )
(foo9 12 8)