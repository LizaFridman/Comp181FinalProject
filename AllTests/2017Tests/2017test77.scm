
(define foo8 (lambda (x y) (
                            begin
                            (define y x)
                            (+ y x))
                 )
    )
(foo8 2 3)