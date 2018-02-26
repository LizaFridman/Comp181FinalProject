
(define f (lambda (p x) (begin
                            (set-car! p x)
                            p)))
(f (cons 4 5) 444)