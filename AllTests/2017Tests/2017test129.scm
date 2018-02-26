
(define f (lambda (p x) (begin
                            (set-cdr! p x)
                            p)))
(f (cons 4 5) 444)