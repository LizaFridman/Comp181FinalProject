
((lambda (list) (begin
		(set-cdr! (cdr list) (cons 1 2))
		list)) (list 1 2 3 4))