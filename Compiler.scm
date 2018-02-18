(load "sexpr-parser.scm")
(load "tag-parser.scm")
(load "semantic-analyzer.scm")

(define file->list
  (lambda (in-file)
    (let ((in-port (open-input-file in-file)))
      (letrec ((run
		(lambda ()
		  (let ((readChar (read-char in-port)))
		    (if (eof-object? readChar)
			(begin
			  (close-input-port in-port)
			  '())
			(cons readChar (run)))))))
	(run)))))

(define file->string
  (lambda (in-file)
    (list->string (file->list in-file))))

(define string->file
  (lambda (str out-file)
    (let ((out-port (open-output-file out-file 'truncate))
          (lst (string->list str)))
      (letrec ((run
                (lambda (lst)
                  (if (null? lst)
                      (close-output-port out-port)
                      (begin (write-char (car lst) out-port)
                             (run (cdr lst)))))))
        (run lst)))))


(define pipeline
  (lambda (sexpr)
    ((star <sexpr>) sexpr
     (lambda (match rest)
       (map (lambda (expr)
	      (annotate-tc
	       (pe->lex-pe
		(box-set
		 (remove-applic-lambda-nil
		  (parse expr))))))
	    match))
     (lambda (fail) 'fail))))



(define list->sexprs
  (lambda (lst)
        (pipeline lst)))

(define string->sexprs
  (lambda (str)
    (let ((stringList (string->list str)))
      (letrec ((translate
                (lambda (lst)
                  (<sexpr> lst 
                           (lambda (expr rest) (if (null? rest)
                                             (list expr)
                                             (cons expr (translate rest))))
                           (lambda (msg) `(error ,@msg))))))
        (translate stringList)))))


;--------------------------------------------------| cTable |--------------------------------------------------------


;  is empty or not a list   -> returns saved results
;  car passes test          -> save car, remove it and do on the rest
;  car is a list            -> open the car and do again
;  else                     -> remove car and do on the rest
(define those-that-pass
  (lambda (exps test positive-results)
    (cond 
          ((or (not (pair? exps)) (null? exps)) positive-results)
          ((test (car exps)) (those-that-pass (cdr exps) test (cons (car exps) positive-results)))
          ((pair? (car exps)) (those-that-pass `(,@(car exps) ,@(cdr exps)) test positive-results))
          (else (those-that-pass (cdr exps) test positive-results)))))

; returns deep search, returns elements that pass test
(define ordered-those-that-pass
  (lambda (exps test)
    (reverse (those-that-pass exps test '()))))


(define compile-scheme-file
  (lambda (source dest)
    (let* ((pipelined (list->sexprs (file->list source)))
	   (size (length pipelined)))
      (display (format "Compiled Scheme File with ~a parsed expressions!\n" size))
      ;;pipelined
      )))
