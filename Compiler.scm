(load "sexpr-parser.scm")
(load "tag-parser.scm")
(load "semantic-analyzer.scm")

(define file->list
  (lambda (in-file)
    (let ((in-port (open-input-file in-file)))
      (letrec ((run
		(lambda ()
		  (let ((ch (read-char in-port)))
		    (if (eof-object? ch)
			(begin
			  (close-input-port in-port)
			  '())
			(cons ch (run)))))))
	(run)))))

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
  (lambda (s)
    ((star <sexpr>) s
     (lambda (m r)
       (map (lambda (e)
	      (annotate-tc
	       (pe->lex-pe
		(box-set
		 (remove-applic-lambda-nil
		  (parse e))))))
	    m))
     (lambda (f) 'fail))))



(define list->sexprs
  (lambda (lst)
      (letrec ((helper
                (lambda (lst)
                  (display (format "lst = ~a\n" lst))
                  (<sexpr> lst 
                           (lambda (e s) (if (null? s)
                                             (list e)
                                             (cons e (helper s))))
                           (lambda (w) `(error ,@w))))))
        (helper lst))))

(define string->sexprs
  (lambda (str)
    (let ((lst (string->list str)))
      (letrec ((helper
                (lambda (lst)
                  (<sexpr> lst 
                           (lambda (e s) (if (null? s)
                                             (list e)
                                             (cons e (helper s))))
                           (lambda (w) `(error ,@w))))))
        (helper lst)))))


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
    (display (format "Compiled Scheme File ~a to ~b\n" source dest))))
