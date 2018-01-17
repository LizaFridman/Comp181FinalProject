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

