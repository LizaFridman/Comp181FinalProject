(load "sexpr-parser.scm")
(load "tag-parser.scm")
(load "semantic-analyzer.scm")


(define first
  (lambda (lst)
    (car lst)))

(define second
  (lambda (lst)
    (cadr lst)))

(define third
  (lambda (lst)
    (caddr lst)))

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

(define list->file
  (lambda (lst out-file)
    (let ((out-port (open-output-file out-file 'truncate)))
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
	   (size (length pipelined))
	   (generated (map code-gen
			   pipelined))
	   (asm-code (fold-left string-append "" generated)))
      (display (format "Compiled Scheme File with ~a parsed expressions!\n" size))
      (list->file (string->list asm-code) dest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Code Generation  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tag?
  (lambda (tag pe)
    (and (list? pe)
	 (equal? tag (first pe)))))

(define code-gen
  (lambda (pe)
    ;;After each generation, the value of the generated code is in RAX
    (display (format "Code Gen to ~a\n" pe))
    (cond ((tag? 'const pe)
	   (cg-const (second pe)))
	  ((tag? 'pvar pe))
	  ((tag? 'bvar pe))
	  ((tag? 'fvar pe))
	  ((tag? 'if3 pe))
	  ((tag? 'or pe))
	  ((tag? 'seq pe)
	   (cg-seq (second pe)))
	  ((tag? 'lambda-simple pe))
	  ((tag? 'lambda-opt pe))
	  ((tag? 'define pe))
	  ((tag? 'applic pe))
	  ((tag? 'tc-applic pe))
	  ((tag? 'set pe))
	  ((tag? 'box pe))
	  ((tag? 'box-get pe))
	  ((tag? 'box-set? pe))
	  (else 'Code-Generation-Error!))))

(define newLine
  (list->string '(#\newline)))

(define tab
  (list->string '(#\tab)))

(define cg-const
  (lambda (const)
    (number->string const)))

(define cg-seq
  (lambda (pe)
    (fold-left (lambda (result e)
		 (string-append result (code-gen e) newLine))
	       (list->string '())
	       pe)))
