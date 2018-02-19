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

(define forth
  (lambda (lst)
    (cadddr lst)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  C-Table  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define c-table '())

;;Gets a const in the table and returns its ADDRESS
;;a.k.a:  c-table[i] = <Memory-Index, Value, (Type, Type-Data)>
(define c-table-contains?
  (lambda (const)
    (cond ((null? c-table)
	   ;;#f
	   0)
	  ;;c-table[i] == const
	  ((equal? const (second (car c-table)))
	   (first (car c-table)))
	  (else
	   (c-table-contains? (cdr c-table) const)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Code Generation  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tag?
  (lambda (tag pe)
    (and (list? pe)
	 (equal? tag (first pe)))))

(define code-gen
  (lambda (pe)
    ;;After each generation, the value of the generated code is in RAX
    ;;Returns string
    ;;(display (format "Code Gen to ~a\n" pe))
    (string-append ";" (format "~a" pe) newLine
		   (cond ((tag? 'const pe)
			  (cg-const (second pe)))
			 
			 ((tag? 'pvar pe)
			  (cg-pvar pe))
			 
			 ((tag? 'bvar pe))
			 
			 ((tag? 'fvar pe))
			 
			 ((tag? 'if3 pe)
			  ;;(if3 test dit dif)
			  (let ((test (second pe))
				(dit (third pe))
				(dif (forth pe)))
			    (cg-if3 test dit dif)))
			 
			 ((tag? 'or pe)
			  (cg-or (second pe) (make-label "L_orEnd")))
			 
			 ((tag? 'seq pe)
			  ;;(seq (E1 .. En))
			  (cg-seq (second pe)))
	  
			 ((tag? 'lambda-simple pe))
			 
			 ((tag? 'lambda-opt pe))
			 
			 ((tag? 'define pe))
			 
			 ((tag? 'applic pe)
			  (string-append ";" (format "~a" pe)))
			 
			 ((tag? 'tc-applic pe))
			 
			 ((tag? 'set pe))
			 
			 ((tag? 'box pe))
			 
			 ((tag? 'box-get pe))
	  
			 ((tag? 'box-set? pe))
			 
			 (else 'Code-Generation-Error!)))))

(define newLine
  (list->string '(#\newline)))

(define tab
  (list->string '(#\tab)))

(define labelIndex 0)

(define make-label
  (lambda (name)
    (set! labelIndex (+ labelIndex 1))
    (string-append name (number->string labelIndex))))

(define cg-const
  (lambda (const)
    ;;(number->string const)
    (let ((value (number->string (c-table-contains? const))))
      (string-append
       tab "MOV RAX, " value newLine)
      )))


(define cg-or
  (lambda (lst end-label)
      (cond ((null? lst)
	     (list->string '()))
	    ((null? (cdr lst))
	     (let ((cg-N (code-gen (first lst))))
	       (string-append cg-N newLine
			      end-label ":" newLine)))
	    (else
	     (let ((cg-i (code-gen (first lst))))
	       (string-append cg-i newLine
			      tab "CMP RAX, SOB_FALSE" newLine
			      tab "JNE " end-label newLine
			      (cg-or (cdr lst) end-label)))))))

(define cg-pvar
  (lambda (pe)
    (let ((minor (third pe)))
      (string-append
       tab "MOV RAX, [" (number->string (+ minor 2)) "]" newLine))))

(define cg-if3
  (lambda (test dit dif)
    (let ((test-cg (code-gen test))
	  (dit-cg (code-gen dit))
	  (dif-cg (code-gen dif))
	  (l-dif (make-label "L_if3Dif"))
	  (l-end (make-label "L_if3End")))
      
      (string-append test-cg newLine
		     tab "CMP RAX, SOB_FALSE" newLine
		     tab "JE " l-dif newLine
		     dit-cg newLine
		     tab "JMP " l-end newLine
		     l-dif ":" newLine
		     dif-cg newLine
		     l-end ":" newLine
		     ))))
    
(define cg-seq
  (lambda (pe)
    (fold-left (lambda (result e)
		 ;;(display (format "cg-seq: e = ~a\nresult = ~b\n" e result))
		 (string-append result (code-gen e) newLine))
	       (list->string '())
	       pe)))
