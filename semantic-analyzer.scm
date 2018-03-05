(load "project/tag-parser.scm")

(define lambda-simple-tag?
  (lambda (pe)
    (and (list? pe)
 	 (eq? 'lambda-simple (car pe)))))

(define lambda-opt-tag?
  (lambda (pe)
    (and (pair? pe)
 	 (eq? 'lambda-opt (car pe)))))

(define applic-lambda-nil?
  (lambda (expr)
    (and (not (null? expr))
	 (list? expr)
	 (eq? 'applic (car expr))
	 (lambda-simple-tag? (cadr expr))
	 (null? (cadadr expr)))))

(define remove-applic-lambda-nil
  (lambda (expr)
    ;;(display (format "Removing applic of lambda null for ~a\n" expr))
    (if (applic-lambda-nil? expr)

	(begin
	  ;;(display (format "~a passed as applic-lambda-nil\n" expr))
	  (let* ((lambda-simple (cadr expr))
		 (body (caddr lambda-simple)))
	    (remove-applic-lambda-nil body)))

	(begin
	  ;;(display (format "~a DIDN'T passed as applic-lambda-nil\n" expr))
	  (if (or (not (list? expr))
		  (not (pair? expr)))
	      expr
	      (map remove-applic-lambda-nil expr))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mtrx-member
  (lambda (el mtrx)
    (if (or (null? mtrx) (not (pair? mtrx)))
	#f
	(ormap (lambda (row)
		 (member el row))
	       mtrx))))

(define var?
  (lambda (expr)
    (and (pair? expr)
	 (equal? 'var (car expr)))))

(define var-in-body?
  (lambda (var)
    (lambda (body)
      (cond ((not (pair? body))
	     #f)
	    ((or (equal? var body) (member var body))
	     #t)
	    (else (ormap (var-in-body? var)
			 body)))
      )))

(define bound-var?
  (lambda (var)
    (lambda (pe)
      (cond ((not (pair? pe))
	     #f)
	    ((lambda-simple-tag? pe)
	     ((var-in-body? var) (caddr pe)))
	    ((lambda-opt-tag? pe)
	     ((var-in-body? var) (cadddr pe)))
	    (else (ormap (bound-var? var)
			 pe))
	    ))))

(define var-set?
  (lambda (var)
    (lambda (pe)
      (cond ((not (pair? pe))
	     #f)
	    ;;(set (var ,var) (expr))
	    ((and (equal? 'set (car pe))
		  (equal? var (cadr pe)))
	     #t)
	    (else (ormap (var-set? var)
			 pe))))))

(define var-get?
  (lambda (var)
    (lambda (pe)
      (cond ((not (pair? pe))
	     #f)
	    ;;(var ,var)
	    ((or (equal? var pe)
		 (member var pe))
	     #t)
	    ;;(set (var ,var) (expr))
	    ((equal? 'set (car pe))
	     ((var-get? var) (caddr pe)))
	    (else (ormap (var-get? var)
			 pe))))))

(define remove-param-lambda
  (lambda (param)
    (lambda (pe)
      (cond ((not (pair? pe))
	     pe)
	    ((and (lambda-simple-tag? pe)
		  (member param (cadr pe)))
	     '())
	    ((and (lambda-opt-tag? pe)
		  (member param (append (cadr pe) (list (caddr pe)))))
	     
	     '())
	    (else
	     (map (remove-param-lambda param)
		  pe))))))

(define to-box?
  (lambda (arg pe)
    (let* ((var `(var ,arg))
	   (param-removed ((remove-param-lambda arg) pe)))
      (and ((bound-var? var) param-removed)
	   ((var-set? var) param-removed)
	   ((var-get? var) param-removed)
	   ))))

(define box-arg
  (lambda (arg)
    (lambda (pe)
      (cond ((not (pair? pe))
	     pe)
	    
	    ;;(var arg)
	    ((and (var? pe)
		  (equal? arg (cadr pe)))
	     `(box-get ,pe))
	    
	    ;;(set (var arg) expr)
	    ((and (equal? 'set (car pe))
		  (equal? arg (cadadr pe)))
	       `(box-set ,(cadr pe) ,((box-arg arg) (caddr pe))))
	    
	    ;;(lambda-simple (args) (body))
	    ((lambda-simple-tag? pe)
	     `(lambda-simple ,(cadr pe) ,(if (member arg (cadr pe))
					     (caddr pe)
					     ((box-arg arg) (caddr pe)))))
	    
	    ;;(lambda-opt (args) opt (body))
	    ((lambda-opt-tag? pe)
	     `(lambda-opt
	       ,(cadr pe) ,(caddr pe)
	       ,(if (member arg (append (cadr pe) (list (caddr pe))))
		    (cadddr pe)
		    ((box-arg arg) (cadddr pe)))))
	    
	    (else (map (box-arg arg)
		       pe))))))

(define box-body
  (lambda (args body)
    (cond ((null? args)
	   body)
	  ((to-box? (car args) body)
	   (box-body (cdr args) ((box-arg (car args)) body)));;)
	  (else (box-body (cdr args) body)))))

(define box-lambda
  (lambda (args body)
    (let ((sets-to-add (map (lambda (var)
			      `(set (var ,var) (box (var ,var))))
			    (filter (lambda (arg)
				      (to-box? arg body))
				    args))))
      (if (null? sets-to-add)
	  body
	  `(seq ,(append sets-to-add (if (equal? 'seq (car body))
					(box-body args (cadr body))
					(list (box-body args body)))))))))

(define box-set
  (lambda (pe)
    ;;(display (format "~a in Box-Set\n" pe))
    (cond ((or (not (pair? pe))
	       (not (list? pe)))
	   pe)
	  ((lambda-simple-tag? pe)
	     `(lambda-simple ,(cadr pe) ,(box-lambda (cadr pe) (box-set (caddr pe)))))
	  ((lambda-opt-tag? pe)
	   `(lambda-opt ,(cadr pe) ,(caddr pe) ,(box-lambda (append (cadr pe) (list (caddr pe))) (box-set (cadddr pe)))))
	  (else (map box-set
		     pe)))))

;;;;;;;;;;;;;;;;;;;;;;;;
(define index-of
  (lambda (el lst)
    (if (null? lst)
	-1
	(if (eq? (car lst) el)
	    0
	    (if (= -1 (index-of el (cdr lst)))
		-1
		(+ 1 (index-of el (cdr lst))))))))

(define major-index-of
  (lambda (el mtrx)
    (if (member el (car mtrx))
	0
	(+ 1 (major-index-of el (cdr mtrx))))))

(define minor-index-of
  (lambda (el row mtrx)
    (if (= 0 row)
	(index-of el (car mtrx))
	(minor-index-of el (- row 1) (cdr mtrx)))))

(define lex-pe
  (lambda (pe params env)
    ;;(display (format "Lex-pe pe: ~a\nparams ~a\n env = ~a\n" pe params env))
    (cond ((or (not (pair? pe))
	       (not (list? pe)))
	   pe)
	  ;;Found variable
	  ((var? pe)
	   (let ((name (cadr pe)))
	     (cond ((member name params)
		    `(pvar ,name ,(index-of name params)))
		   ((mtrx-member name env)
		    (let* ((major (major-index-of name env))
			   (minor (minor-index-of name major env)))
		      `(bvar ,name ,major ,minor)))
		   (else
		    `(fvar ,name)))))
	  ;;Found lambdas
	  ((lambda-simple-tag? pe)
	   (let ((args (cadr pe))
		 (body (cddr pe))
		 (new-env (cons params env)))
	     `(lambda-simple ,args
			     ,@(map (lambda (expr)
				      (lex-pe expr args new-env))
				    body))))
	  ((lambda-opt-tag? pe)
	   (let* ((args (cadr pe))
		  (opt (caddr pe))
		  (body (cdddr pe))
		  (new-params (append args (list opt)))
		  (new-env (cons params env)))
	     `(lambda-opt ,args
			  ,opt
			  ,@(map (lambda (expr)
				   (lex-pe expr new-params new-env))
				 body))))
	  (else
	   (map (lambda (expr)
		  (lex-pe expr params env))
		pe)))
	  ))

(define pe->lex-pe
  (lambda (pe)
    (lex-pe pe '() '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tc-last-expr
  (lambda (expr-lst tp?)
    (if (null? (cdr expr-lst))
	(list (ATP (car expr-lst) tp?))
	(cons (ATP (car expr-lst) #f)
	      (tc-last-expr (cdr expr-lst) tp?)))))

(define ATP
  (lambda (pe tp?)
    (cond ((or (not (pair? pe))
	       (eq? 'var (car pe))
	       (eq? 'const (car pe)))
	   pe)
	  ((eq? 'or (car pe))
	   `(or ,(tc-last-expr (cadr pe) tp?)))
	  ((eq? 'if3 (car pe))
	   (let ((test (cadr pe))
		 (dit (caddr pe))
		 (dif (cadddr pe)))
	     `(if3 ,(ATP test #f)
		   ,(ATP dit tp?)
		   ,(ATP dif tp?))))
	  ((eq? 'define (car pe))
	   `(define ,(cadr pe) ,(ATP (caddr pe) #f)))
	  ((lambda-simple-tag? pe)
	   (let ((args (cadr pe))
		 (body (caddr pe)))
	     `(lambda-simple ,args ,(ATP body #t))))
	  ((lambda-opt-tag? pe)
	   (let ((args (cadr pe))
		 (opt (caddr pe))
		 (body (cadddr pe)))
	     `(lambda-opt ,args ,opt ,(ATP body #t))))
	  ((or (eq? 'set (car pe))
	       (eq? 'box-set (car pe)))
	   (let ((name (cadr pe))
		 (value (caddr pe)))
	     `(,(car pe) ,name ,(ATP value #f))))
	  ((eq? 'seq (car pe))
	   `(seq ,(tc-last-expr (cadr pe) tp?)))
	  ((eq? 'applic (car pe))
	   (if tp?
	       `(tc-applic ,@(ATP (cdr pe) #f))
	       `(applic ,@(ATP (cdr pe) #f))))
	  (else (map (lambda (expr)
		       (ATP expr tp?))
		     pe)))))

(define annotate-tc
  (lambda (pe)
    (ATP pe #f)))

