(load "qq.scm")

(define *reserved-words*
  '(and begin cond define do else if lambda 
	let let* letrec or quasiquote unquote
	unquote-splicing quote set!))

(define void
  (if #f #f))

(define void?
  (lambda (expr)
    (equal? expr void)))

(define variable?
  (lambda (expr)
    (and (symbol? expr) (not (member expr *reserved-words*)))))

(define if-dit?
  (lambda (exprs)
    (and (list? exprs) (eq? 'if (car exprs)) (eq? (length exprs) 3))))

(define if-dit-dif?
  (lambda (exprs)
    (and (list? exprs) (eq? 'if (car exprs)) (eq? (length exprs) 4))))

(define empty-or?
  (lambda (expr)
    (and (eq? 'or (car expr)) (eq? (length expr) 1))))

(define single-or?
  (lambda (expr)
    (and (or? expr) (= (length (cdr expr)) 1))))

(define or?
  (lambda (expr)
    (and (pair? expr) (eq? 'or (car expr)))))

(define lambda?
  (lambda (expr)
    (and (pair? expr) (eq? 'lambda (car expr)) (>= (length expr) 3))))

(define lambda-simple?
  (lambda (expr)
    (and (lambda? expr) (list? (cadr expr)))))

(define lambda-opt?
  (lambda (expr)
    (and (lambda? expr) (not (or (list? (cadr expr)) (symbol? (cadr expr)))))))

(define lambda-var?
  (lambda (expr)
    (and (lambda? expr) (symbol? (cadr expr)))))

(define split$
  (lambda (args k)
    (if (symbol? (cdr args))
	(k `(,(car args)) (cdr args))
	(split$ (cdr args)
		(lambda (man-args arg)
		  (k (cons (car args) man-args) arg))))))

(define begin?
  (lambda (expr)
    (and (list? expr) (eq? 'begin (car expr)))))

(define define?
  (lambda (expr)
    (and (list? expr) (eq? 'define (car expr)) (>= (length expr) 3))))

(define define-simple?
  (lambda (expr)
    (and (define? expr) (symbol? (cadr expr)))))

(define define-MIT?
  (lambda (expr)
    (and (define? expr))))

(define set!?
  (lambda (expr)
    (and (list? expr) 
         (eq? 'set! (car expr)) 
         (= (length expr) 3) 
         (symbol? (cadr expr)))))

(define applic?
  (lambda (expr)
    (and (list? expr) (not (member (car expr) *reserved-words*)))))

(define and?
  (lambda (expr)
    (and (pair? expr) (eq? 'and (car expr)))))

(define empty-and?
  (lambda (expr)
    (and (and? expr) (= (length (cdr expr)) 0))))

(define single-and?
  (lambda (expr)
    (and (and? expr) (= (length (cdr expr)) 1))))

(define multiple-and?
  (lambda (expr)
    (and (and? expr) (> (length (cdr expr)) 1))))

(define let?
  (lambda (expr)
    (and (pair? expr) 
         (eq? 'let (car expr))
         (>= (length expr) 3)
         (list? (cadr expr))
         (andmap (lambda (lst)
                  (and (list? lst) (= (length lst) 2)))
                  (cadr expr))
         (>= (length (cddr expr)) 1))))

(define let*?
  (lambda (expr)
    (and (pair? expr) 
         (eq? 'let* (car expr))
         (>= (length expr) 3)
         (list? (cadr expr))
         (andmap (lambda (lst)
                  (and (list? lst) (= (length lst) 2)))
                  (cadr expr))
         (>= (length (cddr expr)) 1))))

(define letrec?
  (lambda (expr)
    (and (pair? expr) 
         (eq? 'letrec (car expr))
         (>= (length expr) 3)
         (list? (cadr expr))
         (andmap (lambda (lst)
                  (and (list? lst) (= (length lst) 2)))
                  (cadr expr))
         (>= (length (cddr expr)) 1))))




(define cond-clauses (lambda (exp) (cdr exp)))

(define cond-test (lambda (clause) (car clause)))
(define cond-results (lambda (clause) (cdr clause)))

(define cond-first-clause (lambda (clauses) (car clauses)))
(define cond-rest-clauses (lambda (clauses) (cdr clauses)))
(define cond-last-clause? (lambda (clauses) (null? (cdr clauses))))
(define cond-empty-clauses? (lambda (clauses) (null? clauses)))

(define cond-else-clause? (lambda (clause) (eq? (cond-test clause) 'else)))

(define cond?
  (lambda (expr)
    (and (pair? expr)
	 (eq? 'cond (car expr))
	 (list? (cdr expr))
	 (>= (length expr) 2))))

(define cond-single-clause?
  (lambda (expr)
    (and (cond? expr)
	 (cond-last-clause? (cond-clauses expr)))))

(define cond-test-only-clause?
  (lambda (clause)
    (and (not (null? (cond-test clause))) (null? (cond-results clause)))))

(define quasiquote?
  (^quote? 'quasiquote))
  
(define remove-duplicates
  (lambda (lst)
    (if (null? lst)
      '()
      (cons (car lst)
            (remove-duplicates (filter (lambda (mem)
                                        (not (equal? mem (car lst))))
                                       (cdr lst)))))))

(define remove-dup-sets
  (lambda (pair-list)
    (if (null? pair-list)
      '()
      (cons (car pair-list)
            (remove-dup-sets (filter (lambda (pair)
                                        (not (eq? (car pair) (caar pair-list))))
                                       (cdr pair-list)))))))

(define filter-begin
  (lambda (lst)
    (fold-right (lambda (x y)
                  (if (list? x)
                      (if (equal? (car x) 'begin)
                          (filter-begin (append (cdr x) y))
                          (cons x y))
                      (cons x y)))
                '()
                lst)))

(define parse
  (lambda (sexpr)
    ;;(display (format "Parsing ~a\n" sexpr))
    (cond ((or (const? sexpr)
	       (void? sexpr))
	   ;;(display (format "Const or Void ~a\n" sexpr))
	   (if (quote? sexpr)
	       `(const ,@(cdr sexpr))
	       `(const ,sexpr)))
	  
	  ((variable? sexpr)
	   `(var ,sexpr))
	  
	  ((if-dit? sexpr) `(if3 ,(parse (cadr sexpr)) 
				 ,(parse (caddr sexpr)) 
				 ,(parse void)))
	  
      	  ((if-dit-dif? sexpr) `(if3 ,(parse (cadr sexpr))
				     ,(parse (caddr sexpr)) 
				     ,(parse (cadddr sexpr))))
	  
      	  ((empty-or? sexpr) (parse #f))
	  ((single-or? sexpr) (parse (cadr sexpr)))
      	  ((or? sexpr) 
	   `(or (,@(map parse (cdr sexpr)))))
	  
	  ((applic? sexpr)
	   `(applic ,(parse (car sexpr)) (,@(map parse
						 (cdr sexpr)))))
	  
	  ((lambda-simple? sexpr) 
	   (let ((rem-dup (remove-duplicates (cadr sexpr))))
	     (if (< (length rem-dup) (length (cadr sexpr)))
		 (error 'parse "Duplicated argument names in lambda-simple")
		 `(lambda-simple ,(cadr sexpr) ,(parse `(begin ,@(cddr sexpr)))))))
	  
      	  ((lambda-opt? sexpr)
	   (let* ((splitted-vars (split$ (cadr sexpr) list))
		  (rem-dup (filter (lambda (mem)
				     (not (eq? mem (cadr splitted-vars))))
				   (car splitted-vars))))
	     (if (< (length rem-dup) (length (car splitted-vars)))
		 (error 'parse "Duplicated argument names in lambda-opt")
		 `(lambda-opt ,@(split$ (cadr sexpr) list) ,(parse `(begin ,@(cddr sexpr)))))))
      	  
      	  ((lambda-var? sexpr) 
	   `(lambda-opt () ,(cadr sexpr) ,(parse `(begin ,@(cddr sexpr)))))
	  
      	  ((begin? sexpr) 
	   (cond ((null? (cdr sexpr))
		  (parse void))
		 ((= (length (cdr sexpr)) 1)
		  (parse (cadr sexpr)))
		 (else `(seq (,@(map parse (filter-begin (cdr sexpr))))))))
	  
	  ((define-simple? sexpr) 
	   `(define ,(parse (cadr sexpr)) ,(parse (caddr sexpr))))
	  ((define-MIT? sexpr) 
	   `(define ,(parse (caadr sexpr)) ,(parse `(lambda ,(cdadr sexpr) ,(caddr sexpr)))))
	  
	  ((set!? sexpr)
	   `(set (var ,(cadr sexpr)) ,(parse (caddr sexpr))))

	  ((empty-and? sexpr) (parse '#t))
	  ((single-and? sexpr) (parse (cadr sexpr)))
	  ((multiple-and? sexpr) (parse `(if ,(cadr sexpr)
					     (and ,@(cddr sexpr))
					     #f)))

	  ((let? sexpr)
	   (let ((rem-dup (remove-dup-sets (cadr sexpr))))
	     (if (< (length rem-dup) (length (cadr sexpr)))
		 (error 'parse "Duplicated argument names in let")
		 (parse `((lambda ,(map car (cadr sexpr)) (begin ,@(cddr sexpr))) ,@(map cadr (cadr sexpr)))))))
	  
	  ((let*? sexpr)
	   (if (= (length (cadr sexpr)) 1)
	       (parse (cons 'let (cdr sexpr)))
	       (parse `(let (,(caadr sexpr))
			 (let* ,(cdadr sexpr)
			   (begin ,@(cddr sexpr)))))))
	  
	  ((letrec? sexpr)
	   (let ((rem-dup (remove-dup-sets (cadr sexpr))))
	     (if (< (length rem-dup) (length (cadr sexpr)))
		 (error 'parse "Duplicated argument names in letrec")
		 (parse `(let ,(map (lambda (var)
				      (list var '#f)) 
				    (map car (cadr sexpr)))
			   (begin ,@(map (lambda (pair)
					   (list 'set! (car pair) (cadr pair)))
					 (cadr sexpr))
				  ((lambda () ,@(cddr sexpr)))))))))
	  
	  
	  ((quasiquote? sexpr)
	   (parse (expand-qq (cadr sexpr))))
	  
	  ((cond? sexpr)
	   (let* ((clauses (cond-clauses sexpr))
		  (first-clause (cond-first-clause clauses)))
	     ;; (cond (else result1 .. resultn))
	     (cond ((and (= (length sexpr) 2)
			 (eq? 'else (car first-clause)))
		    (if (null? (cdr first-clause))
			(error 'parse "Missing arguments in else clause in cond")
			(parse `(begin ,@(cdr first-clause)))))
		   
		   ((and (>= (length sexpr) 2)
			 (not (eq? 'else (car first-clause)))
			 (null? (cdr first-clause)))
		    (if (cond-last-clause? clauses)
			;;(cond (test))
			(parse (car first-clause))
			;;(cond (test) clause1 clause2)
			(parse `(let ((temp ,(car first-clause)))
				  (if temp
				      temp
				      (cond ,@(cond-rest-clauses clauses)))))))
		   
		   ;; (cond (test result1 result2...))
		   ((and (= (length sexpr) 2)
			 (not (eq? 'else (car first-clause))))                		
		    (parse `(if ,(car first-clause) (begin ,@(cdr first-clause)))))
		   
		   ;; (cond clause1 clause2 ...)
		   ((and (> (length sexpr) 2)
			 (not (eq? 'else (car first-clause))))               	    
		    (parse `(if ,(car first-clause) (begin ,@(cdr first-clause)) (cond ,@(cddr sexpr))))))))
	  
	 
	  
	  
	  (else (error 'parse "Failed to parse")))
	   ))
