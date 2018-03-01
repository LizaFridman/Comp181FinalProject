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
	      ;;(display (format "Pipelining ~a\n" expr))
	      (annotate-tc
	       (pe->lex-pe
		(box-set
		 (remove-applic-lambda-nil
		  (parse expr))))))
	    match))
     (lambda (fail) 'fail))))

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

(define create-code-to-run
  (lambda (sexprs)
    ;;(display (format "Sexprs: ~a\n" sexprs))
    (fold-left string-append
	       ""
	       (map (lambda (expr)
		      (string-append
		       (code-gen expr)
		       cg-print-rax))
		    sexprs))))

(define compile-scheme-file
  (lambda (source dest)
    (let* ((exprs (file->list source))
	   (built-in (file->list "Built-in.scm"))
	   (pipelined (pipeline (append built-in exprs)))
	   (size (length pipelined)))
      ;;(display (format "Pipelined = ~a\n" pipelined))
      ;;(display (format "Before c-table...\n"))
      (set! c-table (master-build-c-table pipelined 6))
      ;;(display (format "C-Table:\n~a\n" c-table))
      ;;(display (format "Before F-Table...\n"))
      (set! f-table (master-build-f-table pipelined))
      ;;(display (format "F-Table:\n~a\n" f-table))
      (let* ((pre (generate-pre-text c-table f-table))
	     (code (create-code-to-run pipelined)))
	;;(display (format "Pre-Text:\n~a\nCode:\n~b\n" pre code))
	(list->file (string->list (string-append pre
						 code
						 post-text))
		  dest)
      (display (format "Compiled Scheme file with ~a parsed expressions!\n" size))))))

;--------------------------------------------------| cTable |--------------------------------------------------------

(define T_UNDEFINED 0)
(define T_VOID 1)
(define T_NIL 2)
(define T_INTEGER 3)
(define T_FRACTION 4)
(define T_BOOL 5)
(define T_CHAR 6)
(define T_STRING 7)
(define T_SYMBOL 8)
(define T_CLOSURE 9)
(define T_PAIR 10)
(define T_VECTOR 11)


;  is empty or not a list   -> returns saved results
;  car passes test          -> save car, remove it and do on the rest
;  car is a list            -> open the car and do again
;  else                     -> remove car and do on the rest

(define improper-list?
	(lambda (x)
		(and (pair? x) (not (list? (last-pair x))))))
		
(define those-that-pass
  (lambda (exps test positive-results)
    ;;(display (format "Exps:\n ~a\nResults:\n~a\n" exps positive-results))
    (cond 
     ((or (not (pair? exps))
	  (null? exps))
      positive-results)
     
     ((test (car exps))
      ;;(display (format "==> Expression ~a Passed test ~a\n" (car exps) test))
      (those-that-pass (cdr exps) test (cons (car exps) positive-results)))
     
	 ((improper-list? (car exps))
     	(append (those-that-pass (car exps) test '()) (those-that-pass (cdr exps) test positive-results)))
	 
     ((pair? (car exps))
	   ;;(list? (caar exps)))
      (those-that-pass `(,@(car exps) ,@(cdr exps)) test positive-results))
     
     (else (those-that-pass (cdr exps) test positive-results)))))
;; returns deep search, returns elements that pass test
;; TODO: if exps passes test, do not go into the those-that-pass function

(define ordered-those-that-pass
  (lambda (exps test)
    ;;(display (format "Ordering:\nExprs: ~a\n" exps))
    (let ((passed (those-that-pass exps test '())))
      ;;(display (format "Passed: ~a\n" passed))
      (reverse (those-that-pass exps test '())))))

(define tagged-by-const
  (lambda (exp)
    (and (pair? exp) (equal? (car exp) 'const))))

(define extract-consts
  (lambda (exp)
    (if (tagged-by-const exp)
	(cdr exp)
	(map (lambda (x) (cadr x))
	     (ordered-those-that-pass exp tagged-by-const)))))

(define extract-and-topo-sort-consts
  (lambda (exp done)
    ;;(display (format "Extract & sort consts:\nExp: ~a\nDone: ~a\n" exp done))
    (if (null? exp) 
	done
	(let* ((sorted (topological-sort (car exp)))
	       (reversed (reverse sorted)))
	  ;;(display (format "Sorted: ~a\nReversed: ~a\n" sorted reversed))
	  (extract-and-topo-sort-consts (cdr exp)
					(append done
						reversed))))))

(define master-const-extract 
  (lambda (exp)
    (if (null? exp)
	'()
	(extract-and-topo-sort-consts (extract-consts exp) '()))))

(define float->integer-func ;not very efficient, #f if didnt find or fnum being integer
	(lambda (fnum guess)
		(cond ((equal? 0.0 (- fnum guess)) guess)
			  ((> guess fnum) #f)
			  (else (float->integer-func fnum (+ 1 guess)))))) 
(define float->integer ;not very efficient, #f if didnt find or fnum being integer
	(lambda (fnum)
		(float->integer-func fnum 0)))

(define add-to-c-table  ;returns (table . nextmem)
  (lambda (table element mem)
    (cond ((char? element)
	   ;; T_Char <index, value, (T_Char, value)>
	   (cons (append table
			 `((,mem ,element 
				 (,T_CHAR ,(char->integer element)))))
		 (+ 2 mem) ))
          ((integer? element)
	   ;; <index, value, (T_Integer, value)>
	   (cons (append table
			 `((,mem ,element
				 (,T_INTEGER ,element))))
		 (+ 2 mem)))
          ((rational? element)
	   ;; <index, value, (T_Fraction, num, denum)>
	   (let* ((top (numerator element))
		  (bottom (denominator element))
		  (topIndx (c-table-contains? table top))
		  (bottomIndx (c-table-contains? table bottom)))
	     ;;(display (format "Adding T_FRACTION to C-table: ~a Rational? ~a\n" element (rational? element)))
	     ;;(display (format "Num:~a\nDenum:~a\n" top bottom))
	     ;;(display (format "Num Index:~a\nDenum Index:~a\n" topIndx bottomIndx))
             (cond ((and topIndx bottomIndx)
		    ;;has both ints -> add fraction          
		    (cons (append table `((,mem ,element
						(,T_FRACTION ,topIndx ,bottomIndx))))
			  (+ 3 mem)))
		   (topIndx										 ;;has only numerator -> add denominator and do again
		    (add-to-c-table (car (add-to-c-table table bottom mem))
				    element
				    (cdr (add-to-c-table table bottom mem))))
		   (else
		    ;;has only maybe the denominator -> add numerator and do again 
		    (add-to-c-table (car (add-to-c-table table top mem))
				    element
				    (cdr (add-to-c-table table top mem)))))))
          ((string? element)
	   ;; <index, value, (T_STRING, length, ASCII-list)>
	   (cons (append table
			 `((,mem ,element
				 (,T_STRING ,(string-length element) ,(map char->integer (string->list element))))))
		 (+ mem (string-length element) 2))) 
          ((symbol? element)
	   ;; <index, symbol, (T_Symbol, string)>
           (let ((rep-str (symbol->string element)))
             (if (c-table-contains? table rep-str)
                 (cons (append table
			       `((,mem ,element (,T_SYMBOL ,(c-table-contains? table rep-str)))))
		       (+ 2 mem))
                 (add-to-c-table (car (add-to-c-table table rep-str mem))
				 element
				 (cdr (add-to-c-table table rep-str mem))))))
          ((pair? element)
	   ;; <index, value, (T_PAIR, car-index, cdr-index)>
	   (let ((carIndex (c-table-contains? table (car element)))
		 (cdrIndex (c-table-contains? table (cdr element))))
	     (cond ((and carIndex cdrIndex)
		    (cons (append table
				  `((,mem ,element (,T_PAIR ,carIndex ,cdrIndex))))
			  (+ 3 mem)))
		   (carIndex
		    (add-to-c-table (car (add-to-c-table table (second element) mem))
				    element
				    (cdr (add-to-c-table table (second element) mem))))
		   (else (add-to-c-table (first (add-to-c-table table (first element) mem))
					 element
					 (second (add-to-c-table table (first element) mem)))))))
	  
          ((vector? element)
	   ;; <index, value, (T_Vector, length, index-list-of-elements)>
	   (cons (append table
			 `((,mem ,element (,T_VECTOR
					   ,(vector-length element)
					   ,(map (lambda (x)
						   (c-table-contains? table x))
						 (vector->list element))))))
		 (+ mem (vector-length element) 2)))
          (else 'error))))

(define last-mem
  (lambda (table starting-mem)
    (if (null? table) starting-mem (caar (last-pair table)))))

(define c-table-contains? ;returns adress
  (lambda (table element)
    (cond ((null? table) #f)
          ((equal? element (cadar table)) (caar table))
          (else (c-table-contains? (cdr table) element)))))

(define build-c-table-func
  (lambda (table lst mem)
    ;;(display (format "Building C-Table function:\nTable: ~a\nList: ~a\n" table lst))
    (cond  ((null? lst)
	    table)
           ((c-table-contains? table (car lst))
	    (build-c-table-func table (cdr lst) mem))
           (else
	    (let* ((new-table (car (add-to-c-table table (car lst) mem)))
		   (new-mem (cdr (add-to-c-table table (car lst) mem))))
	      (build-c-table-func new-table (cdr lst) new-mem))))))


(define starting-table
  (lambda (mem)
    `((,mem ,(if #f #f) (,T_VOID)) (,(+ 1 mem) () (,T_NIL)) (,(+ mem 2) #f (,T_BOOL 0)) (,(+ mem 4) #t (,T_BOOL 1)))))


(define build-c-table
  (lambda (lst starting-mem)
    ;;(display (format "Building c-table with:\nLst: ~a\n" lst))
    (build-c-table-func
     (starting-table (- starting-mem 6))
     lst
     starting-mem)))

(define topological-sort 
  (lambda (e)
    ;;(display (format "Topological Sort of ~a\n" e))
    (cond 
     ((or (number? e) (string? e) (eq? e (if #f #f)) (null? e) (boolean? e) (char? e) ) `(,e)) 
     ((pair? e) 
      `(,e ,@(topological-sort (car e)) ,@(topological-sort (cdr e))))
     ((vector? e) 
      `(,e ,@(apply append 
		    (map topological-sort (vector->list e)))))
     ((symbol? e)
      `(,e ,@(topological-sort (symbol->string e))))
     (else 'topological-sort-error))))

(define master-build-c-table
  (lambda (exp mem)
    (build-c-table (master-const-extract exp) mem)))

(define c-table-getLine
  (lambda (table element)
      (cond ((null? table)
	     #f)
	    ((equal? element (second (first table)))
	     (first table))
	    (else
	     (c-table-getLine (cdr table) element)))))

(define c-table-getLine-byType
  (lambda (table element type)
      (cond ((null? table)
	     #f)
	    ((and (equal? element (second (first table)))
		  (equal? type (first (third table))))
	     (first table))
	    (else
	     (c-table-getLine (cdr table) element type)))))

;;a.k.a:  c-table[i] =
(define c-table '())
(define const-label "L_const")
(define fvar-label "L_global")

(define CHAR_NUL 0)
(define CHAR_TAB 9)
(define CHAR_NEWLINE 10)
(define CHAR_PAGE 12)
(define CHAR_RETURN 13)
(define CHAR_SPACE 32)

(define cg-c-table
  (lambda (ct)
    ;;(display (format "Generating C-Table...\n~a\n" ct))
    (fold-left string-append
	       (list->string '())
	       (map (lambda (row)
		      ;; Row = <Index, Value, (Type, Type-Data)>
		      (let* ((index (first row))
			     (value (second row))
			     (data (third row))
			     (type (first data))
			     (type-data (cdr data)))
			(cond
			 ((equal? T_VOID type)
			  (cg-T-void index))
			 ((equal? T_NIL type)
			  (cg-T-nil index))
			 ((equal? T_INTEGER type)
			  (cg-T-integer value index))
			 ((equal? T_FRACTION type)
			  (cg-T-fraction (first type-data) (second type-data) index))
			 ((equal? T_BOOL type)
			  (cg-T-bool (first type-data) index))
			 ((equal? T_CHAR type)
			  (cg-T-char value index))
			 ((equal? T_STRING type)
			  (cg-T-string (second data) (third data) index))
			 ((equal? T_SYMBOL type)
			  (cg-T-symbol (first type-data) index))
			 ((equal? T_PAIR type)
			  (cg-T-pair (first type-data) (second type-data) index))
			 ((equal? T_VECTOR type)
			  (cg-T-vector (first type-data) (second type-data) index))
			 (else (number->string T_UNDEFINED)))))
		    ct)
	       )))

(define make-const-label
  (lambda (index)
    (string-append const-label (number->string index) ":" newLine)))

(define make-fvar-label
  (lambda (index)
    (string-append fvar-label (number->string index) ":" newLine)))

(define cg-T-void
  (lambda (index)
    (string-append (make-const-label index)
		   tab "dq SOB_VOID" newLine)))

(define cg-T-nil
  (lambda (index)
    (string-append (make-const-label index)
		   tab "dq SOB_NIL" newLine)))

(define cg-T-bool
  (lambda (value index)
    (let ((true (equal? value 1)))
      (string-append (make-const-label index)
		     tab (if true
			     "dq SOB_TRUE" 
			     "dq SOB_FALSE")
		     newLine))))

(define get-T-char-value
  (lambda (value)
    (let ((val (char->integer value)))
      ;;(display (format "Search char value: ~a\nintValue = ~a\n" value val))
      (cond ((equal? val (char->integer #\newline))
	     "CHAR_NEWLINE")
	    ((equal? val (char->integer #\"))
	     "\'\"\'")
	    ((equal? val (char->integer #\\))
	     "\'\\\'")
	    ((equal? val (char->integer #\tab))
	     "\'\t'")
	    (else (string-append "\'" (string value) "\'"))))))

(define cg-T-char
  (lambda (value index)
    ;;(display (format "c-gen to T_CHAR: ~a\n" value))
    (string-append (make-const-label index)
		   tab "dq MAKE_LITERAL(T_CHAR, " (get-T-char-value value) ")" newLine)))

(define cg-T-integer
  (lambda (value index)
    (string-append (make-const-label index)
		   tab "dq MAKE_LITERAL(T_INTEGER, " (number->string value) ")" newLine)))

(define cg-T-fraction
  (lambda (numIndx denumIndx index)
    (string-append (make-const-label index)
		   tab "dq MAKE_LITERAL_FRACTION(" const-label (number->string numIndx) ", " const-label (number->string denumIndx) ")" newLine)))

(define append-params
  (lambda (params)
    (fold-left (lambda (result current)
		 (string-append result
				(if (not (equal? current (first params)))
				    ", "
				    " ")
				(number->string current)))
	       ""
	       params)))

(define cg-T-string
  (lambda (length chars index)
    ;;(display (format "generating string const = ~a\n" chars))
    (string-append
     (make-const-label index)
     tab "MAKE_LITERAL_STRING" (append-params chars) newLine)))

(define cg-T-symbol
  (lambda (stringIndex index)
      (string-append (make-const-label index)
		     tab "MAKE_LITERAL_SYMBOL " (string-append
						const-label
						(number->string stringIndex))
		     newLine)))

(define cg-T-pair
  (lambda (carIndex cdrIndex index)
    ;;(display (format "Generating Pair: carIndx: ~a cdrIndx: ~a\n" carIndex cdrIndex))
    (string-append (make-const-label index)
		   tab "dq MAKE_LITERAL_PAIR(" const-label (number->string carIndex) ", " const-label (number->string cdrIndex) ")" newLine)))

(define cg-T-vector
  (lambda (length items index)
    (string-append
     (make-const-label index)
     tab "MAKE_LITERAL_VECTOR " (append-params items) newLine)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  F-Table  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;row = <Var-name, Index>
;(define f-table '())

;(define f-table-contains?
 ; (lambda (var ft)
 ;   (let ((row (first ft)))
 ;     (cond ((null? ft)
;	     #f)
;	    ((equal? var (first row))
;	     (second row))
;	    (else (f-table-contains? var (cdr ft)))))))

;; Returns a list of all fvar-values
;(define extract-fvars
  ;(lambda (pe)
 ;   (cond ((not (pair? pe))
;	   '())
;	  ((tag? 'fvar pe)
;	   (cdr pe))
;	  (else
;	   `(,@(extract-fvars (first pe)) ,@(extract-fvars (cdr pe)))))))

;(define build-f-table
;  (lambda (pe ft index)
;    (add-to-f-table (remove-duplicates (extract-fvars pe)) ft index)))

;(define add-to-f-table
;  (lambda (vars ft index)
 ;     (cond ((null? vars)
;	     ft)
;	    ((f-table-contains? (first vars) ft)
;	     (add-to-f-table (cdr vars) ft index))
;	    (else
;	     (add-to-f-table (cdr vars)
;			     `(,@ft `(,(first vars) ,index))
;			     (+ index 1))))))

(define f-table-get-func ;; index
	(lambda (table element)
		(cond ((null? table) #f) 
			  ((equal? (second (car table)) element) (first (car table)))
			  (else (f-table-get-func (cdr table) element)))))

(define f-table-get ;; index
	(lambda (element)
		(f-table-get-func f-table element)))
		

(define tagged-by-fvar
  (lambda (exp)
    (and (pair? exp) (equal? (car exp) 'fvar))))

(define extract-fvars
  (lambda (exp)
    ;;(display (format "Extracting F-vars from ~a\n" exp))
    (if (tagged-by-fvar exp)
	(cdr exp)
	(map (lambda (x) (second x))
	     (ordered-those-that-pass exp tagged-by-fvar)))))

(define f-table-contains? ;input is list of fvars, not the final table
  (lambda (table element)
    (cond ((null? table) #f)
          ((equal? (car table) element) element)
          (else (f-table-contains? (cdr table) element)))))

(define f-table-add 
  (lambda (table element)
    (if (f-table-contains? table element)
	table
	(append table (list element)))))

(define build-f-table
  (lambda (table lst)
    (if (null? lst)
	table
	(build-f-table (f-table-add table (car lst)) (cdr lst)))))

(define give-indxes
  (lambda (after before indx)
    (if (null? before)
	after
	(give-indxes (cons (list indx (car before)) after) (cdr before) (+ 1 indx)))))

(define master-give-indxes
	(lambda (lst)
		(reverse (give-indxes '() lst 0))))
	
(define master-give-indxes ;needs a list - not a single element
  (lambda (lst)
    (reverse (give-indxes '() lst 0))))

(define master-build-f-table
  (lambda (exp)
    (let ((extracted (extract-fvars exp)))
      ;;(display (format "Extracted f-vars: ~a\n" extracted))
	(master-give-indxes (build-f-table '() extracted)))))

(define cg-f-table
  (lambda (table)
    ;;(display (format "Generating code for f-table:\n~a\n" table))
    (fold-left string-append
	       ""
               (map (lambda (line)
		      ;;(display (format "Handling line ~a\n" line))
		      (string-append
		       fvar-label (number->string (first line)) ":" newLine
		       tab "dq MAKE_LITERAL(T_UNDEFINED, 0)" newLine))
                    table))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Symbol Table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define symbol_count -1)
(define symbol-label "symbol")

(define get-symbols-func
	(lambda (table passed)
		(cond ((null? table) passed)
		      ((symbol? (cadar table)) (get-symbols-func (cdr table) (append (list (cadar table)) passed)))
		      (else (get-symbols-func (cdr table) passed)))))

(define get-symbols
	(lambda (table)
		(get-symbols-func table '())))


(define symb-table-make-func
	(lambda (lst indx)
		(set! symbol_count (+ 1 symbol_count))
		(if (null? lst)
			""
			(string-append 
			symbol-label (number->string indx) ": \n"
			"MAKE_LITERAL_SYMBOL " (string-append const-label (number->string (c-table-contains? c-table (symbol->string (car lst))))) "\n" 
			(symb-table-make-func (cdr lst) (+ 1 indx))
			))
		))
(define symb-table-make
	(lambda (lst)
		(symb-table-make-func lst 0)))

(define make-linked-symb-list-func 
	(lambda (indx)
		(if (= -1 indx) ""
		(string-append
		"mov rbx, "(string-append symbol-label (number->string indx)) "\n"		;rax has the last pair
		"MAKE_MALLOC_LITERAL_PAIR rax, rbx, rax\n"
		(make-linked-symb-list-func (- indx 1))
		) 
		)))
(define make-linked-symb-list
	(lambda ()
		(string-append
		 "mov rax, " (sobNull) "\n"
		(make-linked-symb-list-func (- symbol_count 1) ) ;symbol_count
		)))

;(define master-symbol-builder
;	(lambda (table) ;c-table
;			(let* ((first (symb-table-make (get-symbols table)))
;				(second (make-linked-symb-list)))			
			;)))

;(define fetch-symbol
;	(lambda (x)
;		))





;(define make-linked-list
;	(lambda ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Code Generation  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tag?
  (lambda (tag pe)
    (and (list? pe)
	 (equal? tag (first pe)))))

(define code-gen
  (lambda (pe)
    ;; After each generation, the value of the generated code is in RAX
    ;; Returns string
    ;;(display (format "Code Gen to ~a\n" pe))
    (string-append ;;";" (format "~a\n" pe)
     (cond ((tag? 'const pe)
	    (cg-const (second pe)))
	   
	   ((tag? 'pvar pe)
	    (cg-pvar pe))
	   
	   ((tag? 'bvar pe)
	    ;;(bvar x major minor)
	    (cg-bvar (third pe) (forth pe)))
	   
	   ((tag? 'fvar pe)
	    (cg-fvar (second pe)))
	   
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
	   
	   ((tag? 'lambda-simple pe)
	    (cg-lambda-simple pe))
	   
	   ((tag? 'lambda-opt pe)
	    (cg-lambda-opt pe))
	   
	   ((tag? 'define pe)
	    ;; (define (*var var) value)
	    (cg-define (second (second pe)) (third pe)))
	   
	   ((or (tag? 'applic pe)
		(tag? 'tc-applic pe))
	    (cg-applic (second pe) (third pe)))
	   
	   ((tag? 'tc-applic pe)
	    (string-append ";" (format "~a" pe) newLine))
	   
	   ((tag? 'set pe)
	    ;;(set! (*var var * *) value)
	    (let* ((var (second pe))
		   (value (third pe))
		   (cg-val (code-gen value)))
	      (string-append cg-val
			     (cond ((tag? 'bvar var)
				    (cg-set-bvar (cdr var)))
				   ((tag? 'pvar var)
				    (cg-set-pvar (cdr var)))
				   ((tag? 'fvar var)
				    (cg-set-fvar (cdr var)))
				   (else
				    (string-append
				     "Undefined set variable"
				     newLine)))
			     tab "MOV rax, " sobVoid newLine)))
	   
	   ((tag? 'box pe)
	    (cg-box (second pe)))
	   
	   ((tag? 'box-get pe)
	    (cg-box-get (second pe)))
	   
	   ((tag? 'box-set pe)
	    ;;(box-set! (*var var * *) value)
	    (let ((var (second pe))
		  (value (third pe))
		  (cg-value (code-gen value)))
	      (string-append cg-value
			     (cond ((tag? 'bvar var)
				    (cg-box-set-bvar (cdr var)))
				   ((tag? 'pvar var)
				    (cg-box-set-pvar (cdr var)))
				   ((tag? 'fvar var)
				    (cg-box-set-fvar (cdr var)))
				   (else
				    (string-append
				     "Undefined box-set variable type"
				     newLine)))
			     tab "MOV rax, sobVoid" newLine)))
	   (else
	    (string-append ";;Code-Generation-Error" newLine))))))

(define newLine
  (list->string '(#\newline)))

(define tab
  (list->string '(#\tab)))

(define labelIndex 0)

(define make-label
  (lambda (name)
    (set! labelIndex (+ labelIndex 1))
    (string-append name (number->string labelIndex))))

(define cg-print-rax
  (string-append
   newLine
   tab "PUSH qword [RAX]" newLine
   tab "call write_sob_if_not_void" newLine
   tab "ADD rsp, 1*8" newLine))

(define cg-print-symbol
   (string-append
   tab "PUSH RAX" newLine
   tab "call write_sob_symbol" newLine
   tab "ADD rsp, 1*8" newLine))

;;; Const

(define cg-const
  (lambda (const)
    (let* ((row (c-table-getLine c-table const))
	   (index (first row))
	   (type (first (third row))))
      (string-append ;;".t_" const-label (number->string index) ":" newLine
		     tab "MOV RAX, " const-label (number->string index) newLine
		     ;;newLine
		     ;;cg-print-rax)
		     ))))

;;; Or

(define cg-or
  (lambda (lst end-label)
    ;;(display (format "sobFalse = ~a\n" (sobFalse)))
      (cond ((null? lst)
	     (list->string '()))
	    ((null? (cdr lst))
	     (let ((cg-N (code-gen (first lst))))
	       (string-append cg-N newLine
			      end-label ":" newLine)))
	    (else
	     (let ((cg-i (code-gen (first lst))))
	       (string-append cg-i newLine
			      tab "CMP RAX, " (sobFalse)
			      newLine
			      tab "JNE " end-label newLine
			      (cg-or (cdr lst) end-label)))))))
;;; Vars

(define cg-pvar
  (lambda (pe)
    (let ((minor (third pe)))
      (string-append
       tab "MOV RAX, qword [rbp + " (number->string (+ minor 4)) "*8]" newLine))))

(define cg-bvar
  (lambda (major minor)
    (string-append
     tab "MOV RAX, qword [rbp + 2*8]" newLine
     tab "MOV RAX, qword [RAX + " major "*8]" newLine
     tab "MOV RAX, qword [RAX + " minor "*8]" newLine)))

(define cg-fvar ;var needs to be the symbol
  (lambda (var)
    (string-append
    tab "MOV RAX, " fvar-label (number->string (f-table-get var)) newLine)))

;;; If3
(define sobNull
	(lambda ()
		(string-append const-label (number->string (c-table-contains? c-table '())))))

(define sobFalse
  (lambda ()
    (string-append const-label (number->string (c-table-contains? c-table #f)))))

(define sobVoid (string-append const-label "0"))

(define cg-if3
  (lambda (test dit dif)
    (let ((test-cg (code-gen test))
	  (dit-cg (code-gen dit))
	  (dif-cg (code-gen dif))
	  (l-dif (make-label "L_ifDif"))
	  (l-end (make-label "L_ifEnd")))
      
      (string-append test-cg newLine
		     tab "MOV RBX, " (sobFalse) newLine
		     tab "CMP RAX, RBX" newLine
		     tab "JE " l-dif newLine
		     dit-cg newLine
		     tab "JMP " l-end newLine
		     l-dif ":" newLine
		     dif-cg newLine
		     l-end ":" newLine
		     ))))
;;; Seq

(define cg-seq
  (lambda (pe)
    (fold-left (lambda (result e)
    ;;(display (format "cg-seq: e = ~a\nresult = ~b\n" e result))
		 (string-append result (code-gen e) newLine))
	       ""
	       pe)))
;;; Set

(define cg-set-bvar
  (lambda (var major minor)
    (string-append
     tab "MOV rbx, qword [rbp + 2*8]" newLine
     tab "MOV rbx, qword [rbx + " major "*8]" newLine
     tab "MOV rbx, qword [rbx + " minor "*8]" newLine
     tab "MOV qword [rbx], rax" newLine)))

(define cg-set-pvar
  (lambda (var minor)
    (string-append
     tab "MOV qword [rbp + " (+ 4 minor) "*8], RAX" newLine)))

(define cg-set-fvar
  ;;(set! (fvar var) value)
  ;; RAX = [|value|]
  (lambda (var)
    (string-append
     tab "MOV qword [" fvar-label (number->string (f-table-get var)) "], RAX" newLine)))

;;; Box

(define malloc
  (lambda (size)
    (string-append
     tab "PUSH rdi" newLine
     tab "MOV rdi, " (number->string (* size 8)) newLine
     tab "call malloc" newLine
     tab "POP rdi" newLine)))

(define cg-box
  ;; Rax = address of var
  (lambda (var)
    (string-append
     (code-gen var) ;; Rax - var
     tab "PUSH rbx" newLine
     tab "PUSH rcx" newLine
     tab "PUSH rdx" newLine
     tab "PUSH rbp" newLine
     tab "PUSH rsi" newLine
     newLine
     tab "MOV rbx, rax" newLine
     (malloc 1)
     tab "MOV qword [rax], rbx" newLine
     newLine
     tab "POP rsi" newLine
     tab "POP rbp" newLine
     tab "POP rdx" newLine
     tab "POP rcx" newLine
     tab "POP rbx" newLine
     )))

(define cg-box-get
  (lambda (var)
    (string-append
     (code-gen var) ;; Rax = var value
     tab "MOV rax, [rax]" newLine ;; Unbox
     )))

(define cg-box-set-bvar
  (lambda (var major minor)
    (string-append
     tab "MOV rbx, qword [rbp + 2*8]" newLine
     tab "MOV rbx, qword [rbx + " major "*8]" newLine
     tab "MOV rbx, qword [rbx + " minor "*8]" newLine
     tab "MOV qword [rbx], rax" newLine
     )))

(define cg-box-set-pvar
  (lambda (var minor)
    (string-append
     tab "MOV rbx, [rbp + " (+ 4 minor) "*8]" newLine
     tab "MOV [rbx], rax" newLine)))

(define cg-box-set-fvar
  (lambda (var-name)
    (string-append
     tab "MOV rbx, [" fvar-label (number->string (f-table-get var-name)) "]" newLine
     tab "MOV [rbx], rax" newLine)))

;;; Applic

(define cg-push-args
  (lambda (args)
    (fold-left (lambda (result arg)
		 (string-append result
				(code-gen arg)
				tab "PUSH rax" newLine))
	       ""
	       (reverse args))))

(define cg-pop-args
  (lambda (args)
    (fold-left (lambda (result arg)
		 (string-append result
				tab "POP rbx" newLine))
	       ""
	       args)))

(define applic-T-closure-error-label "L_Applic_closure_error")
(define error-applic-label "L_error_applic")

(define cg-check-T-closure
  ;; Rax = generated closure
  (lambda (continue-label)
    (string-append
     tab "MOV RAX, qword [RAX]" newLine
     tab "MOV RAX, qword [RAX]" newLine
     tab "TYPE rax" newLine
     tab "CMP rax, T_CLOSURE" newLine
     tab "JE " continue-label newLine
     tab "POP rax" newLine
     tab "POP rbx" newLine
     tab "MOV rax, " error-applic-label newLine
     cg-print-rax
     tab "JMP L_exit" newLine)))

(define applic-num 0)

(define cg-applic
  (lambda (proc args)
    (let* ((args-length (length args))
	   (string-length (number->string args-length))
	   (pass-label (begin
			 (set! applic-num (+ 1 applic-num))
			 (string-append ".closure_"
					(number->string applic-num)
					"_check_passed"))))
	   ;;(display (format "Code-gen Applic:\nargs-length = ~a\nString-length = ~a\n" args-length  string-length))
      (string-append
       (cg-push-args args)
       newLine
       tab "XOR rbx, rbx" newLine
       tab "MOV rbx, " string-length newLine
       tab "PUSH rbx" newLine ;; num of args
       ;;tab "PUSH " (number->string args-length) newLine
       newLine
       (code-gen proc) ;;Rax = Closure value
       newLine
       tab "PUSH rax" newLine
       (cg-check-T-closure pass-label)
       newLine
       pass-label ":" newLine
       tab "POP rax" newLine
       tab "MOV rax, [rax]" newLine
       tab "MOV rax, [rax]" newLine
       tab "MOV rbx, rax" newLine
       tab "CLOSURE_ENV rbx" newLine
       tab "PUSH rbx" newLine ;; Env
       tab "CLOSURE_CODE rax" newLine
       tab "call rax" newLine
       
       tab "POP rbx" newLine ;; Closure_Env
       tab "POP rbx" newLine ;; Args-length
       (cg-pop-args args);; Pop Arguments
       ))))

;;; Define

(define cg-define
  (lambda (var value)
    ;;(display (format "f-table::\n~a\n" f-table))
    ;;(display (format "cg-define:\nVar = ~a\nValue = ~a\n" var value))
    (let ((address (number->string (f-table-get var))))
      ;;(display (format "Address for (~a ~a) is ~a\n" var value address))
      (string-append (code-gen value)
		     newLine
		     tab "MOV qword [" fvar-label address "], RAX" newLine
		     tab "MOV RAX, " sobVoid newLine))))

(define lexical_env -1)

(define arg_count "arg_count")

(define cg-lambda-simple
  (lambda (pe)
    (set! lexical_env (+ lexical_env 1))
    (let* ((args (cadr pe))
	   (body (caddr pe))
	   (skip_code_label (make-label "skip_code"))
	   (for_copy_args (make-label "for_copy_args"))
	   (end_of_copy_args (make-label "end_of_copy_args"))
	   (for_copy_envs (make-label "for_copy_envs"))
	   (end_of_copy_envs (make-label "end_of_copy_envs"))
	   (code_label (make-label "code"))
	   (new_env (make-label "new_env"))
	   (str-gen (string-append   
		     ;;create new env
		     tab "mov rbx, 0" newLine;env
		     tab "mov rax, " (number->string lexical_env) newLine;major
		     tab "cmp rax, 0" newLine
		     tab "je " end_of_copy_envs newLine
		     newLine
		     tab "mov rdi, "(number->string (* 8 (+ 1 lexical_env))) newLine;for allocating space for new extended env 
		     tab "call malloc" newLine
		     ".after_malloc1:" newLine
		     tab "mov rbx, rax"	newLine ;;rbx = malloc(8*(n+1)) *this is x*
		     newLine
		     tab "XOR rax, rax" newLine
		     tab "mov rax, " arg_count newLine
		     tab "mov rdi, 8" newLine
		     tab "mul rdi" newLine
		     tab "PUSH rbx" newLine	;save value of rbx 
		     tab "mov rdi, rax" newLine
		     tab "call malloc" newLine
		     ".after_malloc2:" newLine
		     tab "POP rbx" newLine
		     tab "mov rcx, rax"	newLine
		     ;;rcx = malloc(8*m) *params of lambda*
		     ;;copy arguments into rcx
		     newLine
		     tab "mov rdi, 0" newLine
		     for_copy_args":" newLine
		     tab "cmp rdi, " arg_count newLine
		     tab "je " end_of_copy_args newLine
		     newLine
		     tab "mov rax, 8" newLine
		     tab "mul rdi" newLine
		     tab "mov rdx, An(rdi)" newLine  ; rdx = i'th argument
		     tab "mov qword [rcx+rax], rdx" newLine ;; copy arg i into [rcx+8*i]
		     newLine
		     tab "inc rdi" newLine
		     tab " jmp "for_copy_args newLine
		     newLine
		      end_of_copy_args":" newLine
		     tab "mov qword [rbx], rcx" newLine
		     tab "mov r14, env"	newLine	;; rdx=previous env
		     tab "cmp r14, 0" newLine
		     tab "je "end_of_copy_envs"\n"
		     tab "mov rdi, 0\n"
		     newLine
		     for_copy_envs":\n"
		     tab "cmp rdi, " (number->string lexical_env) "\n"
		     tab "je "end_of_copy_envs"\n"
		     newLine
		     tab "mov rax, 8\n"
		     tab "mul rdi\n"
		     tab "mov rcx, qword [r14+rax]\n" ; rcx = i'th env
		     tab "mov qword [rbx+rax+8], rcx\n" ; copy env i into [rbx+8*i+8]
		     tab "inc rdi\n"
		     tab "jmp "for_copy_envs"\n"
		     newLine
		     end_of_copy_envs":\n"
		     ;;create target
		     tab "PUSH rbx\n"
		     tab "PUSH rcx\n"
		     tab "mov rdi, 16\n"
		     tab "call malloc\n" ;rax = malloc(8*2)
		     ".after_malloc3:" newLine
		     tab "pop rcx\n"
		     tab "pop rbx\n"
		     newLine
		     tab "push rdx\n"
		     tab "mov rdx, "code_label "\n"
                   ; "MAKE_LITERAL_CLOSURE rax, rbx, "code_label "\n"
		     tab "MAKE_LITERAL_CLOSURE rax, rbx, rdx \n"
		     ".after_make_closure:" newLine
		     tab "pop rdx\n"

		     tab "jmp "skip_code_label"\n"
		     newLine
		     ;;create code
		     code_label":\n"
		     tab "push rbp\n"
		     tab "mov rbp, rsp\n"
		     newLine
		     tab (code-gen body)
		     tab "mov rbx, rax\n"
		     tab "mov rax, " arg_count newLine
		     tab "add rax, 1\n"
		     tab "mov rdi, 8\n"
		     tab "mul rdi\n"
		     tab "add rsp, rax\n"
		     tab "mov rax, rbx\n"
		     newLine
		     tab "leave\n"
		     tab "ret\n"
		     newLine
		     skip_code_label":\n")))
      (set! lexical_env (- lexical_env 1)) 
      str-gen)))

(define cg-lambda-opt
  (lambda (pe)
    (set! lexical_env (+ lexical_env 1))
    (let* ((args (cadr pe))
	   (body (cadddr pe))
	   (skip_code_label (make-label "skip_code_lbl")) (for_copy_args (make-label "for_copy_args_lbl")) (end_of_copy_args (make-label "end_of_copy_args_lbl"))
	   (for_copy_envs (make-label "for_copy_envs_lbl")) (end_of_copy_envs (make-label "end_of_copy_envs_lbl")) (code_label (make-label "code_lbl")) (new_env (make-label "new_env_lbl"))
	   (for_fix_stack (make-label "for_fix_stack_lbl")) (end_of_fix_stack (make-label "end_of_fix_stack_lbl")) (dont_push_arg_label (make-label "dont_push_lbl"))
	   (str-gen (string-append
					;create new env
		     "mov rbx, 0\n";env
		     "mov rax, " (number->string lexical_env) "\n";major
		     "cmp rax, 0\n"
		     "je "end_of_copy_envs"\n"
		     "mov rdi, "(number->string (* 8 (+ 1 lexical_env)))"\n";for allocating space for new extended env 
		     "call malloc\n"
		     "mov rbx, rax\n"	;rbx = malloc(8*(n+1)) *this is x*
		     
		     "mov rax, arg_count\n"
		     "mov rdi, 8\n"
		     "mul rdi\n"
		     "push rbx\n"	;save value of rbx 
		     "mov rdi, rax\n"
		     "call malloc\n"
		     "pop rbx\n"
		     "mov rcx, rax\n"	;rcx = malloc(8*m) *params of lambda*
		     
					;copy arguments into rcx
		     "mov rdi, 0\n"
		     for_copy_args":\n"
		     "cmp rdi, arg_count\n"
		     "je "end_of_copy_args"\n"
		     "mov rax, 8\n"
		     "mul rdi\n"
		     "mov rdx, An(rdi)\n"   ; rdx = i'th argument
		     "mov qword [rcx+rax], rdx\n" ; copy arg i into [rcx+8*i]
		     "inc rdi\n"
		     "jmp "for_copy_args"\n"
		     end_of_copy_args":\n"
		     
		     "mov qword [rbx], rcx\n"
		     
		     "mov r14, env\n"		; r14=previous env
		     "cmp r14, 0\n"
		     "jle "end_of_copy_envs"\n"
		     "mov rdi, 0\n"
		     for_copy_envs":\n"
		     "cmp rdi, " (number->string lexical_env) "\n"
		     "je "end_of_copy_envs"\n"
		     "mov rax, 8\n"
		     "mul rdi\n"
		     "mov rcx, qword [r14+rax]\n" ; rcx = i'th env
		     "mov qword [rbx+rax+8], rcx\n" ; copy env i into [rbx+8*i+8]
		     "inc rdi\n"
		     "jmp "for_copy_envs"\n"
		     
		     end_of_copy_envs":\n"
					;create target
		     "push rbx\n"
		     "push rcx\n"
		     "mov rdi, 16\n"
		     "call malloc\n" ;rax = malloc(8*2)
		     "pop rcx\n"
		     "pop rbx\n"
		     
		     "MAKE_LITERAL_CLOSURE rax, rbx, " code_label "\n"

		     "jmp "skip_code_label"\n"

		     code_label":\n"
		     "push rbp\n"
		     "mov rbp, rsp\n"
		     "mov rbx, " (sobNull) "\n"
		     "mov r10, arg_count\n"
		     
		     for_fix_stack":\n"
		     "cmp r10, "(number->string (length args)) "\n"
		     "je " end_of_fix_stack "\n"
		     
		     "mov rdi, 8\n"
		     "call malloc\n"			
		     "mov rdx, rbp\n"				
		     "add rdx, 4*8\n"				;rdx point to n+m in stack (offset)
		     "mov r11, r10\n"				;r10 is helper for point of arg[i]
		     "dec r11\n"
		     "shl r11, 3\n"				;now offset+r10 = address of curr arg				
		     "add rdx, r11\n"				;rdx = address of arg[i]
		     "mov rdx, qword [rdx]\n"		
		     
		     "MAKE_MALLOC_LITERAL_PAIR rax, rdx, rbx\n"	;rax = target, rbx = cdr, rcx = car
		     "mov rbx, rax\n"				;rbx ponints to the new pair as cdr for the new allocate in next iteration
		     "dec r10\n"					
		     "jmp " for_fix_stack "\n"
		     
		     end_of_fix_stack":\n"
		     "cmp rbx, " (sobNull) "\n"
					;"je "dont_push_arg_label"\n"
		     "mov qword [rbp+4*8+"(number->string (length args))"*8], rbx\n"	;add the list in stack after const params (not optinals)
					;dont_push_arg_label":\n"
					;"mov qword [rbp+5*8+"(number->string (length args))"*8], const_2\n"
					;"mov qword [rbp + 3*8], "(number->string (+ 1 (length args)))"\n" ;update arg_count
					;"add rsp, r9\n"

					(code-gen body)
					
					"leave\n"
					"ret\n"
					skip_code_label":\n")))
        (set! lexical_env (- lexical_env 1)) 
	str-gen)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Pre-Text ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define param-get-def (string-append
		       newLine
		       ";;; Parameter Getters" newLine
		       newLine
		       "%define param(offset) qword [rbp + offset]" newLine
		       newLine
		       "struc scmframe" newLine
		       ".old_rbp: resq 1" newLine
		       ".ret_addr: resq 1" newLine
		       ".env: resq 1" newLine
		       ".arg_count: resq 1" newLine
		       ".A0: resq 1" newLine
		       ".A1: resq 1" newLine
		       ".A2: resq 1" newLine
		       ".A3: resq 1" newLine
		       ".A4: resq 1" newLine
		       ".A5: resq 1" newLine
		       "endstruc" newLine
		       newLine
		       "%define old_rbp param(scmframe.old_rbp)" newLine
		       "%define ret_addr param(scmframe.ret_addr)" newLine
		       "%define env param(scmframe.env)" newLine
		       "%define arg_count param(scmframe.arg_count)" newLine
		       "%define A0 param(scmframe.A0)" newLine
		       "%define A1 param(scmframe.A1)" newLine
		       "%define A2 param(scmframe.A2)" newLine
		       "%define A3 param(scmframe.A3)" newLine
		       "%define A4 param(scmframe.A4)" newLine
		       "%define A5 param(scmframe.A5)" newLine
		       "%define An(n) qword [rbp + 8*(n+4)]" newLine
		       newLine
		       ))

(define generate-pre-text
  (lambda (ct ft)
    ;;(display (format "Generating Prolog\n"))
    (string-append "%include \"scheme.s\"" newLine
		   param-get-def
		   newLine
		   "section .bss" newLine
		   "global main" newLine
		   "extern malloc" newLine
		   newLine
		   "section .data" newLine
		   "start_of_data:" newLine
		   error-applic-label ":" newLine
		   tab "MAKE_LITERAL_STRING \"Error:\", CHAR_SPACE, \"Applic\", CHAR_SPACE, \"on\", CHAR_SPACE, \"non\", CHAR_SPACE, \"procedure\"" newLine
		   newLine
		   (cg-f-table ft)
		   newLine
		   (cg-c-table ct)

		   "SymbolTable: \n"
		   "dq 1\n" ; 1 is arbituary


		   newLine
		   ;;(cg-built-in-closures (filter (lambda (row)
		   ;;(built-in? (second row)))
		   ;;f-table))
		   "section .text" newLine
		   newLine
		   "main:" newLine
		   ;;(master-symbol-builder ct)
		   ;;"mov [SymbolTable], rax \n"
		   )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Post-Text ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l-exit "L_exit")

(define cg-error
  (lambda (msgPtr)
    (string-append
     tab "MOV rax, " msgPtr newLine
     cg-print-rax)))

(define cg-error-applic
  (lambda ()
    (let ((chars (map (lambda (ch)
			(char->integer ch))
		      (string->list "Apply argument not a clusure!\n"))))
      (string-append
       newLine
       applic-T-closure-error-label ":" newLine
       (cg-error error-applic-label)
       newLine
       tab "JMP " l-exit newLine
       newLine
       "section .text" newLine))))

(define post-text
  ;;(begin
  ;;(display (format "Generating Epilogue\n"))
  (string-append
   l-exit ":" newLine
   tab "MOV rax, 60" newLine
   tab "MOV rdi, 0" newLine
   tab "syscall" newLine))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Built-in ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define built-in-map
  ;; <func-name, func-label>
  '((null? null-pred-label)
    (boolean? bool-pred-label)
    (char? char-pred-label)
    (integer? integer-pred-label)
    (number? number-pred-label)
    (rational? rational-pred-label)
    (pair? pair-pred-label)
    (string? string-pred-label)
    (symbol? symbol-pred-label)
    (vector? vector-pred-label)
    (procedure? closure-pred-label)

    (apply "L_apply")

    (b< "L_less_b")
    (b= "L_equal_b")
    (b+ "L_add_b")
    (b/ "L_div_b")
    (b* "L_mult_b")
    (b- "L_sub_b")

    (car "L_car")
    (cdr "L_cdr")
    (char->integer "L_char_integer")
    (cons "L_cons")
    (denominator "L_denominator")
    (eq? "L_eq")
    (integer->char "L_integer_char")
    (make-string "L_make_string")
    (make-vector "L_make_vector")
    (numerator "L_numerator")
    (remainder "L_remainder")
    (string-length "L_string_length")
    (string-ref "L_string_ref")
    (string-set! "L_string_set")
    (string->symbol "L_string_symbol")
    (symbol->string "L_symbol_string")
    (vector "L_vector")
    (vector-length "L_vector_length")
    (vector-ref "L_vector_ref")
    (vector-set! "L_vector_set")))

(define built-in-funcs
  (map first built-in-map))

(define built-in?
  (lambda (fun)
    (member func built-in-funcs)))

(define cg-built-in
  (lambda ()
    (string-append
     cg-null?
     newLine
     cg-bool?
     newLine
     cg-char?
     newLine
     cg-integer?
     newLine
     cg-number?
     newLine
     cg-rational?
     newLine
     cg-pair?
     newLine
     cg-string?
     newLine
     cg-symbol?
     newLine
     cg-vector?
     newLine
     cg-closure?
     newLine)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Type Checks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cg-type-check
  (lambda (type-label . sub-types)
    (string-append
     type-label ":" newLine
     tab "enter" newLine
     tab "PUSHA" newLine
     tab "MOV rax, [rbp + 8 + 1*8]" newLine

     type-label "_predicate:" newLine
     (fold-left string-append
		""
		(map (lambda (sub)
		       (string-append
			tab "CMP rax, " sub newLine
			tab "JE " type-label "_match" newLine))
		     sub-types))

     tab "MOV rax, [L_const2]" newLine
     tab "JMP " type-label "_end" newLine
     newLine
     type-label "_match:" newLine
     tab "MOV rax, [L_const4]" newLine
     type-label "_end:" newLine
     
     tab "POPA" newLine
     tab "leave" newLine
     tab "ret" newLine)))

(define null-pred-label "L_null_check")

(define cg-null?
  (cg-type-check null-pred-label "T_NIL"))

(define bool-pred-label "L_bool_check")

(define cg-bool?
  (cg-type-check bool-pred-label "T_BOOL"))

(define char-pred-label "L_char_check")

(define cg-char?
  (cg-type-check char-pred-label "T_CHAR"))

(define integer-pred-label "L_integer_check")

(define cg-integer?
  (cg-type-check integer-pred-label "T_INTEGER"))

(define number-pred-label "L_number_check")

(define cg-number?
  (cg-type-check number-pred-label "T_INTEGER" "T_FRACTION"))

(define rational-pred-label "L_rational_check")

(define cg-rational?
  (cg-type-check rational-pred-label "T_INTEGER" "T_FRACTION"))

(define pair-pred-label "L_pair_check")

(define cg-pair?
  (cg-type-check pair-pred-label "T_PAIR"))

(define string-pred-label "L_string_check")

(define cg-string?
  (cg-type-check string-pred-label "T_STRING"))

(define symbol-pred-label "L_symbol_check")

(define cg-symbol?
  (cg-type-check symbol-pred-label "T_SYMBOL"))

(define vector-pred-label "L_vector_check")

(define cg-vector?
  (cg-type-check vector-pred-label "T_VECTOR"))

(define closure-pred-label "L_closure_check")

(define cg-closure?
  (cg-type-check closure-pred-label "T_CLOSURE"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Pair Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Binary Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; String Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Vector Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Closure Generation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cg-built-in-closures
  (lambda (rows-to-create)
    (fold-left string-append
	       ""
	       (map (lambda (row)
		      (let ((var (second row))
			    (index (first row))
			    (label (second (assoc (second row) built-in-map))))
			(create-built-in-closure var index label)))
		    rows-to-create))))

(define create-built-in-closure
  (lambda (var index func-label)
    (string-append
     tab "MOV rdi, 16" newLine 
     tab "call malloc" newLine ;; Env = 8 * num_of_vars
     tab "MOV rbx, rax" newLine
     tab "PUSH rbx" newLine
     newLine
     tab "MOV rdi, 8" newLine
     tab "call malloc" newLine ;; Body-code label
     tab "POP rbx" newLine ;;rax=malloc(8), rbx=malloc(16)
     tab "MOV rdx, " func-label newLine
     tab "MAKE_LITERAL_CLOSURE rax, rbx, rdx" newLine
     tab "MOV rbx, " fvar-label (number->string index) newLine
     tab "MOV [rbx], rax" newLine
     newLine)))
