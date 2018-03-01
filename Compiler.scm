(load "project/sexpr-parser.scm")
(load "project/tag-parser.scm")
(load "project/semantic-analyzer.scm")

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
	   (built-in (file->list "project/Built-in.scm"))
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
		  dest)))))
	;;(display (format "Compiled Scheme file with ~a parsed expressions!\n" size))))))

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
    (let ((extracted (append built-in-funcs (extract-fvars exp))))
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

(define master-symbol-builder
  (lambda (table) ;c-table
    (let* ((firstPart (symb-table-make (get-symbols table)))
	   (secondPart (make-linked-symb-list)))			
      (string-append
       firstPart
       secondPart
       "mov [SymbolTable], eax \n"
       ))))






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
    tab "MOV RAX, [" fvar-label (number->string (f-table-get var)) "]" newLine)))

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
     ;;tab "MOV RAX, qword [RAX]" newLine
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
	   (string-length (number->string args-length)))
      ;;(display (format "Code-gen Applic:\nargs-length = ~a\nString-length = ~a\n" args-length  string-length))
      (set! applic-num (+ 1 applic-num))
      (string-append
       ";; cg-applic" newLine
       (cg-push-args args)
       newLine
       ;;tab "XOR rbx, rbx" newLine
       ;;tab "MOV rbx, " string-length newLine
       tab "PUSH " string-length newLine ;; num of args
       ;;tab "PUSH " (number->string args-length) newLine
       newLine
       (code-gen proc) ;;Rax = Closure value
       newLine
       tab "MOV rax, [rax]" newLine
       tab "MOV rbx, rax" newLine
       tab "TYPE rbx" newLine
       newLine
       tab "CMP rbx, T_CLOSURE" newLine
       tab "JNE .end_applic" (number->string applic-num) newLine
       newLine
       tab "MOV rbx, rax" newLine
       tab "CLOSURE_ENV rbx" newLine
       tab "PUSH rbx" newLine ;; Env
       tab "CLOSURE_CODE rax" newLine
       tab "call rax" newLine
       ".end_applic" (number->string applic-num) ":" newLine
       tab "ADD rsp, "  (number->string (* 8 (+ 2 args-length))) newLine
       ))))

;;; Define

(define cg-define
  (lambda (var value)
    ;;(display (format "f-table::\n~a\n" f-table))
    ;;(display (format "cg-define:\nVar = ~a\nValue = ~a\n" var value))
    (let ((address (number->string (f-table-get var))))
      ;;(display (format "Address for (~a ~a) is ~a\n" var value address))
      (string-append
       ";; cg-define" newLine
       (code-gen value)
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
		     ";; cg-lambda-simple" newLine
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
    	(opt (caddr pe))
	   (body (cadddr pe))
	   (code_end (make-label "code_end")) (loop_copy_args_ (make-label "loop_copy_args_")) (loop_copy_args_end (make-label "loop_copy_args_end"))
	   (loop_copy_envs (make-label "loop_copy_envs")) (loop_copy_envs_end (make-label "loop_copy_envs_end")) (code (make-label "code")) (new_env (make-label "new_env"))
	   (loop_fix_stack (make-label "loop_fix_stack")) (loop_fix_stack_end (make-label "loop_fix_stack_end"))
	   (str-gen (string-append
		     ";; cg-lambda-opt" newLine
		     "mov rbx, 0\n"
		     "mov rax, " (number->string lexical_env) "\n"
		     "cmp rbx, 0\n"
		     "cmp rax, 0\n"
		     "je "loop_copy_envs_end"\n"
		     tab "push rax \n"
		     newLine
		     "mov rdi, "(number->string (* 8 (+ 1 lexical_env)))"\n"
		     "call malloc\n"
		     "mov rbx, rax\n"	
		     tab "pop rax \n"
		     "mov rax, arg_count\n"
		     "mov rdi, 8\n"
		     "mul rdi\n"
		     "push rbx\n"	
		     "mov rdi, rax\n"
		     "call malloc\n"
		     "pop rbx\n"
		     "mov rcx, rax\n"						
		     "mov rdi, 0\n"
		     newLine
		     loop_copy_args_":\n"
		     "inc rax\n"
		     "cmp rax, arg_count \n"
		     "sub rax, 1 \n"
		     "cmp rdi, arg_count\n"
		     "je "loop_copy_args_end"\n"
		     "mov rax, 8\n"
		     "mul rdi\n"
		     "mov rdx, An(rdi)\n"   
		     "mov qword [rcx+rax], rdx\n" 
		     "inc rdi\n"
		     "jmp "loop_copy_args_"\n"
		     loop_copy_args_end":\n"		     
		     "mov qword [rbx], rcx\n"		     
		     "mov r14, env\n"		
		     "cmp r14, 0\n"
		     "jle "loop_copy_envs_end"\n"
		     "mov rdi, 0\n"
		     newLine
		     loop_copy_envs":\n"
		     "cmp rdi, " (number->string lexical_env) "\n"
		     "je "loop_copy_envs_end"\n"
		     "mov rax, 8\n"
		     "mul rdi\n"
		     "cmp rdi, 999999 \n"
		     "je " loop_copy_envs "\n" 
		     "mov rcx, qword [r14+rax]\n" 
		     "mov qword [rbx+rax+8], rcx\n" 
		     "inc rdi\n"
		     "jmp "loop_copy_envs"\n"		
		     newLine     
		     loop_copy_envs_end":\n"					
		     "push rbx\n"
		     "push rcx\n"
		     "mov rdi, 16\n"
		     "call malloc\n" 
		     "pop rcx\n"
		     "pop rbx\n"		     
		     "MAKE_LITERAL_CLOSURE rax, rbx, " code "\n"
		     "jmp "code_end"\n"
		     newLine
		     code":\n"
		     "push rbp\n"
		     "mov rbp, rsp\n"
		     "mov rbx, " (sobNull) "\n"
		     "mov r10, arg_count\n"
		     newLine
		     loop_fix_stack":\n"
		     "cmp r10, "(number->string (length args)) "\n"
		     "je " loop_fix_stack_end "\n"
		     "mov rdi, 8\n"
		     "call malloc\n"			
		     "mov rdx, rbp\n"				
		     "add rdx, 4*8\n"				
		     "mov r11, r10\n"				
		     "dec r11\n"
		     "shl r11, 3\n"							
		     "add rdx, r11\n"				
		     "mov rdx, qword [rdx]\n"		
		     "inc rax\n"
		     "sub rax, 1\n"
		     "MAKE_MALLOC_LITERAL_PAIR rax, rdx, rbx\n"	
		     "mov rbx, rax\n"				
		     "dec r10\n"					
		     "jmp " loop_fix_stack "\n"
		     newLine
		     loop_fix_stack_end":\n"
		     "cmp rbx, " (sobNull) "\n"
		     "mov qword [rbp+4*8+"(number->string (length args))"*8], rbx\n"	
			(code-gen body)
			"leave\n"
			"ret\n"
			code_end":\n")))
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
    (string-append "%include \"project/scheme.s\"" newLine
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
		   tab "PUSH 0" newLine
		   tab "PUSH 0" newLine
		   tab "PUSH L_exit" newLine
		   tab "push rbp" newLine
		   tab "mov rbp, rsp" newLine
		   (master-symbol-builder ct)
		   "mov [SymbolTable], rax \n"
		   newLine
		   (cg-built-in)
		   ;;(cg-built-in-closures (filter (lambda (row)
						   ;;(built-in? (second row)))
		   ;;f-table))
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
   tab "leave" newLine
   tab "MOV rax, 60" newLine
   tab "MOV rdi, 0" newLine
   tab "syscall" newLine))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Built-in ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define cg-cons
    (lambda()
        (string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, cons_body\n"
            "mov [L_global21], rax\n"
            "jmp cons_exit\n"
            "cons_body:\n"
            "push rbp\n"
			"mov rbp, rsp\n"
			"mov rbx, arg_count\n"
	        "cmp rbx, 2\n" 
	 		"jne cons_finish\n"
            "mov rdi, 8\n"
            "call malloc\n"
            "mov rcx, An(0)\n"
            "mov rdx, An(1)\n"
            "MAKE_MALLOC_LITERAL_PAIR rax, rcx, rdx\n"
            "cons_finish:\n"
        	"leave\n"
            "ret\n"
            "cons_exit:\n")))
(define cg-car
    (lambda()
        (string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, car_body\n"
            "mov [L_global2], rax\n"
            "jmp car_exit\n"
            "car_body:\n"
            "push rbp\n"
			"mov rbp, rsp\n"       
            "mov rax, An(0)\n"
            "mov rax, [rax]\n"
            "DATA_UPPER rax\n"
			"add rax, start_of_data\n"
            "leave\n"
            "ret\n"
            "car_exit:\n")))

(define cg-cdr
    (lambda()
        (string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, cdr_body\n"
            "mov [L_global19], rax\n"
            "jmp cdr_exit\n"
            "cdr_body:\n"
            "push rbp\n"
			"mov rbp, rsp\n"       
            "mov rax, An(0)\n"
            "mov rax, [rax]\n"
            "DATA_LOWER rax\n"
			"add rax, start_of_data\n"
            "leave\n"
            "ret\n"
            "cdr_exit:\n")))

(define cg-string->symbol
	(lambda ()
		(string-append
			"mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, string_to_symbol_body\n"
            "mov qword [L_global32], rax\n"
            "jmp string_to_symbol_exit\n"
            
            "string_to_symbol_body:\n"
			"push rbp\n"
			"mov rbp, rsp\n"
			"mov r11, An(0)\n" ;r11= pointer to arg
			;"mov r11, [r11]\n"
		    "mov r10, [SymbolTable]\n" ;content of fymbol_table, either a pair or const_2
		    ;"mov r10, [r10]\n"				
	        "cmp r10, L_const1\n" 
	        "je string_to_symbol_create_symbol\n"
    		
	        "string_to_symbol_loop:\n"
			"mov r12, r10\n"
			"mov r12, [r12]\n"
			"DATA_UPPER r12\n"
			"add r12 , start_of_data\n"
			"mov r12, [r12]\n"
			"DATA r12\n"
			"add r12 , start_of_data\n"
      		;"cmp r12, r11\n"  
      		"STRING_COMPARE r12, r11\n"
      		"cmp rax, L_const4\n" 
	        "je string_to_symbol_found\n"
		    ;"CDR r10\n"
		    "mov r10, [r10]\n"
		    "DATA_LOWER r10\n"
			"add r10, start_of_data\n"
			"cmp r10, L_const1\n"
      		"je string_to_symbol_create_symbol\n"
	        "jmp string_to_symbol_loop\n"
	            
	        "string_to_symbol_found:\n"
	        "mov r10, [r10]\n"
	        "DATA_UPPER r10\n"
			"add r10, start_of_data\n"
		   	"mov rax, r10\n"
	        "jmp string_to_symbol_finish\n"
	      
	        "string_to_symbol_create_symbol:\n"
	        "push r11\n"
	        "mov rdi,8\n"
	        "call malloc\n"
	        "pop r11\n"
	        "MAKE_MALLOC_LITERAL_SYMBOL rax , r11\n"
			"mov r11, rax\n"
      		"mov r13, r11\n"                    ;backup
      		"mov r14, [SymbolTable]\n"       
      
      		"push r11\n"
      		"push r14\n"
       		"mov rdi, 8\n"
       		"call malloc\n"
       		"pop r14\n"
       		"pop r11\n"
       		"MAKE_MALLOC_LITERAL_PAIR rax, r11 ,r14\n"
      		"mov [SymbolTable],rax\n"
      		"mov rax, r13\n"

	        "string_to_symbol_finish:\n"
	        "leave\n"
	        "ret\n"
	        "string_to_symbol_exit:\n"      
	)))



(define cg-char->integer
	(lambda()
	  (string-append
	   "mov rdi, 16\n"
	   "call malloc\n"
	   "mov rbx, qword 0\n"
	   "MAKE_LITERAL_CLOSURE rax, rbx, char_to_integer_body\n"
	   "mov [L_global20], rax\n"
	   "jmp char_to_integer_exit\n"
	   
	   "char_to_integer_body:\n"
	   "push rbp\n"
	   "mov rbp, rsp\n"         
	   "mov rbx, arg_count\n"
	   "cmp rbx, 1\n" 
	   "jne char_to_integer_finish\n"
	   
	   "mov rax, An(0)\n"
	   "mov rax, [rax]\n"
	   "mov rbx, rax\n"
	   "TYPE rax\n"
	   "cmp rax, T_CHAR\n"
	   "jne char_to_integer_finish\n"
	   
	   "sub rbx, T_CHAR\n"
	   "or rbx, T_INTEGER\n"
	   
	   "mov rdi,8\n"
	   "call malloc\n"
	   "mov qword [rax], rbx\n"
	   
	   "char_to_integer_finish:\n"
	   "leave\n"
	   "ret\n"
	   "char_to_integer_exit:\n" )
	))





(define cg-integer->char
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, integer_to_char_body\n"
            "mov [L_global24], rax\n"
            "jmp integer_to_char_exit\n"
            
            "integer_to_char_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 1\n" 
	 		"jne integer_to_char_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "mov rbx, rax\n"
	        "TYPE rax\n"
	        "cmp rax, T_INTEGER\n"
	        "jne integer_to_char_finish\n"

	        "sub rbx, T_INTEGER\n"
	        "or rbx, T_CHAR\n"

	        "mov rdi,8\n"
	        "call malloc\n"
	        "mov qword [rax], rbx\n"

	        "integer_to_char_finish:\n"
	        "leave\n"
	        "ret\n"
	        "integer_to_char_exit:\n" )
	))



(define cg-numerator
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, numerator_body\n"
            "mov [L_global27], rax\n"
            "jmp numerator_exit\n"
            
            "numerator_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 1\n" 
	 		"jne numerator_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "TYPE rax\n"
	        "cmp rax, T_INTEGER\n"
	        "je get_integer_numerator\n"
	        "cmp rax, T_FRACTION\n"
	        "jne numerator_finish\n"
	        "mov rax, An(0)\n"
	        "mov rax, [rax]\n"

	        "DATA_UPPER rax\n"
			"add rax, start_of_data\n"
	        "jmp numerator_finish\n"

	        "get_integer_numerator:\n"
	        "mov rax, An(0)\n"

	        "numerator_finish:\n"
	        "leave\n"
	        "ret\n"
	        "numerator_exit:\n" )
	))


(define cg-denominator
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, denominator_body\n"
            "mov [L_global22], rax\n"
            "jmp denominator_exit\n"
            
            "denominator_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 1\n" 
	 		"jne denominator_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "TYPE rax\n"
	        "cmp rax, T_INTEGER\n"
	        "je get_integer_denominator\n"
	        "cmp rax, T_FRACTION\n"
	        "jne denominator_finish\n"
	        "mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "DATA_LOWER rax\n"
			"add rax, start_of_data\n"
	        "jmp denominator_finish\n"

	        "get_integer_denominator:\n"
	        "mov rbx, MAKE_LITERAL(T_INTEGER,1)\n"
	        "mov rdi,8\n"
	        "call malloc\n"    
	        "mov qword [rax], rbx\n"

	        "denominator_finish:\n"
	        "leave\n"
	        "ret\n"
	        "denominator_exit:\n" )
	))

(define cg-eq
 	(lambda()       
		(string-append
		 "mov rdi, 16\n"
		 "call malloc\n"
		 "mov rbx, qword 0\n"
		 "MAKE_LITERAL_CLOSURE rax, rbx, eq_body\n"
		 "mov [L_global13], rax\n"
		 "jmp eq_exit\n"
		 
		 "eq_body:\n"
		 "push rbp\n"
		 "mov rbp, rsp\n"         
		 "mov rbx, arg_count\n"
		 "cmp rbx, 2\n" 
		 "jne eq_finish\n"
		 
		 "mov rax, An(0)\n"
		 "mov rax, [rax]\n"
		 "mov rbx, An(1)\n"
		 "mov rbx, [rbx]\n"
		 "cmp rax, rbx\n"
		 "je eq_true\n"
		 "mov rax, L_const2\n"
		 "jmp eq_finish\n"
		 
		 "eq_true:\n"
		 "mov rax, L_const4\n"
		 
		 "eq_finish:\n"
		 "leave\n"
		 "ret\n"
		 "eq_exit:\n" )))



(define cg-make-string
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, make_string_body\n"
            "mov [L_global25], rax\n"
            "jmp make_string_exit\n"
            
            "make_string_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"
			"mov rdx, qword 0\n" ;initialize char with 0         
            "mov r9, arg_count\n"
	        "cmp r9, 2\n"
	 		"jg make_string_finish\n"

	 		"mov rax, An(0)\n" ;length of string	        
	        "mov rax, [rax]\n"
	        "mov rbx, rax\n"
	        "DATA rbx\n" 

	        "TYPE rax\n"
	        "cmp rax, T_INTEGER\n"
	        "jne make_string_finish\n"

	        "cmp r9, 1\n"
	        "je start_creating_string\n"

	        "mov rcx, An(1)\n" ;char
	        "mov rcx, [rcx]\n"
	        "mov rdx, rcx\n"
	        "DATA rdx\n"

	        "TYPE rcx\n"
	        "cmp rcx, T_CHAR\n"
	        "jne make_string_finish\n"

	        "start_creating_string:\n"

	        "push rbx\n"
	        "push rdx\n"
	        "mov rdi, rbx\n"
	        "call malloc\n"
	        "pop rdx\n"
	        "pop rbx\n"

 
	        ;rax= pointer to address of rbx bytes, rbx=length of string, rdx=char
	        "mov r10, 0\n" ;counter

	        "for_create_string:\n"
	        "cmp r10, rbx\n"
	        "je end_of_create_string\n"
	        "mov byte [rax+r10], dl\n"
	        "inc r10\n"
	        "jmp for_create_string\n"
	        "end_of_create_string:\n"

	        "mov rcx, rax\n"

	        "MAKE_LITERAL_STRING_WITH_REGS rcx, rbx\n" ;at the end of macro, rax is a literal string
	        "mov rcx, rax\n"
	        "push rcx\n"
	        "mov rdi, 8\n"
	        "call malloc\n"
	        "pop rcx\n"

	        "mov [rax], rcx\n"

	        "make_string_finish:\n"
	        "leave\n"
	        "ret\n"
	        "make_string_exit:\n" )))




(define cg-make-vector
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, make_vector_body\n"
            "mov [L_global26], rax\n"
            "jmp make_vector_exit\n"
            
            "make_vector_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"  
			;initialize item with 0
			"mov rdi, 8\n"
			"call malloc\n"
			"mov rdx, 0\n"
			"shl rdx, TYPE_BITS\n"
	        "add rdx, T_INTEGER\n"       
            "mov rbx, arg_count\n"
            "mov [rax], rdx\n" 
            "mov rdx, rax\n" ;now rdx conatins integer 0
	        "cmp rbx, 2\n" 
	 		"jg make_vector_finish\n"

	 		"mov rax, An(0)\n" ;length of vector	        
	        "mov rax, [rax]\n"
	        "mov rbx, rax\n"
	        "DATA rbx\n" 

	        "TYPE rax\n"
	        "cmp rax, T_INTEGER\n"
	        "jne make_vector_finish\n"

	        "mov r9, arg_count\n"
	        "cmp r9, 1\n"
	        "je start_creating_vector\n"
	        "mov rdx, An(1)\n" ; address of item

	        "start_creating_vector:\n"

	        "push rbx\n"
	        "push rdx\n"
	        "shl rbx, 3\n"
	        "mov rdi, rbx\n"
	        "call malloc\n"
	        "pop rdx\n"
	        "pop rbx\n"
 
	        ;rax= pointer to address of rbx*8 bytes, rbx=length of vector, rdx=address of item
	        "mov r10, 0\n" ;counter

	        "for_create_vector:\n"
	        "cmp r10, rbx\n"
	        "je end_of_create_vector\n"
	        "mov qword [rax+r10*8], rdx\n"
	        "inc r10\n"
	        "jmp for_create_vector\n"
	        "end_of_create_vector:\n"

	        "mov rcx, rax\n"
	        "shl rbx, 3\n"
	        "MAKE_LITERAL_VECTOR_WITH_REGS rcx, rbx\n" ;at the end of macro, rax is a literal vector
	        "mov rcx, rax\n"
	        "push rcx\n"
	        "mov rdi, 8\n"
	        "call malloc\n"
	        "pop rcx\n"
	        "mov [rax], rcx\n"

	        "make_vector_finish:\n"
	        "leave\n"
	        "ret\n"
	        "make_vector_exit:\n" )))
	

	(define cg-remainder
 	(lambda()       
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, remainder_body\n"
            "mov [L_global28], rax\n"
            "jmp remainder_exit\n"
            
            "remainder_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 2\n" 
	 		"jne remainder_finish\n"

	 		"mov rax, An(0)\n"
	 		"mov rax, [rax]\n"
	 		"mov rcx, rax\n"
	 		"mov rbx, An(1)\n"
	 		"mov rbx, [rbx]\n"
	 		"mov r10, rbx\n"
	 		"TYPE rcx\n"
	        "cmp rcx, T_INTEGER\n"
	        "jne remainder_finish\n"
	        "TYPE rbx\n"
	        "cmp rbx, T_INTEGER\n"
	        "jne remainder_finish\n"
	        "DATA rax\n"
	        "mov r9, rax\n"
	        "DATA r10\n"
	        "mov rdx, qword 0\n"
	      
	        "cmp r9, 0\n"
	        "jge is_not_negative1\n"
	        "neg rax\n"
	        "is_not_negative1:\n"
	        "mov rdx, qword 0\n"
	        "idiv r10\n"
	        "cmp r9, 0\n"
	        "jge is_not_negative2\n"
	        "neg rdx\n"
	        "is_not_negative2:\n"

	        "shl rdx, TYPE_BITS\n"
	        "add rdx, T_INTEGER\n"
	        "push rdx\n"
	        "mov rdi, 8\n"
	        "call malloc\n"
	        "pop rdx\n"
	        "mov [rax], rdx\n"
	        
	        "remainder_finish:\n"
	        "leave\n"
	        "ret\n"
	        "remainder_exit:\n" )))



(define cg-string-length
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, string_length_body\n"
            "mov [L_global29], rax\n"
            "jmp string_length_exit\n"
            
            "string_length_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 1\n" 
	 		"jne string_length_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "mov rbx,rax\n"
	        "TYPE rax\n"
	        "cmp rax, T_STRING\n"
	        "jne string_length_finish\n"
	        "STRING_LENGTH rbx\n"
	        "shl rbx, TYPE_BITS\n"
	        "add rbx, T_INTEGER\n"
	        "mov rdi,8\n"
	        "call malloc\n"
	        "mov [rax], rbx\n"

	        "string_length_finish:\n"
	        "leave\n"
	        "ret\n"
	        "string_length_exit:\n" )
	))



(define cg-string-ref
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, string_ref_body\n"
            "mov [L_global30], rax\n"
            "jmp string_ref_exit\n"
            
            "string_ref_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 2\n" 
	 		"jne string_ref_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "mov rbx, rax\n"
	        
	        "mov rcx, An(1)\n"
	        "mov rcx, [rcx]\n"
	        "mov rdx, rcx\n"
	        "DATA rdx\n"

	        "TYPE rax\n"
	        "cmp rax, T_STRING\n"
	        "jne string_ref_finish\n"

	        "TYPE rcx\n"
	        "cmp rcx, T_INTEGER\n"
	        "jne string_ref_finish\n"

	        "STRING_REF cl, rbx, rdx\n"
	        "shl rcx, TYPE_BITS\n"
	        "add rcx, T_CHAR\n"
	        "push rcx\n"
	        "mov rdi,8\n"
	        "call malloc\n"
	        "pop rcx\n"
	        "mov [rax], rcx\n"

	        "string_ref_finish:\n"
	        "leave\n"
	        "ret\n"
	        "string_ref_exit:\n" )))



(define cg-string-set
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, string_set_body\n"
            "mov [L_global31], rax\n"
            "jmp string_set_exit\n"
            
            "string_set_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 3\n" 
	 		"jne string_set_finish\n"

	 		"mov rax, An(0)\n" ;string	        
	        
	        "mov rax, [rax]\n"
	        "mov rbx, rax\n" 

	        "mov r11, An(1)\n" ;index
	        "mov r11, [r11]\n"
	        "mov rdx, r11\n"
	        "DATA rdx\n"

	       	"mov r10, An(2)\n" ;char
	        "mov r10, [r10]\n"
	        "mov rcx, r10\n"
	        "DATA rcx\n"

	        "TYPE rax\n"
	        "cmp rax, T_STRING\n"
	        "jne string_set_finish\n"

	        "TYPE r11\n"
	        "cmp r11, T_INTEGER\n"
	        "jne string_set_finish\n"

	       	"TYPE r10\n"
	        "cmp r10, T_CHAR\n"
	        "jne string_set_finish\n"

	        ;rbx=string, rdx=index, rcx=char

	        "mov r12, rbx\n"
	        "STRING_ELEMENTS rbx\n"
			"add rbx, rdx\n"
			"mov byte [rbx], cl\n"

	        "mov rax, L_const0\n"

	        "string_set_finish:\n"
	        "leave\n"
	        "ret\n"
	        "string_set_exit:\n" )))



(define cg-vector-set
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, vector_set_body\n"
            "mov [L_global37], rax\n"
            "jmp vector_set_exit\n"
            
            "vector_set_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 3\n" 
	 		"jne vector_set_finish\n"

	 		"mov rax, An(0)\n" ;vector	        
	        "mov rax, [rax]\n"
	        "mov rbx, rax\n" 

	        "mov r11, An(1)\n" ;index
	        "mov r11, [r11]\n"
	        "mov rdx, r11\n"
	        "DATA rdx\n"

	       	"mov rcx, An(2)\n" ;address of item

	        "TYPE rax\n"
	        "cmp rax, T_VECTOR\n"
	        "jne vector_set_finish\n"

	        "TYPE r11\n"
	        "cmp r11, T_INTEGER\n"
	        "jne vector_set_finish\n"

	        ;rbx=vector, rdx=index, rcx=item

	        "mov r12, rbx\n"
	        "VECTOR_ELEMENTS r12\n"
	        "mov [r12 + rdx*8], rcx\n"

	        "mov rax, L_const0\n"

	        "vector_set_finish:\n"
	        "leave\n"
	        "ret\n"
	        "vector_set_exit:\n" )))

(define cg-bin-minus
  (lambda ()
    (string-append
     "mov rdi, 16\n"
     "call malloc\n"
     "mov rbx, qword 0\n"
     "MAKE_LITERAL_CLOSURE rax, rbx, bin_minus_body\n"
     "mov [L_global17], rax\n"
     "jmp bin_minus_exit\n"
     
     "bin_minus_body:\n"
     "push rbp\n"
     "mov rbp, rsp\n"
     "mov rcx ,arg_count\n"
     "mov rax ,An(0)\n"                
     
     "cmp rcx, 2\n"         			; nil + 1 arguments?
     "jne bin_minus_finish\n"
     
     "mov rax ,qword [rax]\n"                     
     "mov rcx, rax\n"
     "TYPE rcx\n"
     "cmp rcx, T_INTEGER\n"
     "je bin_minus_arg1_int_check_arg2\n"
     
     "bin_minus_arg1_frac_check_arg2:\n"
     "mov rbx, An(1)\n"
     "mov rbx, qword [rbx]\n"                    
     "mov rcx, rbx\n"
     "TYPE rcx\n"
     "cmp rcx, T_INTEGER\n"
     "je bin_minus_arg1_frac_arg2_int\n"
     
     "bin_minus_arg1_frac_arg2_frac:\n"
     "mov rcx, rax\n"				
     "CAR rcx\n"
     "DATA rcx\n"					;rcx holds arg1 numerator	
     "mov r11, rax\n"      			;rdx keeps rax value
     "CDR r11\n"
     "DATA r11\n"
     
     "mov r10, rbx\n"					
     "CAR r10\n"
     "DATA r10\n"					;r10 holds arg2 numerator
     "mov r12, rbx\n"				;r12 keeps r10 value
     "CDR r12\n"
     "DATA r12\n"
     "mov r8, r11\n"					;backup first denominator
     
     "MULT r11, r12\n"				;now r11 holds arg1 denominator * arg2 denominator
     "MULT rcx, r12\n"				;now rcx holds arg1 numerator * arg2 denominator
     "MULT r10, r8\n"				;now r10 holds second numerator * arg1 denominator
     "sub rcx, r10\n"
     "mov rax, rcx\n"	
     "mov r12, r11\n"					
     "jmp bin_minus_simplify_and_create_fraction\n"                                             
     
     "bin_minus_arg1_int_check_arg2:\n"
     "mov rbx, An(1)\n"
     "mov rbx, qword [rbx]\n"                    
     "mov rdx, rbx\n"
     "TYPE rdx\n"
     "cmp rdx, T_INTEGER\n"
     "je bin_minus_arg1_int_arg2_int\n"
     
     "bin_minus_arg1_int_arg2_frac:\n"
     "mov r10, rbx\n"
     "CAR r10\n"
     "DATA r10\n";we will put in r10 second_numerator
     "mov r12, rbx\n"
     "CDR r12\n"
     "DATA r12\n";we will put in r12 second_denominator
     "mov r8,rax\n"
     "DATA r8\n"
     "MULT r8,r12\n"
     "mov rax,r8\n";now rax holds first_numerator*second_denominator
     "sub rax,r10\n"
     "jmp bin_minus_simplify_and_create_fraction\n"
     
     "bin_minus_arg1_frac_arg2_int:\n"
     "mov r10, rax\n"
     "CAR r10\n"
     "DATA r10\n"
     "mov r12, rax\n"
     "CDR r12\n"
     "DATA r12\n"
     "mov r8,rbx\n"
     "DATA r8\n"
     "MULT r8,r12\n"
     "mov rax,r8\n";now rax holds first_numerator*second_denominator
     "sub r10, rax\n"
     "mov rax, r10\n"
     
					;rax holds new numerator
					;r12 holds new denominator
     "bin_minus_simplify_and_create_fraction:\n"
     "mov r8,  rax\n"
     "mov r9, r12\n"
		  	"push r9\n"
		  	"push r8\n"
		  	"call simplify_fraction\n"
		  	"add rsp, 16\n"
		  	"jmp bin_minus_create_new_fraction\n"

		  	"bin_minus_arg1_int_arg2_int:\n"
		  	"DATA rbx\n" 
		  	"DATA rax\n"      
		  	"sub rax, rbx\n"
		  	"mov r10, rax\n"
		  	"shl r10, 4\n"
		  	"or r10, T_INTEGER\n"
		  	"push r10\n"
		  	"mov rdi, 8\n"
		  	"call malloc\n"
		  	"pop r10\n"
		  	"mov [rax],r10\n"
		                        
		  	"jmp bin_minus_finish\n"                     
                    
		  	"bin_minus_create_new_fraction:\n"
		  	"cmp r9, 1\n"
		  	"je bin_minus_create_new_int\n"

		  	"shl r8, 4\n"
		  	"or r8, T_INTEGER\n"		;r8 holds numerator in data structure
		  	"shl r9, 4\n"
		  	"or r9, T_INTEGER\n"		;r9 holds denominator in data structure
		    
		    "mov rdi, 8\n"
		    "call malloc\n"
		    "mov rbx, rax\n"
		    "mov [rbx], r8\n"
		    "push rbx\n"

		    "mov rdi, 8\n"
		    "call malloc\n"
		    "pop rbx\n"
		    "mov rcx, rax\n"
		    "mov [rcx], r9\n"
		    "push rbx\n"
		    "push rcx\n"

		    "mov rdi, 8\n"
		    "call malloc\n" 
		    "pop rcx\n"
		    "pop rbx\n"
                                              
		  	"MAKE_LITERAL_FRACTION_WITH_REGS rax, rbx, rcx\n"      
		  	"jmp bin_minus_finish\n"
		          
		   	"bin_minus_create_new_int:\n"

		   	"shl r8, 4\n"
		  	"or r8, T_INTEGER\n"
		  	"push r8\n"

		  	"mov rdi, 8\n"
		    "call malloc\n"
		    "pop r8\n"
		    "mov [rax], r8\n"
          
	        "bin_minus_finish:\n"
	        "leave\n"
	        "ret\n"
	        "bin_minus_exit:\n")))


(define cg-less-than
 	(lambda()       
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, bin_less_than_body\n"
            "mov [L_global12], rax\n"
            "jmp bin_less_than_exit\n"
            
            "bin_less_than_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 2\n" 
	 		"jne bin_less_than_finish\n"

	 		;"push L_const4\n"
	 		"push An(1)\n"
	 		"push An(0)\n"
	 		"push 2\n"
	 		"push qword 0\n"
	 		"call bin_minus_body\n"
	 		"mov rax, [rax]\n"
	 		"mov rbx, rax\n"
	 		"TYPE rbx\n"
	 		"cmp rbx, T_INTEGER\n"
	 		"je check_sign\n"
	 		"CAR rax\n"

	 		"check_sign:\n"
	 		"DATA rax\n"
	 		"cmp rax, 0\n"
	 		"jl bin_less_than_true\n"
	        
	        "mov rax, L_const2\n"
	        "jmp bin_less_than_finish\n"

	        "bin_less_than_true:\n"
	        "mov rax, L_const4\n"

	        "bin_less_than_finish:\n"
	        "leave\n"
	        "ret\n"
	        "bin_less_than_exit:\n" )))



(define cg-eq?
 	(lambda()       
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, eq?_body\n"
            "mov [L_global23], rax\n"
            "jmp eq?_exit\n"
            
            "eq?_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 2\n" 
	 		"jne eq?_finish\n"

	 		"mov rax, An(0)\n"
	 		"mov rax, [rax]\n"
	 		"mov rbx, An(1)\n"
	 		"mov rbx, [rbx]\n"
	        "cmp rax, rbx\n"
	        "je eq?_true\n"
	        "mov rax, L_const2\n"
	        "jmp eq?_finish\n"
	        
	        "eq?_true:\n"
	        "mov rax, L_const4\n"

	        "eq?_finish:\n"
	        "leave\n"
	        "ret\n"
	        "eq?_exit:\n" )))


(define cg-bin-plus
	(lambda ()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, bin_plus_body\n"
            "mov [L_global14], rax\n"
            "jmp bin_plus_exit\n"
            
            "bin_plus_body:\n"
        	"push rbp\n"
      		"mov rbp, rsp\n"
      		"mov rcx ,arg_count\n"
      		"mov rax ,An(0)\n"                

      		"cmp rcx, 2\n"         			; nil + 1 arguments?
      		"jne bin_plus_finish\n"
     
      		"mov rax ,qword [rax]\n"                     
      		"mov rcx, rax\n"
      		"TYPE rcx\n"
      		"cmp rcx, T_INTEGER\n"
      		"je bin_plus_arg1_int_check_arg2\n"
      
      		"bin_plus_arg1_frac_check_arg2:\n"
      		"mov rbx, An(1)\n"
       		"mov rbx, qword [rbx]\n"                    
      		"mov rcx, rbx\n"
      		"TYPE rcx\n"
      		"cmp rcx, T_INTEGER\n"
      		"jne bin_plus_arg1_frac_arg2_frac\n"
      		"mov r10, rax\n"
      		"mov rax, rbx\n"
      		"mov rbx, r10\n"
      		"jmp bin_plus_arg1_int_arg2_frac\n"
      
      		"bin_plus_arg1_frac_arg2_frac:\n"
      		"mov rcx, rax\n"				
      		"CAR rcx\n"
      		"DATA rcx\n"					;rcx holds arg1 numerator	
      		"mov r11, rax\n"      			;rdx keeps rax value
      		"CDR r11\n"
      		"DATA r11\n"
      
     		"mov r10, rbx\n"					
      		"CAR r10\n"
      		"DATA r10\n"					;r10 holds arg2 numerator
      		"mov r12, rbx\n"				;r12 keeps r10 value
      		"CDR r12\n"
      		"DATA r12\n"
      		"mov r8, r11\n"					;backup first denominator

      		"MULT r11, r12\n"				;now r11 holds arg1 denominator * arg2 denominator
      		"MULT rcx, r12\n"				;now rcx holds arg1 numerator * arg2 denominator
      		"MULT r10, r8\n"				;now r10 holds second numerator * arg1 denominator
      		"add rcx, r10\n"
      		"mov rax, rcx\n"	
      		"mov r12, r11\n"					
      		"jmp bin_plus_simplify_and_create_fraction\n"                                             
      		
		  	"bin_plus_arg1_int_check_arg2:\n"
		  	"mov rbx, An(1)\n"
		  	"mov rbx, qword [rbx]\n"                    
		  	"mov rdx, rbx\n"
		  	"TYPE rdx\n"
		  	"cmp rdx, T_INTEGER\n"
		  	"je bin_plus_arg1_int_arg2_int\n"

		  	"bin_plus_arg1_int_arg2_frac:\n"
		  	"mov r10, rbx\n"
		  	"CAR r10\n"
		  	"DATA r10\n";we will put in r10 second_numerator
		  	"mov r12, rbx\n"
		  	"CDR r12\n"
		  	"DATA r12\n";we will put in r12 second_denominator
		  	"mov r8,rax\n"
		  	"DATA r8\n"
		  	"MULT r8,r12\n"
		  	"mov rax,r8\n";now rax holds first_numerator*second_denominator
		  	"add rax,r10\n"

		  	;rax holds new numerator
		  	;r12 holds new denominator
		  	"bin_plus_simplify_and_create_fraction:\n"
		  	"mov r8,  rax\n"
		  	"mov r9, r12\n"
		  	"push r9\n"
		  	"push r8\n"
		  	"call simplify_fraction\n"
		  	"add rsp, 16\n"
		  	"jmp bin_plus_create_new_fraction\n"

		  	"bin_plus_arg1_int_arg2_int:\n"
		  	"DATA rbx\n" 
		  	"DATA rax\n"      
		  	"add rbx,rax\n"
		  	"shl rbx, 4\n"
		  	"or rbx, T_INTEGER\n"
		  	"push rbx\n"
		  	"mov rdi, 8\n"
		  	"call malloc\n"
		  	"pop rbx\n"
		  	"mov [rax],rbx\n"
		                        
		  	"jmp bin_plus_finish\n"                     
                    
		  	"bin_plus_create_new_fraction:\n"
		  	"cmp r9, 1\n"
		  	"je bin_plus_create_new_int\n"

		  	"shl r8, 4\n"
		  	"or r8, T_INTEGER\n"		;r8 holds numerator in data structure
		  	"shl r9, 4\n"
		  	"or r9, T_INTEGER\n"		;r9 holds denominator in data structure
		    
		    "mov rdi, 8\n"
		    "call malloc\n"
		    "mov rbx, rax\n"
		    "mov [rbx], r8\n"
		    "push rbx\n"

		    "mov rdi, 8\n"
		    "call malloc\n"
		    "pop rbx\n"
		    "mov rcx, rax\n"
		    "mov [rcx], r9\n"
		    "push rbx\n"
		    "push rcx\n"

		    "mov rdi, 8\n"
		    "call malloc\n" 
		    "pop rcx\n"
		    "pop rbx\n"
                                               
		  	"MAKE_LITERAL_FRACTION_WITH_REGS rax, rbx, rcx\n"
		  	"jmp bin_plus_finish\n"
		          
		   	"bin_plus_create_new_int:\n"

		   	"shl r8, 4\n"
		  	"or r8, T_INTEGER\n"
		  	"push r8\n"

		  	"mov rdi, 8\n"
		    "call malloc\n"
		    "pop r8\n"
		    "mov [rax], r8\n"
          
	        "bin_plus_finish:\n"
	        "leave\n"
	        "ret\n"
	        "bin_plus_exit:\n")))


(define cg-bin-div
	(lambda ()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, bin_div_body\n"
            "mov [L_global15], rax\n"
            "jmp bin_div_exit\n"
            
            "bin_div_body:\n"
        	"push rbp\n"
      		"mov rbp, rsp\n"
      		"mov rcx ,arg_count\n"
      		"mov rax ,An(0)\n"                

      		"cmp rcx, 2\n"         			; nil + 1 arguments?
      		"jne bin_div_finish\n"
     
      		"mov rax ,qword [rax]\n"                     
      		"mov rcx, rax\n"
      		"TYPE rcx\n"
      		"cmp rcx, T_INTEGER\n"
      		"je bin_div_arg1_int_check_arg2\n"
      
      		"bin_div_arg1_frac_check_arg2:\n"
      		"mov rbx, An(1)\n"
       		"mov rbx, qword [rbx]\n"                    
      		"mov rcx, rbx\n"
      		"TYPE rcx\n"
      		"cmp rcx, T_INTEGER\n"
      		"je bin_div_arg1_frac_arg2_int\n"
      
      		"bin_div_arg1_frac_arg2_frac:\n"
      		"mov rcx, rax\n"				
      		"CAR rcx\n"
      		"DATA rcx\n"					;rcx holds arg1 numerator	
      		"mov r11, rax\n"      			;rdx keeps rax value
      		"CDR r11\n"
      		"DATA r11\n"
      
     		"mov r10, rbx\n"					
      		"CAR r10\n"
      		"DATA r10\n"					;r10 holds arg2 numerator
      		"mov r12, rbx\n"				;r12 keeps r10 value
      		"CDR r12\n"
      		"DATA r12\n"
      		"mov r8, r11\n"					;backup first denominator

      		"MULT rcx, r12\n"				;now rcx holds arg1 numerator * arg2 denominator
      		"MULT r11, r10\n"				;now r11 holds arg1 denominator * arg2 numerator
      		"cmp r11, 0\n"
		  	"jg bin_div_positive_int1\n"
		  	"neg r11\n"
		  	"neg rcx\n"
		  	"bin_div_positive_int1:\n"
      		"mov rax, rcx\n"	
      		"mov r12, r11\n"					
      		"jmp bin_div_simplify_and_create_fraction\n"                                             
      		
		  	"bin_div_arg1_int_check_arg2:\n"
		  	"mov rbx, An(1)\n"
		  	"mov rbx, qword [rbx]\n"                    
		  	"mov rdx, rbx\n"
		  	"TYPE rdx\n"
		  	"cmp rdx, T_INTEGER\n"
		  	"je bin_div_arg1_int_arg2_int\n"

		  	"bin_div_arg1_int_arg2_frac:\n"
		  	"mov r10, rbx\n"
		  	"CAR r10\n"
		  	"DATA r10\n";we will put in r10 second_numerator
		  	"mov r12, rbx\n"
		  	"CDR r12\n"
		  	"DATA r12\n";we will put in r12 second_denominator
		  	"mov r8,rax\n"
		  	"DATA r8\n"
		  	"MULT r8,r12\n"
		  	"cmp r10, 0\n"
		  	"jg bin_div_positive_int2\n"
		  	"neg r10\n"
		  	"neg r8\n"
		  	"bin_div_positive_int2:\n"
		  	"mov r12, r10\n"
		  	"mov rax,r8\n"
		  	"jmp bin_div_simplify_and_create_fraction\n"

		  	"bin_div_arg1_frac_arg2_int:\n"
		  	"mov r10, rax\n"
		  	"CAR r10\n"
		  	"DATA r10\n"
		  	"mov r12, rax\n"
		  	"CDR r12\n"
		  	"DATA r12\n"
		  	"mov r8,rbx\n"
		  	"DATA r8\n"
		  	"MULT r8,r12\n"
		  	"cmp r8, 0\n"
		  	"jg bin_div_positive_int3\n"
		  	"neg r8\n"
		  	"neg r10\n"
		  	"bin_div_positive_int3:\n"
		  	"mov rax,r8\n";now rax holds first_denominator*second_denominator
		  	"mov r11, rax\n"
		  	"mov rax, r10\n"
		  	"mov r12, r11\n"


		  	;rax holds new numerator
		  	;r12 holds new denominator
		  	"bin_div_simplify_and_create_fraction:\n"
		  	"mov r8,  rax\n"
		  	"mov r9, r12\n"
		  	"push r9\n"
		  	"push r8\n"
		  	"call simplify_fraction\n"
		  	"add rsp, 16\n"
		  	"jmp bin_div_create_new_fraction\n"

		  	"bin_div_arg1_int_arg2_int:\n"
		  	"DATA rax\n"
		  	"DATA rbx\n"
		  	"mov r12, rbx\n"
		  	"cmp r12, 0\n"
		  	"jg bin_div_positive_int4\n"
		  	"neg r12\n"
		  	"neg rax\n"
		  	"bin_div_positive_int4:\n" 
		  	"jmp bin_div_simplify_and_create_fraction\n"
		                                
		  	"bin_div_create_new_fraction:\n"
		  	"cmp r9, 1\n"
		  	"je bin_div_create_new_int\n"

		  	"shl r8, 4\n"
		  	"or r8, T_INTEGER\n"		;r8 holds numerator in data structure
		  	"shl r9, 4\n"
		  	"or r9, T_INTEGER\n"		;r9 holds denominator in data structure
		    
		    "mov rdi, 8\n"
		    "call malloc\n"
		    "mov rbx, rax\n"
		    "mov [rbx], r8\n"
		    "push rbx\n"

		    "mov rdi, 8\n"
		    "call malloc\n"
		    "pop rbx\n"
		    "mov rcx, rax\n"
		    "mov [rcx], r9\n"
		    "push rbx\n"
		    "push rcx\n"

		    "mov rdi, 8\n"
		    "call malloc\n" 
		    "pop rcx\n"
		    "pop rbx\n"
                                               
		  	"MAKE_LITERAL_FRACTION_WITH_REGS rax, rbx, rcx\n"
		  	"jmp bin_div_finish\n"
		          
		   	"bin_div_create_new_int:\n"

		   	"shl r8, 4\n"
		  	"or r8, T_INTEGER\n"
		  	"push r8\n"

		  	"mov rdi, 8\n"
		    "call malloc\n"
		    "pop r8\n"
		    "mov [rax], r8\n"
          
	        "bin_div_finish:\n"
	        "leave\n"
	        "ret\n"
	        "bin_div_exit:\n")))

(define cg-bin-mul
	(lambda ()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, bin_mul_body\n"
            "mov [L_global16], rax\n"
            "jmp bin_mul_exit\n"
            
            "bin_mul_body:\n"
        	"push rbp\n"
      		"mov rbp, rsp\n"
      		"mov rcx ,arg_count\n"
      		"mov rax ,An(0)\n"                

      		"cmp rcx, 2\n"         			; nil + 1 arguments?
      		"jne bin_mul_finish\n"
     
      		"mov rax ,qword [rax]\n"                     
      		"mov rcx, rax\n"
      		"TYPE rcx\n"
      		"cmp rcx, T_INTEGER\n"
      		"je bin_mul_arg1_int_check_arg2\n"
      
      		"bin_mul_arg1_frac_check_arg2:\n"
      		"mov rbx, An(1)\n"
       		"mov rbx, qword [rbx]\n"                    
      		"mov rcx, rbx\n"
      		"TYPE rcx\n"
      		"cmp rcx, T_INTEGER\n"
      		"jne bin_mul_arg1_frac_arg2_frac\n"
      		"mov r10, rax\n"
      		"mov rax, rbx\n"
      		"mov rbx, r10\n"
      		"jmp bin_mul_arg1_int_arg2_frac\n"
      
      		"bin_mul_arg1_frac_arg2_frac:\n"
      		"mov rcx, rax\n"				
      		"CAR rcx\n"
      		"DATA rcx\n"					;rcx holds arg1 numerator	
      		"mov r11, rax\n"      			;rdx keeps rax value
      		"CDR r11\n"
      		"DATA r11\n"
      
     		"mov r10, rbx\n"					
      		"CAR r10\n"
      		"DATA r10\n"					;r10 holds arg2 numerator
      		"mov r12, rbx\n"				;r12 keeps r10 value
      		"CDR r12\n"
      		"DATA r12\n"
      		"mov r8, r11\n"					;backup first denominator

      		"MULT r11, r12\n"				;now r11 holds arg1 denominator * arg2 denominator
      		"MULT r10, rcx\n"				;now r10 holds arg1 numerator * arg2 numerator
      		"mov rax, r10\n"	
      		"mov r12, r11\n"					
      		"jmp bin_mul_simplify_and_create_fraction\n"                                             
      		
		  	"bin_mul_arg1_int_check_arg2:\n"
		  	"mov rbx, An(1)\n"
		  	"mov rbx, qword [rbx]\n"                    
		  	"mov rdx, rbx\n"
		  	"TYPE rdx\n"
		  	"cmp rdx, T_INTEGER\n"
		  	"je bin_mul_arg1_int_arg2_int\n"

		  	"bin_mul_arg1_int_arg2_frac:\n"
		  	"mov r10, rbx\n"
		  	"CAR r10\n"
		  	"DATA r10\n";we will put in r10 second_numerator
		  	"mov r12, rbx\n"
		  	"CDR r12\n"
		  	"DATA r12\n";we will put in r12 second_denominator
		  	"mov r8,rax\n"
		  	"DATA r8\n"
		  	"MULT r8,r10\n"
		  	
		  	"mov rax,r8\n";now rax holds first_numerator*second_numerator

		  	;rax holds new numerator
		  	;r12 holds new denominator
		  	"bin_mul_simplify_and_create_fraction:\n"
		  	"mov r8,  rax\n"
		  	"mov r9, r12\n"
		  	"push r9\n"
		  	"push r8\n"
		  	"call simplify_fraction\n"
		  	"add rsp, 16\n"
		  	"jmp bin_mul_create_new_fraction\n"

		  	"bin_mul_arg1_int_arg2_int:\n"
		  	"DATA rbx\n" 
		  	"DATA rax\n"
		  	"mov r10, rax\n"     
		  	"MULT rbx,r10\n"
		  	"shl rbx, 4\n"
		  	"or rbx, T_INTEGER\n"
		  	"push rbx\n"
		  	"mov rdi, 8\n"
		  	"call malloc\n"
		  	"pop rbx\n"
		  	"mov [rax],rbx\n"
		                        
		  	"jmp bin_mul_finish\n"                     
                    
		  	"bin_mul_create_new_fraction:\n"
		  	"cmp r9, 1\n"
		  	"je bin_mul_create_new_int\n"

		  	"shl r8, 4\n"
		  	"or r8, T_INTEGER\n"		;r8 holds numerator in data structure
		  	"shl r9, 4\n"
		  	"or r9, T_INTEGER\n"		;r9 holds denominator in data structure
		    
		    "mov rdi, 8\n"
		    "call malloc\n"
		    "mov rbx, rax\n"
		    "mov [rbx], r8\n"
		    "push rbx\n"

		    "mov rdi, 8\n"
		    "call malloc\n"
		    "pop rbx\n"
		    "mov rcx, rax\n"
		    "mov [rcx], r9\n"
		    "push rbx\n"
		    "push rcx\n"

		    "mov rdi, 8\n"
		    "call malloc\n" 
		    "pop rcx\n"
		    "pop rbx\n"
                                               
		  	"MAKE_LITERAL_FRACTION_WITH_REGS rax, rbx, rcx\n"
		  	"jmp bin_mul_finish\n"
		          
		   	"bin_mul_create_new_int:\n"

		   	"shl r8, 4\n"
		  	"or r8, T_INTEGER\n"
		  	"push r8\n"

		  	"mov rdi, 8\n"
		    "call malloc\n"
		    "pop r8\n"
		    "mov [rax], r8\n"
          
	        "bin_mul_finish:\n"
	        "leave\n"
	        "ret\n"
	        "bin_mul_exit:\n")))

(define cg-apply
 	(lambda()       
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, apply_body\n"
            "mov [L_global11], rax\n"
            "jmp apply_exit\n"
            
            "apply_body:\n"
    		"push rbp\n"
            "mov rbp, rsp\n"
	        "mov rax, An(0)\n"				;closure
	        "mov rax, qword [rax]\n"
	        "mov r10, qword [rbp]\n" 		;old rbp
	        "mov r11,qword [rbp+8]\n" 		;ret addr
	        "mov r12, rbp\n"
	        "add r12, 5*8\n"
	        
	        "mov rbx, rax\n" 
	        "TYPE rbx\n"
	        "cmp rbx, T_CLOSURE\n"
	        "jne apply_finish\n"
	         
	        "mov rcx, An(1)\n"
	        "mov rcx, qword [rcx]\n"
	        "mov rbx, rcx\n"
	        "TYPE rbx\n"
	        "cmp rbx, T_PAIR\n"
	        "je apply_start\n"

	        "cmp rbx, T_NIL\n"
	        "jne apply_finish\n"

	        "apply_start:\n"
	        "mov rsi, 0\n"

		    "apply_calculate_list_length:\n"
		    "cmp rbx, T_NIL\n"
		    "je apply_calculate_list_length_done\n"
		    "CDR rcx\n"
		    "mov rbx, rcx\n"
		    "TYPE rbx\n"
		    "inc rsi\n"
		    "jmp apply_calculate_list_length\n"

	        "apply_calculate_list_length_done:\n"
	        "shl rsi, 3\n"
	        "sub r12, rsi\n"
	        "shr rsi, 3\n"
	        
	        "mov rdi, 0\n"
	        "mov rcx, An(1)\n"  			 
	        "mov rcx, qword [rcx]\n"

	        "apply_loop:\n"

	        "cmp rdi, rsi\n"
	        "je apply_loop_exit\n"
	        "mov rbx, rcx\n"
	        "DATA_UPPER rbx\n"
			"add rbx, start_of_data\n"    
	        "mov qword [r12 + 8*rdi], rbx\n"
	        "CDR rcx\n"
	        "inc rdi\n"
	        "jmp apply_loop\n"
	        
	        "apply_loop_exit:\n"

	        "sub r12, 8\n"
	        "mov qword [r12],rsi\n"
	        "sub r12, 8\n"
	        "mov rbx, rax\n"
	        "CLOSURE_ENV rbx\n"
	        "mov qword [r12], rbx\n"
	        "sub r12, 8\n"
	        "mov qword [r12], r11\n"
	        "mov rsp, r12\n"	        
	        "mov rbp, r10\n"
	        "mov rbx, rax\n"
	        "TYPE rbx\n"
	        
	        "cmp rbx, T_CLOSURE\n"
	        "jne apply_finish\n"
	        "CLOSURE_CODE rax\n"
	        "jmp rax\n"
	        "apply_finish:\n"
	        "leave\n"
	        "ret\n"
	        "apply_exit:\n" )))

(define cg-symbol->string
	(lambda ()
		(string-append
			"mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, symbol_to_string_body\n"
            "mov [L_global33], rax\n"
            "jmp symbol_to_string_exit\n"
            
            "symbol_to_string_body:\n"
    		"push rbp\n"
            "mov rbp, rsp\n"
            "mov rax, An(0)\n"
            "mov rax, [rax]\n"
            "DATA rax\n"
			"add rax , start_of_data\n"
	        "symbol_to_string_finish:\n"
	        "leave\n"
	        "ret\n"
	        "symbol_to_string_exit:\n" )))

(define cg-vector
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, custom_vector_body\n"
            "mov [L_global9], rax\n"
            "jmp custom_vector_exit\n"
            
            "custom_vector_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"

	        "push rbx\n"
	        "shl rbx, 3\n"
	        "mov rdi, rbx\n"
	        "call malloc\n"
	        "pop rbx\n"
 
	        ;rax= pointer to address of rbx*8 bytes, rbx=length of vector
	        "mov r10, 0\n" ;counter
	        "for_vector:\n"
	        "cmp r10, rbx\n"
	        "je end_of_vector\n"

	        "mov rdx, An(r10)\n" 
	        "mov qword [rax+r10*8], rdx\n"
	        "inc r10\n"
	        "jmp for_vector\n"
	        "end_of_vector:\n"

	        "mov rcx, rax\n"
	        "shl rbx, 3\n"
	        "MAKE_LITERAL_VECTOR_WITH_REGS rcx, rbx\n" ;at the end of macro, rax is a literal vector
	        "mov rcx, rax\n"
	        "push rcx\n"
	        "mov rdi, 8\n"
	        "call malloc\n"
	        "pop rcx\n"
	        "mov [rax], rcx\n"

	        "custom_vector_finish:\n"
	        "leave\n"
	        "ret\n"
	        "custom_vector_exit:\n" )))

	(define cg-vector-length
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, vector_length_body\n"
            "mov [L_global35], rax\n"
            "jmp vector_length_exit\n"
            
            "vector_length_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 1\n" 
	 		"jne vector_length_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "mov rbx,rax\n"
	        "TYPE rax\n"
	        "cmp rax, T_VECTOR\n"
	        "jne vector_length_finish\n"
	        "VECTOR_LENGTH rbx\n"
	        "shl rbx, TYPE_BITS\n"
	        "add rbx, T_INTEGER\n"
	        "mov rdi,8\n"
	        "call malloc\n"
	        "mov [rax], rbx\n"

	        "vector_length_finish:\n"
	        "leave\n"
	        "ret\n"
	        "vector_length_exit:\n" )
	))

(define cg-vector-ref
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, vector_ref_body\n"
            "mov [L_global36], rax\n"
            "jmp vector_ref_exit\n"
            
            "vector_ref_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 2\n" 
	 		"jne vector_ref_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "mov rbx, rax\n"
	        
	        "mov rcx, An(1)\n"
	        "mov rcx, [rcx]\n"
	        "mov rdx, rcx\n"
	        "DATA rdx\n"

	        "TYPE rax\n"
	        "cmp rax, T_VECTOR\n"
	        "jne vector_ref_finish\n"

	        "TYPE rcx\n"
	        "cmp rcx, T_INTEGER\n"
	        "jne vector_ref_finish\n"

	        "VECTOR_REF rcx, rbx, rdx\n"
	        "push rcx\n"
	        "mov rdi,8\n"
	        "call malloc\n"
	        "pop rcx\n"
	        "mov [rax], rcx\n"

	        "vector_ref_finish:\n"
	        "leave\n"
	        "ret\n"
	        "vector_ref_exit:\n" )))

(define cg-vector-set
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, vector_set_body\n"
            "mov [L_global37], rax\n"
            "jmp vector_set_exit\n"
            
            "vector_set_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 3\n" 
	 		"jne vector_set_finish\n"

	 		"mov rax, An(0)\n" ;vector	        
	        "mov rax, [rax]\n"
	        "mov rbx, rax\n" 

	        "mov r11, An(1)\n" ;index
	        "mov r11, [r11]\n"
	        "mov rdx, r11\n"
	        "DATA rdx\n"

	       	"mov rcx, An(2)\n" ;address of item

	        "TYPE rax\n"
	        "cmp rax, T_VECTOR\n"
	        "jne vector_set_finish\n"

	        "TYPE r11\n"
	        "cmp r11, T_INTEGER\n"
	        "jne vector_set_finish\n"

	        ;rbx=vector, rdx=index, rcx=item

	        "mov r12, rbx\n"
	        "VECTOR_ELEMENTS r12\n"
	        "mov [r12 + rdx*8], rcx\n"

	        "mov rax, L_const0\n"

	        "vector_set_finish:\n"
	        "leave\n"
	        "ret\n"
	        "vector_set_exit:\n" )))

;((0 null?) (1 boolean?) (2 char?) (3 integer?) (4 number?) (5 rational?) (6 pair?) (7 string?) (8 symbol?) (9 vector?) (10 procedure?) (11 apply) (12 b<) (13 b=) (14 b+) (15 b/) (16 b*) (17 b-) (18 car) (19 cdr) (20 char->integer) (21 cons) (22 denominator) (23 eq?) (24 integer->char) (25 make-string) (26 make-vector) (27 numerator) (28 remainder) (29 string-length) (30 string-ref) (31 string-set!) (32 string->symbol) (33 symbol->string) (34 vector) (35 vector-length) (36 vector-ref) (37 vector-set!) (38 not) (39 list) (40 x))


(define built-in-map
  ;; <func-name, func-label>
  '((null?  "L_global0")
    (boolean? "L_global1")
    (char? "L_global2")
    (integer? "L_global3")
    (number? "L_global4")
    (rational? "L_global5")
    (pair? "L_global6")
    (string? "L_global7")
    (symbol? "L_global8")
    (vector? "L_global9")
    (procedure? "L_global10")

    (apply "L_global11")

    (b< "L_global12")
    (b= "L_global13")
    (b+ "L_global14")
    (b/ "L_global15")
    (b* "L_global16")
    (b- "L_global17")

    (car "L_global18")
    (cdr "L_global19")
    (char->integer "L_global20")
    (cons "L_global21")
    (denominator "L_global22")
    (eq? "L_global23")
    (integer->char "L_global24")
    (make-string "L_global25")
    (make-vector "L_global26")
    (numerator "L_global27")
    (remainder "L_global28")
    (string-length "L_global29")
    (string-ref "L_global30")
    (string-set! "L_global31")
    (string->symbol "L_global32")
    (symbol->string "L_global33")
    (vector "L_global34")
    (vector-length "L_global35")
    (vector-ref "L_global36")
    (vector-set! "L_global37")))

(define built-in-funcs
  (map first built-in-map))

(define built-in?
  (lambda (func)
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
     newLine
     (cg-cons)
     newLine
     (cg-car)
     newLine
     (cg-cdr)
     newLine
     (cg-string->symbol)
     newLine
     (cg-integer->char)
     newLine 
     (cg-char->integer)
     newLine
     (cg-numerator)
     newLine
     (cg-eq)
     newLine
     (cg-make-string)
     newLine
     (cg-make-vector)
     newLine
     (cg-remainder)
     newLine
     (cg-string-length)
     newLine
     (cg-string-ref)
     newLine
     (cg-string-set)
     newLine
     (cg-less-than)
     newLine 
     (cg-eq?)
     newLine
     (cg-bin-plus)
     newLine
     (cg-bin-div)
     newLine
     (cg-bin-mul)
     newLine
     (cg-bin-minus)
     newLine
     (cg-apply)
     newLine
     (cg-symbol->string)
     newLine
     (cg-vector)
     newLine
     (cg-vector-length)
     newLine
     (cg-vector-set)

     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Type Checks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cg-type-check
  (lambda (type-label . sub-types)
    (string-append
     type-label "_create_closure:" newLine
     "mov rdi, 16\n"
     "call malloc\n"
     "mov rbx, qword 0\n"
     "MAKE_LITERAL_CLOSURE rax, rbx, " type-label "_body\n"
     "mov [" type-label "], rax\n"
     "jmp " type-label "_exit\n"
     
     type-label "_body:" newLine
     tab "push rbp" newLine
     tab "mov rbp, rsp" newLine
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
     tab "ret" newLine
     type-label "_exit:" newLine
     tab "NOP" newLine)))

(define make-pred-label
  (lambda (index)
    (string-append fvar-label (number->string index))))

(define cg-null?
  (cg-type-check (make-pred-label 0) "T_NIL"))

(define bool-pred-label "L_bool_check")

(define cg-bool?
  (cg-type-check(make-pred-label 1) "T_BOOL"))

(define char-pred-label "L_char_check")

(define cg-char?
  (cg-type-check (make-pred-label 2) "T_CHAR"))

(define integer-pred-label "L_integer_check")

(define cg-integer?
  (cg-type-check (make-pred-label 3) "T_INTEGER"))

(define number-pred-label "L_number_check")

(define cg-number?
  (cg-type-check (make-pred-label 4) "T_INTEGER" "T_FRACTION"))

(define rational-pred-label "L_rational_check")

(define cg-rational?
  (cg-type-check (make-pred-label 5) "T_INTEGER" "T_FRACTION"))

(define pair-pred-label "L_pair_check")

(define cg-pair?
  (cg-type-check (make-pred-label 6) "T_PAIR"))

(define string-pred-label "L_string_check")

(define cg-string?
  (cg-type-check (make-pred-label 7) "T_STRING"))

(define symbol-pred-label "L_symbol_check")

(define cg-symbol?
  (cg-type-check (make-pred-label 8) "T_SYMBOL"))

(define vector-pred-label "L_vector_check")

(define cg-vector?
  (cg-type-check (make-pred-label 9) "T_VECTOR"))

(define closure-pred-label "L_closure_check")

(define cg-closure?
  (cg-type-check (make-pred-label 10) "T_CLOSURE"))

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
