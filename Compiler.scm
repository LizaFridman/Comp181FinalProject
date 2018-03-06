(load "project/sexpr-parser.scm")
(load "project/tag-parser.scm")
(load "project/semantic-analyzer.scm")

(define to-string
	(lambda (exp)
		(cond ((number? exp) (number->string exp))
              ((symbol? exp) (symbol->string exp))
              (else exp))))
;labels:
(define env_level -1)
(define label-start-index 1)
(define update-label
    (lambda (label_name)
        (let ((curr-index label-start-index))
            (set! label-start-index (+ label-start-index 1))
            (string-append label_name (number->string curr-index)))))
(define make_label
	(lambda (label-prefix)
		(lambda ()
			(let ((index 0))
				(lambda ()
					(set! index (+ index 1))
					(string-append label-prefix (number->string index)))))))
(define end_of_code ((make_label "code_end")))
(define loop_to_copy_args ((make_label "copy_arg_loop")))
(define end_of_if ((make_label "if_exit")))
(define error? ((make_label "error?")))
(define not_pushing ((make_label "don't_push_args")))
(define global_var ((make_label "global_var")))
(define loop_to_copy_envs ((make_label "copy_envs_loop")))
(define else_of_if ((make_label "if_else_")))
(define end_of_copy_args_lbl ((make_label "end_of_copy_args")))
(define end_of_of_stack ((make_label "end_of_fix_stack_loop")))
(define end_of_or ((make_label "or_exit")))
(define end_of_applic ((make_label "applic_else")))
(define end_of_copy_envs ((make_label "end_of_copy_envs")))
(define code ((make_label "code_start")))
(define new_env ((make_label "new_env")))
(define loop_to_fix_stack ((make_label "fix_stack_loop")))

(define str-list->delimited-str
	(lambda (lst)
		(if (= (length lst) 0) 
			""
			(fold-left (lambda (acc x) (string-append acc ", " x))
						(car lst)
						(cdr lst)))))
(define remove-dups
    (lambda (lst)
        (fold-left (lambda (acc curr)
                        (if (not (member curr acc))
                       (append acc (list curr))
                       acc
                       ))
                    (list)
                    lst)))
(define emptry_string?
	(lambda (str)
		(= (string-length str) 0)))
(define string->str-list 
	(lambda (str)
		(let* ((ans (fold-left (lambda (acc ch)
								(let ((this_str (car acc)) (so_far (cdr acc)))
								(cond ((equal? ch #\nul) ;deal with special chars
										((equal? ch #\newline)
										(if (emptry_string? this_str)
											(cons "" (append so_far (list "CHAR_NEWLINE")))
											(cons "" (append so_far (list (string-append "\"" this_str "\"")) (list "CHAR_NEWLINE")))))
										(if (emptry_string? this_str)
											(cons "" (append so_far (list "CHAR_NUL")))
											(cons "" (append so_far (list (string-append "\"" this_str "\"")) (list "CHAR_NUL")))))
									  ((equal? ch #\space) 
									  	(if (emptry_string? this_str)
											(cons "" (append so_far (list "CHAR_SPACE")))
											(cons "" (append so_far (list (string-append "\"" this_str "\"")) (list "CHAR_SPACE")))))
									  ((equal? ch #\tab)
										(if (emptry_string? this_str)
											(cons "" (append so_far (list "CHAR_TAB")))
											(cons "" (append so_far (list (string-append "\"" this_str "\"")) (list "CHAR_TAB")))))
									  
									  ((equal? ch #\return)
									  	(if (emptry_string? this_str)
											(cons "" (append so_far (list "CHAR_RETURN")))
											(cons "" (append so_far (list (string-append "\"" this_str "\"")) (list "CHAR_RETURN")))))
									  
									  (else 
											(cons (string-append this_str (list->string (list ch))) so_far)))))

							  	(list (string))
							 	(string->list str))))
			(if (equal? (car ans) "")
				(cdr ans)
				(append (cdr ans) (list (string-append  (car ans)))))
		)
	))

(define turn_to_ascii_strings
	(lambda (str)
		(map (lambda (x) (number->string (char->integer x))) (string->list str))))


(define c-table `(,void () ,#t ,#f))

(define add_to_c-table ;takes pe and updates the c-table
    (lambda (pe)
        (cond 	((or (null? pe) (not (list? pe))) #f)
              	((equal? (car pe) 'const)
              		(let ((val (cadr pe)))
	                	(cond  	((null? val) #f) 
	                			((pair? val)
	                       		 	(begin (add_to_c-table `(const ,(car val)))
	                               		   (add_to_c-table `(const ,(cdr val)))
	                               		   (set! c-table (append c-table (list val))))) 
	                		   	((vector? val)
	                        		(begin (vector-map (lambda (e) (add_to_c-table `(const ,e))) val)
	                            	   	   (set! c-table (append c-table (list val)))))
	                      	   	((and (number? val) (not (integer? val)))
	                                (let* ((original-num (numerator val))
	                                	   (original-den (denominator val))
	                                	   (gcd-val (gcd original-num original-den))
	                                       (updated-num (/ original-num gcd-val))
	                                       (updated-den (/ original-den gcd-val)))
	                                       (begin (add_to_c-table `(const ,updated-num))
	                                           (add_to_c-table `(const ,updated-den))
	                                           (set! c-table (append c-table (list val))))))
	                          	((symbol? val) 
	                          		(begin 	(set! c-table (append c-table (list (symbol->string val))))
		                                    (set! c-table (append c-table (list val))))) 
		                      	(else (set! c-table (append c-table (list val)))))))
              					(else (map add_to_c-table pe)))))
                   
(define remove-duplicates-from-c-table
    (lambda ()
        (set! c-table (remove-dups c-table))))

(define get_c_label
    (lambda(constant tagged-table)
        (let ((val (cadar tagged-table))
        	   (var_label (caar tagged-table)))
        	  (if (equal? constant val)
        	  		var_label
        	  	  (get_c_label constant (cdr tagged-table))))))
                    
(define add_ids_to_c_table
    (lambda()
        (set! c-table 
        	(fold-left
                (lambda (acc-table constant)
                	(let ((const-lbl "const_"))
                		(append acc-table 
			                    (list (cond 	
			                    		((equal? constant void) `(,(update-label const-lbl) ,constant T_VOID))
			                          	((equal? constant '()) `(,(update-label	const-lbl) ,constant T_NIL))
			                          	((equal? constant #t) `(,(update-label const-lbl) ,constant T_BOOL))
			                          	((equal? constant #f) `(,(update-label const-lbl) ,constant T_BOOL))
			                        	((number? constant)
			                            	(let* ((original-num (numerator constant))
			                                	   (original-den (denominator constant))
			                            		   (gcd-val (gcd original-num original-den))
			                                       (updated-num (/ original-num gcd-val))
			                                       (updated-den (/ original-den gcd-val)))
			                                (if (integer? constant)
			                                    `(,(update-label const-lbl) ,updated-num T_INTEGER)
			                                    `(,(update-label const-lbl) ,constant T_FRACTION))))
			                        	((char? constant) `(,(update-label const-lbl) ,constant T_CHAR))
				                        ((vector? constant)
				                                `(,(update-label const-lbl) ,constant T_VECTOR))
				                        ((string? constant)
				                                `(,(update-label const-lbl) ,constant T_STRING))
				                        ((symbol? constant)
				                            `(,(update-label const-lbl) ,constant T_SYMBOL))
				                        ((pair? constant)
				                            `(,(update-label const-lbl) ,constant T_PAIR))
				                        (else (error 'constant "error in the const table construction"))))))) '() c-table))))
(define create-c-table ; run this when compilation starts
	(lambda (exprs)
		(begin (add_to_c-table exprs)
			   (remove-duplicates-from-c-table)
			   (add_ids_to_c_table))))
(define get-const-string
	(lambda ()
		(fold-left
            (lambda (acc el)
            	(let ((const-val (cadr el))
					  (const-label (car el)))
                    (cond 	
				    	   	((equal? const-val '()); null
								(string-append acc (string-append const-label ":\n\t dq SOB_NIL\n")))
							((equal? const-val #t); bool-t
								(string-append acc (string-append const-label ":\n\t dq SOB_TRUE\n")))
							((equal? const-val void)
								(string-append acc (string-append const-label ":\n\t dq SOB_VOID\n")))
							((equal? const-val #f); bool-f
								(string-append acc (string-append const-label ":\n\t dq SOB_FALSE\n")))
	                    	((number? const-val);num
	                        	(let* ((original-num (numerator const-val))
	                        		   (original-den (denominator const-val))
	                        		   (gcd-val (gcd original-num original-den))
	                                   (updated-num (/ original-num gcd-val))
	                                   (updated-den (/ original-den gcd-val)))
	                            	(if (integer? const-val)
	                            		(string-append acc (string-append const-label ":\n\t dq MAKE_LITERAL(T_INTEGER, "(number->string updated-num)")\n"));add both
	                            		(string-append acc (string-append const-label ":\n\t dq MAKE_LITERAL_FRACTION("(get_c_label updated-num c-table) 
	                            																				  	  ", "(get_c_label updated-den c-table)")\n")))))
	                    	((char? const-val) ;char
								(string-append acc (string-append const-label ":\n\t dq MAKE_LITERAL(T_CHAR, "(number->string (char->integer const-val))")\n")))
	                        ((vector? const-val);vector
	                            (let ((label-lst (map (lambda (x) (get_c_label x c-table)) (vector->list const-val)))
	                            	  (vector-len (vector-length const-val)))
                        			(string-append acc (string-append const-label ":\n\t MAKE_LITERAL_VECTOR " (str-list->delimited-str label-lst)"\n"))))
	                        ((string? const-val);string
                        		(string-append acc (string-append const-label ":\n\t MAKE_LITERAL_STRING " (str-list->delimited-str (turn_to_ascii_strings (car (string->str-list const-val))))"\n")))
	                        ((pair? const-val);pair
	                        	(string-append acc (string-append const-label ":\n\t dq MAKE_LITERAL_PAIR("(get_c_label (car const-val) c-table)
	                            																	    ", "(get_c_label (cdr const-val) c-table)")\n")))
	                        ((symbol? const-val);symbol
                        		(string-append acc (string-append const-label ":\n\t dq MAKE_LITERAL_SYMBOL("(get_c_label (symbol->string const-val) c-table)")\n")))
	                        
	                        (else (error 'constant "error making c-table creation")))))
                ""
                c-table)))
;f-table
(define string_for_f_table_initialization '((car "car") 
							   (cdr "cdr")
							   (cons "cons")
							   (boolean? "boolean?")
							   (pair? "pair?")
							   (char? "char?")
							   (list "list")
							   (null? "null?")
							   (apply "apply")
							   (integer? "integer?")
							   (procedure? "procedure?")
							   (string? "string?")
							   (symbol? "symbol?")
							   (vector? "vector?")
							   (zero? "zero?")
							   (not "not")
							   (car "car")
							   (cdr "cdr")
							   (number? "number?")
							   (rational? "rational?") 
							   (eq? "eq?")
							   (+ "plus")
							   (- "minus")
							   (/ "div")
							   (* "mul")
							   (< "less_than")
							   (> "greater_than")
							   (= "equal")
							   (char->integer "char_to_integer")
							   (integer->char "integer_to_char")
							   (numerator "numerator")
							   (string-ref "string_ref")
							   (vector-ref "vector_ref")
							   (remainder "remainder")
							   (denominator "denominator")
							   (string-length "string_length")
							   (vector-length "vector_length")
							   
							    (string-set! "string_set")
							   (vector-set! "vector_set")
							   (bin_equal "bin_equal")
							   (bin_plus  "bin_plus")
							   (bin_minus "bin_minus")
							   (bin_mul   "bin_mul")
							   (bin_div   "bin_div")
							   (bin_less_than   "bin_less_than")
							   (bin_greater_than   "bin_greater_than")
							  
							   (make-string "make_string")
							   (make-vector "make_vector")
							   (vector "custom_vector")
							   
							   (map "map")
							   
							   (symbol->string "symbol_to_string")
							   (string->symbol "string_to_symbol")))

(define global-var-table string_for_f_table_initialization) ;this build the table into global-var-table
(define add-to-global-var-table
    (lambda (pe)
        (cond ((or (not (list? pe)) (null? pe)) #f)
              ((and (equal? (car pe) 'fvar) (not (assq (cadr pe) string_for_f_table_initialization)))
                (set! global-var-table (append global-var-table (list (list (cadr pe) (global_var))))))
              (else (map add-to-global-var-table pe)))))

(define remove-duplicates-from-global-var-table
    (lambda ()
        (set! global-var-table (remove-dups global-var-table))))

(define get-label-from-global-var-table
    (lambda (val curr-var-table)
        (let* ((curr-row (car curr-var-table))
        	   (curr-val (car curr-row))
        	   (curr-label (cadr curr-row)))
        	  (if (equal? val curr-val)
        	  		curr-label
        	  	  (get-label-from-global-var-table val (cdr curr-var-table))))))

(define create-global-var-table
	(lambda (pe-lst)
		(begin (add-to-global-var-table pe-lst)
			   (remove-duplicates-from-global-var-table)
			)))

(define get-fvar-string
	(lambda ()
		(fold-left
	        (lambda (a b)
	            	(string-append a (cadr b) ":\n\tdq SOB_UNDEFINED\n"))
	        (string)
	        global-var-table)))

;------------------------SYMBOL-TABLE------------------------------

(define count-symbols
	(lambda ()
		(fold-left 
    		(lambda (acc el)
    			(if (equal? (caddr el) 'T_SYMBOL)
    				(+ acc 1)
    				acc))
    		0
    		c-table)))



(define cg-symbol-table 
    (lambda (curr-c-table index number-of-symbols-left)
        (if (equal? number-of-symbols-left 0)
        	(if (equal? index 0)
                (string-append "symbol_table:\n\t dq const_2\n") 
            	(string-append "symbol_table:\n\t dq symbol_0\n"))
            (if (not (null? curr-c-table))
            	(let* ((el (car curr-c-table)) 
    		  		  (const-label (car el)) 
    		  		  (const-val (cadr el)) 
    		  		  (const-type (caddr el)))
                	(if (not (equal? const-type 'T_SYMBOL))
                    	(cg-symbol-table (cdr curr-c-table) index number-of-symbols-left)
                    	(if (equal? number-of-symbols-left 1)
							(string-append "symbol_"(number->string index)": 
								\n\t dq MAKE_LITERAL_PAIR("const-label", const_2)\n" (cg-symbol-table (cdr curr-c-table) (+ index 1) (- number-of-symbols-left 1)))
							(string-append "symbol_"(number->string index)": 
								\n\t dq MAKE_LITERAL_PAIR("const-label", symbol_"(number->string (+ index 1))")\n"(cg-symbol-table (cdr curr-c-table) (+ index 1) (- number-of-symbols-left 1))))))
            (cg-symbol-table (cdr c-table) index number-of-symbols-left)))))
;cg-code
(define code_gen
    (lambda (pe)
        (cond 
            ((equal? (car pe) 'if3) (cg-if  pe))
                        ((equal? (car pe) 'pvar) (cg-pvar pe))

            ((equal? (car pe) 'box-set) (cg-box-set pe))
            ((equal? (car pe) 'box-get) (cg-box-get pe))
            ((equal? (car pe) 'seq) (cg-seq pe))
            ((equal? (car pe) 'or)  (cg-or  pe))
            ((equal? (car pe) 'set) (cg-set pe))
            ((equal? (car pe) 'box) (cg-box pe))
            ((equal? (car pe) 'applic)  (cg-app pe))
           	((equal? (car pe) 'tc-applic)  (cg-app pe))
           	((equal? (car pe) 'lambda-simple)  (cg-lambda-simple pe))
            ((equal? (car pe) 'lambda-opt)  (cg-lambda-opt pe))
            ((equal? (car pe) 'define) (cg-define pe))
            ((equal? (car pe) 'const) (cg-const pe))
            ((equal? (car pe) 'fvar)  (cg-fvar pe))
            ((equal? (car pe) 'bvar) (cg-bvar pe))

            (else "mov rax, const_4\n")
        )))

(define cg-define
    (lambda(pe)
    	(let ((var-name (cadadr pe)) (pe-val (caddr pe)) (void_lbl "const_1"))
	        (string-append
	            (code_gen pe-val)
	            "mov [" (cadr (assq var-name global-var-table)) "], rax\n"
	            "mov rax, "void_lbl"\n"))))
(define cg-const
    (lambda(pe)
        (string-append "mov rax, "(to-string (get_c_label (cadr pe) c-table))"\n")))
(define cg-fvar
	(lambda(pe)
		(string-append "mov rax, ["(get-label-from-global-var-table (cadr pe) global-var-table)"]\n")))
(define cg-or
    (lambda (expr)
        (let ((exit (end_of_or)) (arg_start (caadr expr)))
            (string-append
                (code_gen arg_start)
                (fold-right
                    string-append
                    ""
                    (map (lambda (next-el)
                            (string-append "cmp rax , const_4\n"
                                           "jne" " " exit "\n"
                                           (code_gen next-el)))
                    		(cdadr expr)))
                exit ":\n"))))



(define cg-seq
    (lambda (pe)
        (fold-left
            string-append
            ""
            (map code_gen (cadr pe)))))

(define cg-app
    (lambda (pe)
        (let ((args (reverse (caddr pe))) (func (cadr pe)) (exit (end_of_applic)))
        (string-append
        	;"push const_2\n"
            (fold-left
                string-append
                ""
                (map (lambda (el)
                    (string-append
                        (code_gen el)
                        "push rax\n"))
                 args))
            "push "(number->string (length args))"\n"
			(code_gen func)
			"\t " "mov rax, [rax]\n"
			"mov rbx, rax\n"
			"TYPE rbx\n"
            "cmp rbx, T_CLOSURE\n"
            "jne "exit"\n"
            "\t \t \n"
            "mov rbx, rax\n"            
			"CLOSURE_ENV rbx\n"
			"push rbx\n"	
            "CLOSURE_CODE rax\n"
            "call rax\n"
            exit":\n"
            "add rsp, " (number->string (* (+ 2 (length args)) 8)) " \n"))))

(define cg-tc-app
    (lambda (pe)
        (let ((args (reverse (caddr pe))) (proc (cadr pe)) (exit (end_of_applic)) (loop_to_copy_args (loop_to_copy_args)) (end_of_copy_args (end_of_copy_args_lbl)))
        (string-append
        	;"push const_2\n"
            (fold-left
                string-append
                ""
                (map (lambda (el)
                    (string-append
                        (code_gen el)
                        "push rax\n"))
                 args))
            "push "(number->string (length args))"\n"
			(code_gen proc)
			"mov rax, [rax]\n"
			"mov rbx, rax\n"
			"\t \n TYPE rbx\n"
            "cmp rbx, T_CLOSURE\n"
            "jne "exit"\n"

            "mov rbx, rax\n"            
			"CLOSURE_ENV rbx\n"
			"push rbx\n"
			" \t ;; save the ret address \n"
			"push ret_addr\n" 
			"mov r8, rbp\n"
			"mov rbp, qword[r8]\n"
			"mov r11,rsp\n"
			"mov r15,arg_count\n"
			"add r15, 5\n" 
			" \t ;copy the args \n"
			"mov rdi, "(number->string (+ (length args) 4)) "\n"
			loop_to_copy_args":\n"
			"cmp rdi, 0\n"
			"je "end_of_copy_args"\n"
			"mov r12, rdi\n"
			"dec r12\n"
			"shl r12, 3\n"
			"mov r10,qword[r11+r12]\n"
			"dec r15\n"
			"mov r12,r15\n"
			"shl r12, 3\n"
			"mov qword[r8+r12],r10\n"
			"dec rdi\n"
			";\t copy args \n"
			"jmp "loop_to_copy_args"\n"
			end_of_copy_args":\n"
			"mov r12,r15\n"
			"shl r12, 3\n"
			"lea rsp,[r8+r12]\n"
            "CLOSURE_CODE rax\n"
            "jmp rax\n"
            exit":\n"
        	))))
(define cg-if ;if
    (lambda (pe)
        (let ((exit (end_of_if)) (else_part (else_of_if)) (test_if3 (cadr pe)) (if_then_part (caddr pe)) (if_else_part (cadddr pe)))
            (string-append
                (code_gen test_if3)
                "cmp rax, const_4\n"
                "je "else_part"\n"
                (code_gen if_then_part)
                "jmp "exit"\n"
                else_part":\n"
                (code_gen if_else_part)
                exit":\n"))))
(define cg-lambda-simple ;lambda-simple
    (lambda (pe)
        (set! env_level (+ env_level 1))
            (let* ((args (cadr pe)) (body (caddr pe))
            	  (end_of_code (end_of_code)) (loop_to_copy_args (loop_to_copy_args)) (end_of_copy_args (end_of_copy_args_lbl))
            	  (for_copy_envs (loop_to_copy_envs)) (end_of_copy_envs (end_of_copy_envs)) (code_label (code)) (new_env (new_env))
                  (cg_string (string-append
                  	;create new env
                  	"mov rbx, 0\n";env
                    "mov rax, " (number->string env_level) "\n";major
                    "cmp rax, 0\n"
                    "je "end_of_copy_envs"\n"
                    "mov rdi, "(number->string (* 8 (+ 1 env_level)))"\n";for allocating space for new extended env 
                    "call malloc\n"
                    "mov rbx, rax\n"	
                    
                    "mov rax, arg_count\n"
					"mov rdi, 8\n"
					"mul rdi\n"
                    "push rbx\n"	
                    "mov rdi, rax\n"
                    "call malloc\n"
                    "pop rbx\n"
                    "\t "
                    "mov rcx, rax\n"	
                    "push rbx\n"
                    "\t ;copy args \n"
					"mov rdi, 0\n"
					"pop rbx\n"
					loop_to_copy_args":\n"
					"cmp rdi, arg_count\n"
					"je "end_of_copy_args"\n"
					"mov rax, 8\n"
					"mul rdi\n"
					"mov rdx, An(rdi)\n"   ; rdx = i'th argument
					"mov qword [rcx+rax], rdx\n" ; copy arg i into [rcx+8*i]
					"inc rdi\n"




					"jmp "loop_to_copy_args"\n"
					end_of_copy_args":\n"
					"mov qword [rbx], rcx\n"
					"mov r14, env\n"		; rdx=previous env
					"cmp r14, 0\n"
					"je "end_of_copy_envs"\n"
					"mov rdi, 0\n"
					for_copy_envs":\n"
					"cmp rdi, " (number->string env_level) "\n"
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
                    "push rbx\n"
                    "MAKE_LITERAL_CLOSURE rax, rbx, " code_label "\n"
                    "pop rbx\n"
                    "jmp "end_of_code"\n"
					;create code
					code_label":\n"
					"push rbp\n"
					"mov rbp, rsp\n"
					(code_gen body)
					"mov rbx, rax\n"
					"mov rax, arg_count\n"
					"add rax, 1\n"
					"mov rdi, 8\n"
					"mul rdi\n"
					"add rsp, rax\n"
					"mov rax, rbx\n"
					"leave\n"
					"ret\n"
					end_of_code":\n")))
        		(set! env_level (- env_level 1)) 
        		cg_string)))

(define cg-box-set
	(lambda (pe)
		(let ((var (cadr pe)) (val (caddr pe)))
			(string-append
				(code_gen val)
				"mov rbx, rax\n"
				(code_gen var)
				"mov qword [rax], rbx\n"
				"mov rax, const_1\n"
			))))

(define cg-lambda-opt
    (lambda (pe)
        (set! env_level (+ env_level 1))
            (let* ((args (cadr pe))
            	   (body (cadddr pe))
            	   (end_of_code (end_of_code)) (loop_to_copy_args (loop_to_copy_args)) (end_of_copy_args (end_of_copy_args_lbl))
            	   (for_copy_envs (loop_to_copy_envs)) (end_of_copy_envs (end_of_copy_envs)) (code_label (code)) (new_env (new_env))
            	   (for_fix_stack (loop_to_fix_stack)) (end_of_fix_stack (end_of_of_stack)) (dont_push_arg_label (not_pushing))
                   (cg_string (string-append
                  	;create new env
                  	"mov rbx, 0\n";env
                    "mov rax, " (number->string env_level) "\n"
                    "cmp rax, 0\n"
                    "je "end_of_copy_envs"\n"
                    "mov rdi, "(number->string (* 8 (+ 1 env_level)))"\n" 
                    "call malloc\n"
                    "mov rbx, rax\n"	 
                    "mov rax, arg_count\n"
					"mov rdi, 8\n"
					"mul rdi\n"
                    "push rbx\n"	
                    "mov rdi, rax\n"
                    "call malloc\n"
                    "pop rbx\n"
                    "mov rcx, rax\n"	
					"mov rdi, 0\n"
					loop_to_copy_args":\n"
					"cmp rdi, arg_count\n"
					"je "end_of_copy_args"\n"
					"mov rax, 8\n"
					"mul rdi\n"
					"mov rdx, An(rdi)\n"  
					"mov qword [rcx+rax], rdx\n" 
					"inc rdi\n"
					"jmp "loop_to_copy_args"\n"
					end_of_copy_args":\n"

					"mov qword [rbx], rcx\n"

					"mov r14, env\n"		
					"cmp r14, 0\n"
					"jle "end_of_copy_envs"\n"
					"mov rdi, 0\n"
					for_copy_envs":\n"
					"cmp rdi, " (number->string env_level) "\n"
					"je "end_of_copy_envs"\n"
					"mov rax, 8\n"
					"mul rdi\n"
					"mov rcx, qword [r14+rax]\n"
					"mov qword [rbx+rax+8], rcx\n"
					"inc rdi\n"
					"jmp "for_copy_envs"\n"
					"\t ;copy env in loop \n"
					end_of_copy_envs":\n"
                    "push rbx\n"
                    "push rcx\n"
                    "mov rdi, 16\n"
                    "call malloc\n" 
                    "pop rcx\n"
                    "pop rbx\n"
                    "MAKE_LITERAL_CLOSURE rax, rbx, " code_label "\n"
                    "jmp "end_of_code"\n"
					code_label":\n"
					"push rbp\n"
					"mov rbp, rsp\n"
					"mov rbx, const_2\n"
					"mov r10, arg_count\n"
					for_fix_stack":\n"
					"cmp r10, "(number->string (length args)) "\n"
					"je " end_of_fix_stack "\n"
					"mov rdi, 8\n"
					"call malloc\n"			
					"mov rdx, rbp\n"				
					"add rdx, 4*8\n"	
					"\t ;stack fixer \n"			
					"mov r11, r10\n"				
					"dec r11\n"
					"shl r11, 3\n"							
					"add rdx, r11\n"				
					"mov rdx, qword [rdx]\n"		
					"MAKE_MALLOC_LITERAL_PAIR rax, rdx, rbx\n"	
					"\t ;;rax <- target, rbx <- cdr, rcx <- car \n"
					"mov rbx, rax\n"
					"push rax\n"				
					"dec r10\n"		
					"pop rax\n"			
					"jmp " for_fix_stack "\n"
					
					end_of_fix_stack":\n"
					"cmp rbx, const_2\n"
					"\t; ---------------------dont push the args! \n"
					"mov qword [rbp+4*8+"(number->string (length args))"*8], rbx\n"	;add the list in stack after const params (not optinals)
					(code_gen body)
					"leave\n"
					"ret\n"
					end_of_code":\n")))
        (set! env_level (- env_level 1)) 
	cg_string)))

(define cg-pvar
	(lambda (pe)
		(let ((minor (caddr pe)))
			(string-append "mov rax, qword [rbp+32+"(number->string minor)"*8]\n"))))
(define cg-box
	(lambda (pe)
		(let ((var (cadr pe)))
			(string-append
				(code_gen var)
				"mov rbx, rax\n"
				"mov rdi, 8\n"
				"call malloc\n"
				"mov qword [rax], rbx\n"
			))))

(define cg-box-get
	(lambda (pe)
		(let ((var (cadr pe)))
			(string-append
				(code_gen var)
				"mov qword rax, [rax]\n"
			))))
(define cg-bvar
	(lambda (pe)
		(let ((major (caddr pe)) (minor (cadddr pe)))
			(string-append 
			"mov rax, qword [rbp+16]\n"
			"mov rax, qword [rax+"(number->string major)"*8]\n"
			"mov rax, qword [rax+"(number->string minor)"*8]\n"
			))))

(define cg-set
	(lambda (pe)
		(let ((tag (caadr pe))
               (var (cadadr pe))
               (val (caddr pe)))
			(cond 
                   ((equal? tag 'bvar)
                    (let ((major (car (cddadr pe)))
                         (minor (car(cdr (cddadr pe)))))
                            (string-append
                                (code_gen val)
                                "mov rbx, qword [rbp+16]\n"
                                "mov rbx, qword [rbx+8*"(number->string major)"]\n"
                                "mov qword [rbx+8*"(number->string minor)"], rax\n"
								"mov rax, const_1\n")))
                   ((equal? tag 'pvar) 
                    (let ((minor (car (cddadr pe))))
						(string-append
						(code_gen val)
						"mov qword [rbp+8*(4+"(number->string minor)")], rax\n"
						"mov rax, const_1\n"
						)))
                       ((equal? tag 'fvar) 
                            (string-append
                                (code_gen val)
                                "mov ["(get-label-from-global-var-table var global-var-table)"], rax\n"
                                "mov rax, const_1\n"
                                ))
                      (else "wrong input")))))




;built-in
(define append_build_in_funcs
	(lambda (lst) (append (string->list (string-append lib-map lib-list lib-fold-left lib-bin-append lib-append 
				   						 				lib-equal lib-greater-than lib-less-than lib-less-than lib-plus lib-minus lib-mul lib-div )) lst)))
(define lib-map "(define map (lambda (proc lst) (if (null? lst) 
													lst 
													(cons (proc (car lst)) (map proc (cdr lst))))))\n")
(define lib-list "(define list (lambda x x))\n")
(define lib-fold-left "(define fold_left (lambda (proc init lst) (if (null? lst) 
																	 init 
																	 (fold_left proc (proc init (car lst)) (cdr lst)))))\n")
(define lib-bin-append "(define bin_append (lambda (lst1 lst2) (if (null? lst1) 
																	lst2 
																	(cons (car lst1) (bin_append (cdr lst1) lst2)))))\n")
(define lib-append "(define append (lambda x (fold_left bin_append '() x)))\n")
(define lib-equal "(define = (lambda x (fold_left (lambda (acc y) (and acc (bin_equal (car x) y))) #t x)))\n")
(define lib-greater-than "(define > (lambda x (if (null? (cdr x))
												  #t 
												  (and (bin_greater_than (car x) (car (cdr x))) (apply > (cdr x)))))) \n")
(define lib-less-than "(define < (lambda x (if (null? (cdr x)) 
												#t 
												(and (bin_less_than (car x) (car (cdr x))) (apply < (cdr x)))))) \n")
(define lib-plus "(define + (lambda x (fold_left (lambda (acc y) (bin_plus acc y)) 0 x)))\n")

(define lib-minus "(define - (lambda x (if (null? (cdr x)) 
											(bin_minus 0 (car x)) 
											(fold_left (lambda (acc y) (bin_minus acc y)) (car x) (cdr x)))))\n")
(define lib-mul "(define * (lambda x (fold_left (lambda (acc y) (bin_mul acc y)) 1 x)))\n")

(define lib-div "(define / (lambda x (if (null? (cdr x)) 
										 (bin_div 1 (car x)) 
										 (fold_left (lambda (acc y) (bin_div acc y)) (car x) (cdr x)))))\n")

(define code-cg-library-functions
    (lambda ()
        (string-append
            (cg-cons)
            (cg-car)
            (cg-cdr)
            (cg-null?)
            (cg-pair?)
            (cg-boolean?)
            (cg-char?)
            (cg-integer?)
            (cg-procedure?)
            (cg-string?)
            (cg-symbol?)
            (cg-vector?)
            (cg-zero?)
            (cg-apply)
            (cg-make-string)
            (cg-make-vector)
            (cg-not)
            (cg-string-length)
            (cg-vector-length)
            (cg-vector)
            (cg-char->integer)
            (cg-integer->char)
            (cg-string-ref)
            (cg-vector-ref)
            (cg-symbol->string)
            (cg-string->symbol)
            (cg-bin-mul)
            (cg-bin-div)
            (cg-bin-minus)
            (cg-bin-equal)
            (cg-bin-less-than)
            (cg-bin-greater-than)
            (cg-string-set)
            (cg-vector-set)
            (cg-bin-plus)
            (cg-remainder)
            (cg-denominator)
            (cg-numerator)
            (cg-number?)
            (cg-rational?)
            (cg-eq?)
            )))

(define type_checking

    (lambda (var_type tag)
    	(let ((low_case (string-downcase var_type)))
	        (string-append
	           low_case":\n"
	           "\t push rbx \n"
	            "mov rdi, 16\n"
	            "call malloc\n"
	            "\t ;malloc is called here \n"
	            "\t pop rbx  \n"
	            "mov rbx, qword 0\n"
	            "MAKE_LITERAL_CLOSURE rax, rbx, "low_case"_body\n"
	            "mov ["(symbol->string tag)"], rax\n"
	            "jmp "low_case"_exit\n"
	            low_case"_body:\n"
	            "push rbp\n"
				"mov rbp, rsp\n"
	            "mov rbx, arg_count\n"
	            "cmp rbx, 1\n"
	 			"jne "low_case"_exit\n"
	            "mov r10, An(0)\n"
	            "mov r10, [r10]\n"
	            "TYPE r10\n"
	            "cmp r10, T_"var_type"\n"
	            "jne "low_case"_not\n"
	            "mov rax, const_3\n" 
	            "jmp "low_case"_finish\n"
	            low_case"_not:\n"
	            "mov rax, const_4\n"
	           	low_case"_finish:\n"
	            "leave\n"
				"ret\n"
	            low_case"_exit:\n"))))
        
     (define cg-integer?
    (lambda()
        (type_checking "INTEGER" 'integer?)))
        
(define cg-procedure?
    (lambda()
        (type_checking "CLOSURE" 'procedure?)))
        
(define cg-string?
    (lambda()
        (type_checking "STRING" 'string?)))   
(define cg-null?
    (lambda()
        (type_checking "NIL" 'null?)))

(define cg-pair?
    (lambda()
        (type_checking "PAIR" 'pair?)))
        
(define cg-boolean?
    (lambda()
        (type_checking "BOOL" 'boolean?)))
        (define cg-symbol?
    (lambda()
        (type_checking "SYMBOL" 'symbol?)))
(define cg-vector?
    (lambda()
        (type_checking "VECTOR" 'vector?)))
(define cg-char?
    (lambda()
        (type_checking "CHAR" 'char?)))
(define cg-zero?
    (lambda()
        (string-append
            "push rbx\n"
            "mov rdi, 16\n"
            "pop rbx\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, zero?_body\n"
            "mov [zero?], rax\n"
            "jmp zero?_exit\n"  
            "zero?_body:\n"        
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
            "push rcx\n"
	        "cmp rbx, 1\n" 
	        "pop rcx\n"
	 		"jne zero_finish\n"
	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "TYPE rax\n"
	        "cmp rax, T_INTEGER\n"
	        "je zero_check\n"
	        "cmp rax, T_FRACTION\n"
	        "jne zero_finish\n"
	        "zero_check:\n"
	        "mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "DATA rax\n"
	        "cmp rax, 0\n"
	        "je zero_true\n"
	        "mov rax, const_4\n"
	        "jmp zero_finish\n"
	        "zero_true:\n"
	        "mov rax, const_3\n"

	        "zero_finish:\n"
	        "leave\n"
	        "ret\n"
	        "zero?_exit:\n" )))

(define cg-number?
    (lambda()
        (string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, number?_body\n"
            "mov [number?], rax\n"
            "jmp number?_exit\n"
            
            "number?_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 1\n" 
	 		"jne number?_finish\n"
	 		
	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "TYPE rax\n"
	        "cmp rax, T_INTEGER\n"
	        "je number?_true\n"
	        "cmp rax, T_FRACTION\n"
	        "je number?_true\n"
	        "mov rax, const_4\n"
	        "jmp number?_finish\n"
	        
	        "number?_true:\n"
	        "mov rax, const_3\n"

	        "number?_finish:\n"
	        "leave\n"
	        "ret\n"
	        "number?_exit:\n" )))

(define cg-not
    (lambda ()
        (string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, not_body\n"
            "mov [not], rax\n"
            "jmp not_exit\n"
            
            "not_body:\n"
            "push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 1\n" ; pop n from stack
	 		"jne not_exit\n"
            "mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "cmp rax, [const_4]\n"
	        "je is_false\n"
	        "mov rax, const_4\n"
	        "jmp not_finish\n"
	        "is_false:\n"
	        "mov rax, const_3\n"
	        "not_finish:\n"
	        "leave\n"
	        "ret\n"

            "not_exit:\n")))

(define cg-car
    (lambda()
        (string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, car_body\n"
            "mov [car], rax\n"
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
            "mov [cdr], rax\n"
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

(define cg-cons
    (lambda()
        (string-append
            
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, cons_body\n"
            "mov [cons], rax\n"
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

(define cg-rational?
 	(lambda()       
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, rational?_body\n"
            "mov [rational?], rax\n"
            "jmp rational?_exit\n"
            
            "rational?_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 1\n" 
	 		"jne rational?_finish\n"

	 		"mov rax, An(0)\n"
	        "mov rax, [rax]\n"
	        "TYPE rax\n"
	        "cmp rax, T_INTEGER\n"
	        "je rational?_true\n"
	        "cmp rax, T_FRACTION\n"
	        "je rational?_true\n"
	        "mov rax, const_4\n"
	        "jmp rational?_finish\n"
	        
	        "rational?_true:\n"
	        "mov rax, const_3\n"

	        "rational?_finish:\n"
	        "leave\n"
	        "ret\n"
	        "rational?_exit:\n" )))

(define cg-eq?
 	(lambda()       
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, eq?_body\n"
            "mov [eq?], rax\n"
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
	        "mov rax, const_4\n"
	        "jmp eq?_finish\n"
	        
	        "eq?_true:\n"
	        "mov rax, const_3\n"

	        "eq?_finish:\n"
	        "leave\n"
	        "ret\n"
	        "eq?_exit:\n" )))

(define cg-bin-equal
 	(lambda()       
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, bin_equal_body\n"
            "mov [bin_equal], rax\n"
            "jmp bin_equal_exit\n"
            
            "bin_equal_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 2\n" 
	 		"jne bin_equal_finish\n"

	 		"mov rax, An(0)\n"
	 		"mov rax, [rax]\n"
	 		"mov rcx, rax\n"
	 		"mov rbx, An(1)\n"
	 		"mov rbx, [rbx]\n"
	 		"mov rdx, rbx\n"
	 		"TYPE rax\n"
	 		"TYPE rbx\n"
	        
	        "cmp rax, T_INTEGER\n"
	        "jne check_fraction\n"
	        "cmp rbx, T_INTEGER\n"
	        "jne bin_equal_false\n"
	        
	        "DATA rcx\n"
	        "DATA rdx\n"
	        "cmp rcx, rdx\n"
	        "jne bin_equal_false\n"
	        "mov rax, const_3\n"
	        "jmp bin_equal_finish\n"

	        "check_fraction:\n"
	        "cmp rax, T_FRACTION\n"
	        "jne bin_equal_finish\n"
	        "cmp rbx, T_FRACTION\n"
	        "jne bin_equal_false\n"

	        "mov r8, rcx\n"
	        "mov r9, rdx\n"
	        "CAR rcx\n"
	        "CAR rdx\n"
	        "cmp rcx, rdx\n"
	        "jne bin_equal_false\n"
	        "CDR r8\n"
	        "CDR r9\n"
	        "cmp r8, r9\n"
	        "jne bin_equal_false\n"
	        "mov rax, const_3\n"
	        "jmp bin_equal_finish\n"

	        "bin_equal_false:\n"
	        "mov rax, const_4\n"

	        "bin_equal_finish:\n"
	        "leave\n"
	        "ret\n"
	        "bin_equal_exit:\n" )))

(define cg-bin-plus
	(lambda ()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, bin_plus_body\n"
            "mov [bin_plus], rax\n"
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

(define cg-bin-minus
	(lambda ()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, bin_minus_body\n"
            "mov [bin_minus], rax\n"
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

(define cg-bin-mul
	(lambda ()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, bin_mul_body\n"
            "mov [bin_mul], rax\n"
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

(define cg-bin-div
	(lambda ()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, bin_div_body\n"
            "mov [bin_div], rax\n"
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

(define cg-bin-less-than
 	(lambda()       
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, bin_less_than_body\n"
            "mov [bin_less_than], rax\n"
            "jmp bin_less_than_exit\n"
            
            "bin_less_than_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 2\n" 
	 		"jne bin_less_than_finish\n"

	 		;"push const_2\n"
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
	        
	        "mov rax, const_4\n"
	        "jmp bin_less_than_finish\n"

	        "bin_less_than_true:\n"
	        "mov rax, const_3\n"

	        "bin_less_than_finish:\n"
	        "leave\n"
	        "ret\n"
	        "bin_less_than_exit:\n" )))

(define cg-bin-greater-than
 	(lambda()       
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, bin_greater_than_body\n"
            "mov [bin_greater_than], rax\n"
            "jmp bin_greater_than_exit\n"
            
            "bin_greater_than_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"         
            "mov rbx, arg_count\n"
	        "cmp rbx, 2\n" 
	 		"jne bin_greater_than_finish\n"

	 		;"push const_2\n"
	 		"push An(1)\n"
	 		"push An(0)\n"
	 		"push 2\n"
	 		"push qword 0\n"
	 		"call bin_minus_body\n"
	 		"mov rax, [rax]\n"
	 		"mov rbx, rax\n"
	 		"TYPE rbx\n"
	 		"cmp rbx, T_INTEGER\n"
	 		"je bin_greater_than_check_sign\n"
	 		"CAR rax\n"

	 		"bin_greater_than_check_sign:\n"
	 		"DATA rax\n"
	 		"cmp rax, 0\n"
	 		"jg bin_greater_than_true\n"
	        
	        "mov rax, const_4\n"
	        "jmp bin_greater_than_finish\n"

	        "bin_greater_than_true:\n"
	        "mov rax, const_3\n"

	        "bin_greater_than_finish:\n"
	        "leave\n"
	        "ret\n"
	        "bin_greater_than_exit:\n" )))

(define cg-char->integer
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, char_to_integer_body\n"
            "mov [char_to_integer], rax\n"
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
            "mov [integer_to_char], rax\n"
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
            "mov [numerator], rax\n"
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
            "mov [denominator], rax\n"
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

(define cg-remainder
 	(lambda()       
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, remainder_body\n"
            "mov [remainder], rax\n"
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
            "mov [string_length], rax\n"
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

(define cg-vector-length
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, vector_length_body\n"
            "mov [vector_length], rax\n"
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

(define cg-string-ref
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, string_ref_body\n"
            "mov [string_ref], rax\n"
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

(define cg-vector-ref
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, vector_ref_body\n"
            "mov [vector_ref], rax\n"
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

(define cg-string-set
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, string_set_body\n"
            "mov [string_set], rax\n"
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

	        "mov rax, const_1\n"

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
            "mov [vector_set], rax\n"
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

	        "mov rax, const_1\n"

	        "vector_set_finish:\n"
	        "leave\n"
	        "ret\n"
	        "vector_set_exit:\n" )))

(define cg-make-string
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, make_string_body\n"
            "mov [make_string], rax\n"
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
            "mov [make_vector], rax\n"
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

(define cg-vector
	(lambda()
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, custom_vector_body\n"
            "mov [custom_vector], rax\n"
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

(define cg-apply
 	(lambda()       
		(string-append
            "mov rdi, 16\n"
            "call malloc\n"
            "mov rbx, qword 0\n"
            "MAKE_LITERAL_CLOSURE rax, rbx, apply_body\n"
            "mov [apply], rax\n"
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
            "mov [symbol_to_string], rax\n"
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


(define newLine
  (list->string '(#\newline)))

(define tab
(list->string '(#\tab)))

(define cg-string->symbol
	(lambda ()
	  (string-append
	   tab "mov rdi, 16" newLine
	   tab "call malloc" newLine
	   tab "mov rbx, qword 0" newLine
	   tab "MAKE_LITERAL_CLOSURE rax, rbx, string_to_symbol_body" newLine
	   tab "mov qword [string_to_symbol], rax" newLine
	   tab "jmp string_to_symbol_exit" newLine
	   newLine
	    "string_to_symbol_body:" newLine
	   tab "push rbp" newLine
	   tab "mov rbp, rsp" newLine
	   tab "mov r11, An(0)" newLine ;r11= pointer to arg
	   
	   tab "mov r10, [symbol_table]" newLine		
	   tab "cmp r10, const_2" newLine
	   tab "je string_to_symbol_create_symbol" newLine
    		
	   "string_to_symbol_loop:" newLine
	   tab "mov r12, r10" newLine
	   tab "mov r12, [r12]" newLine
	   tab "DATA_UPPER r12" newLine
	   tab "add r12 , start_of_data" newLine
	   tab "mov r12, [r12]" newLine
	   tab "DATA r12" newLine
	   tab "add r12 , start_of_data" newLine
	   tab "STRING_COMPARE r12, r11" newLine
	   tab "cmp rax, const_3" newLine 
	   tab "je string_to_symbol_found" newLine
	   newLine
	   tab "mov r10, [r10]" newLine
	   tab "DATA_LOWER r10" newLine
	   tab "add r10, start_of_data" newLine
	   tab "cmp r10, const_2" newLine
	   tab "je string_to_symbol_create_symbol" newLine
	   newLine
	   tab "jmp string_to_symbol_loop" newLine
	   newLine         
	   "string_to_symbol_found:" newLine
	   tab "mov r10, [r10]" newLine
	   tab "DATA_UPPER r10" newLine
	   tab "add r10, start_of_data" newLine
	   tab "mov rax, r10" newLine
	   tab "jmp string_to_symbol_finish" newLine
	   newLine
	   tab "string_to_symbol_create_symbol:" newLine
	   tab "push r11" newLine
	   tab "mov rdi,8" newLine
	   tab "call malloc" newLine
	   tab "pop r11" newLine
	   tab "MAKE_MALLOC_LITERAL_SYMBOL rax , r11" newLine
	   tab "mov r11, rax" newLine
	   tab "mov r13, r11" newLine
	   tab "mov r14, [symbol_table]" newLine
	   newLine
	   tab "push r11" newLine
	   tab "push r14" newLine
	   tab "mov rdi, 8" newLine
	   tab "call malloc" newLine
	   tab "pop r14" newLine
	   tab "pop r11" newLine
	   tab "MAKE_MALLOC_LITERAL_PAIR rax, r11 ,r14" newLine
	   tab "mov [symbol_table],rax"  newLine
	   tab "mov rax, r13" newLine
	   newLine
	   "string_to_symbol_finish:" newLine
	   tab "leave" newLine
	   tab "ret" newLine
	   "string_to_symbol_exit:" newLine)))

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

(define file->list
	(lambda (input-file)
		(let ((in-port (open-input-file input-file)))
			(letrec ((run (lambda ()
							(let ((ch (read-char in-port)))
								(if (eof-object? ch)
									(begin (close-input-port in-port) '())
									(cons ch (run)))))))
				(run)))))

(define string->file
	(lambda (out-file str)
		(if (file-exists? out-file) (delete-file out-file))
		(let ((out-port (open-output-file out-file)))
			(letrec ((run
				(lambda (lst)
					(if (null? lst) (close-output-port out-port)
								(begin
									(write-char (car lst) out-port)
									(run (cdr lst)))))))
				(run (string->list str))))))


(define compile-scheme-file
	(lambda (source-file target-file)
		(let* ((parsed-exp-list 	(pipeline (append_build_in_funcs (file->list source-file))))
			   (epilogue 			"push qword [rax]\ncall write_sob_if_not_void\nadd rsp, 1*8\n"))
			   (create-c-table parsed-exp-list)

			   ;;(display (format "c-table ~a\n" c-table))


			   (create-global-var-table parsed-exp-list)

			   (string->file
					target-file
					(string-append
						"%include \"project/scheme.s\"\nsection .data\nstart_of_data:\n"
						(get-const-string)
						(get-fvar-string)
						(cg-symbol-table c-table 0 (count-symbols))
						"\nsection .text\nmain:\n"
						"push 0\n"
						"push 0\n"
						"push exit_compilation\n"
						"push rbp\n"
						"mov rbp, rsp\n"
						(code-cg-library-functions)
						(fold-left (lambda(acc pe) 
	                                    (string-append acc (code_gen pe) epilogue))
									""
								parsed-exp-list)
						"exit_compilation:\n"
						"leave\n"
						"mov rax, 0\n"
						"call exit\n"
						)))))



