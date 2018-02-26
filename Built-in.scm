
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
    (procedure? closure-pred-label)))

(define built-in-funcs
  (map first built-in-map))

(define built-in?
  (lambda (fun)
    (member func built-in-funcs)))

(define cg-built-in
  (lambda ()
    (string-append
     cg-null?
     cg-bool?
     cg-char?
     cg-integer?
     cg-number?
     cg-rational?
     cg-pair?
     cg-string?
     cg-symbol?
     cg-vector?
     cg-closure?)))

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
		      (create-built-in-closure (first row)
					       (second row)
					       (second (assoc (first row) built-in-map))))))))

(define create-built-in-closure
  (lambda (var value func-label)
    (string-append
     "Create Closure for " var)))

(define self-implemented
  '((define append
      (lambda (lst1 lst2)
	(cond ((null? lst1)
	       lst2)
	      (else (cons (car lst1)
			  (append (cdr lst1) lst2))))))
    
    (define list
      (lambda items items))
    
    (define zero?
      (lambda (element)
	(and (number? element)
	     (equal? 0 x))))))

