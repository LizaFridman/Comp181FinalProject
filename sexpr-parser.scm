(load "project/pc.scm")

(define <whitespace>
  (const (lambda (c)
	   (char<=? c #\space))))

(define <eol>
  (new
   (*parser (char #\newline))
   (*parser <end-of-input>)
   (*disj 2)
   done))

(define <linecomment>
  (new
   (*parser (char #\;))
   (*parser <any-char>)
   (*parser <eol>)
   *diff
   *star
   (*parser <eol>)
   (*caten 3)
   done))

(define <exprcomment>
  (new
   (*parser (word "#;"))
   (*delayed (lambda () <infixaddsub>))
   (*caten 2)
   
   (*parser (word "#;"))
   (*delayed (lambda () <sexpr>))
   (*caten 2)

   (*disj 2)
   done))

(define <comment>
  (new
   (*parser <linecomment>)
   (*parser <exprcomment>)
   (*disj 2)
   done))

(define <skip>
  (new
   (*parser <comment>)
   (*parser <whitespace>)
   (*disj 2)
   done))

(define <wrap-skip>
  (lambda (<parser>)
    (new (*parser (star <skip>))
	 (*parser <parser>)
	 (*parser (star <skip>))
	 (*caten 3)
	 (*pack-with (lambda (_ls p _rs) p))
	 done)))

(define <boolean>
  (new (*parser (word-ci "#t"))
       (*pack (lambda (_) #t))
       (*parser (word-ci "#f"))
       (*pack (lambda (_) #f))
       (*disj 2)
       done))

(define <namedchar>
  (new (*parser (word-ci "lambda"))
       (*pack (lambda (_) (integer->char 955)))
       (*parser (word-ci "newline"))
       (*pack (lambda (_) #\newline))
       (*parser (word-ci "nul"))
       (*pack (lambda (_) #\nul))
       (*parser (word-ci "page"))
       (*pack (lambda (_) #\page))
       (*parser (word-ci "return"))
       (*pack (lambda (_) #\return))
       (*parser (word-ci "space"))
       (*pack (lambda (_) #\space))
       (*parser (word-ci "tab"))
       (*pack (lambda (_) #\tab))
       (*disj 7)
       done))

(define <hexchar>
  (one-of-ci "0123456789abcdef"))

(define <hexunicodechar>
  (new (*parser (char #\x))
       (*parser <hexchar>)
       *plus
       (*pack (lambda (cs) (string->number (list->string cs) 16)))
       (*caten 2)
       (*pack-with (lambda (_x hex) (integer->char hex)))
       done))

(define <visiblesimplechar>
  (const (lambda (c) (char>? #\space))))

(define <char>
  (new (*parser (word "#\\"))
       (*parser <namedchar>)
       (*parser <hexunicodechar>)
       (*parser <visiblesimplechar>)
       (*disj 3)
       (*caten 2)
       (*pack-with
	(lambda (_ char)
	  char))
       done))

(define <natural>
  (pack (plus (range #\0 #\9))
	(lambda (ds) (string->number (list->string ds)))))

(define <integer>
  (new (*parser (one-of "+-"))
	*maybe
	(*pack-with (lambda (sign? sign)
		      (if sign?
			  (if (char=? sign #\+) 1 -1)
			  1)))
	(*parser <natural>)
	(*caten 2)
	(*pack-with
	 (lambda (sign nat)
	   (* sign nat)))
	done))

(define <fraction>
  (new (*parser <integer>)
       (*parser (char #\/))
       (*parser <natural>)
       (*caten 3)
       (*pack-with (lambda (int _ nat) (/ int nat)))
       done))

(define <number>
  (new (*parser <fraction>)
       (*parser <integer>)
       (*disj 2)

       (*delayed (lambda () <symbol>))
       *not-followed-by
       done))

(define <stringliteralchar>
  (new (*parser <any-char>)
       (*parser (one-of "\"\\"))
       *diff
       done))

(define <stringmetachar>
  (new (*parser (word "\\\\"))
       (*pack (lambda (_) #\\))
       (*parser (word "\\\""))
       (*pack (lambda (_) #\"))
       (*parser (word "\\t"))
       (*pack (lambda (_) #\tab))
       (*parser (word "\\f"))
       (*pack (lambda (_) #\page))
       (*parser (word "\\n"))
       (*pack (lambda (_) #\newline))
       (*parser (word "\\r"))
       (*pack (lambda (_) #\return))
       (*disj 6)
       done))

(define <stringhexchar>
  (new (*parser (word "\\x"))
       (*parser <hexchar>)
       *star
       (*parser (char #\;))
       (*caten 3)
       (*pack-with (lambda (_ hcs __)
		     (integer->char
		      (string->number
		       (list->string hcs) 16))))
       done))

(define <stringchar>
  (new (*parser <stringhexchar>)
       (*parser <stringmetachar>)
       (*parser <stringliteralchar>)
       (*disj 3)
       done))

(define <string>
  (new (*parser (char #\"))
       (*parser <stringchar>)
       *star
       (*parser (char #\"))
       (*caten 3)
       (*pack-with
	(lambda (_ scs __)
	  (list->string scs)))
       done))

(define <symbolchar>
  (one-of-ci "0123456789abcdefghijklmnopqrstuvwxyz!$^*-_=+<>?/"))

(define <symbol>
  (new (*parser <symbolchar>)
       *plus
       (*pack (lambda (scs) (string->symbol (string-downcase (list->string scs)))))
       done))

(define <properlist>
  (new
   (*parser (char #\())
   (*delayed (lambda () <sexpr>))
   *star
   (*parser (char #\)))
   (*caten 3)
   (*pack-with
    (lambda (_ exp __)
      exp))
   done))

(define <improperlist>
  (new
   (*parser (char #\())
   (*delayed (lambda () <sexpr>))
   *plus
   (*parser (char #\.))
   (*delayed (lambda () <sexpr>))
   (*parser (char #\)))
   (*caten 5)
   (*pack-with (lambda (_lb exps _dot exp _rb)
		`(,@exps . ,exp) ))
   done))

(define <vector>
  (new
   (*parser (char #\#))
   (*parser (char #\())
   (*delayed (lambda () <sexpr>))
   *star
   (*parser (char #\)))
   (*caten 4)
   (*pack-with (lambda (_ht _lb exps _rb)
		 (apply vector exps)))
  done))

(define <quoted>
  (new
   (*parser (char #\'))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with
    (lambda (_ expr)
       `',expr))
   done))

(define <quasiquote>
  (new
   (*parser (char #\`))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with
    (lambda (_ expr)
      (list 'quasiquote expr)))
   done))

(define <unquote>
   (new
   (*parser (char #\,))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with
    (lambda (_ expr)
      (list 'unquote expr)))
   done))

(define <unquoteandsplice>
     (new
      (*parser (char #\,))
      (*parser (char #\@))
      (*delayed (lambda () <sexpr>))
      (*caten 3)
      (*pack-with
       (lambda (_unq _at expr)
	 (list 'unquote-splicing expr)))
      done))

(define <CBnamesyntax1>
  (new
   (*parser (char #\@))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with
    (lambda (_ expr)
      `(cbname ,expr)))
   done))

(define <CBnamesyntax2>
  (new
   (*parser (char #\{))
   (*delayed (lambda () <sexpr>))
   (*parser (char #\}))
   (*caten 3)
   (*pack-with
    (lambda (_lb expr _rb)
      `(cbname ,expr)))
   done))

(define <CBname>
  (new
   (*parser <CBnamesyntax1>)
   (*parser <CBnamesyntax2>)
   (*disj 2)
   done))

(define <infixsymbol>
  (new (*parser (one-of-ci "0123456789abcdefghijklmnopqrstuvwxyz!$_=<>?"))
       *plus
       (*pack (lambda (scs) (string->symbol (string-downcase (list->string scs)))))
       done))

(define <infixnumber>
  (new (*parser <fraction>)
       (*parser <integer>)
       (*disj 2)
       (*pack
	(lambda (num)
	  num))
       done))

(define <infixexpr>
  (<wrap-skip>
  (new (*parser <infixnumber>)
       (*parser <infixsymbol>)
       (*parser (char #\())
       (*delayed (lambda () <infixaddsub>))
       (*parser (char #\)))
       (*caten 3)
       (*pack-with (lambda (_lp expr _rp) expr))
       (*delayed (lambda () <infix-sexpr-escape>))
       (*disj 4)
       
       done)))

(define <infixfuncallarrayget>
  (<wrap-skip>
  (new (*parser <infixexpr>)
       (*parser (char #\[))
       (*delayed (lambda () <infixaddsub>))
       (*parser (char #\]))
       (*caten 3)
       (*pack-with (lambda (_lb expr _rb) `(vecref ,expr)))
       (*parser (char #\())
       (*delayed (lambda () <infixaddsub>))
       (*parser (char #\,))
       (*delayed (lambda () <infixaddsub>))
       (*caten 2)
       (*pack-with (lambda (_comma expr) expr))
       *star
       (*caten 2)
       (*pack-with (lambda (first rest) `(funargs ,first ,@rest)))
       (*parser (char #\)))
       (*caten 3)
       (*pack-with (lambda (_lp exprs _rp) exprs))
       (*parser (char #\())
       (*parser <epsilon>)
       (*parser (char #\)))
       (*caten 3)
       (*pack-with (lambda (_lp eps _rp) '(funargs)))
       (*disj 3)
       *star
       (*caten 2)
       (*pack-with (lambda (id args) (letrec ((run (lambda (id args)
						     (cond ((null? args) id)
							   ((eq? (caar args) 'funargs) (run `(,id ,@(cdar args)) (cdr args)))
							   ((eq? (caar args) 'vecref) (run `(vector-ref ,id ,(cadar args)) (cdr args)))
							   (else 'wtf)))))
				       (run id args))))
       done)))
       
       

(define <infixpower>
  (new
   (*parser <infixfuncallarrayget>)
   (*parser (char #\^))
   (*parser (word "**"))
   (*disj 2)
   (*parser <infixfuncallarrayget>)
   (*caten 2)
   (*pack-with (lambda (_ exp) exp))
   *star
   (*caten 2)
   (*pack-with (lambda (first rest)
		 (let* ((all (cons first rest))
			(last (car (reverse all)))
			(abl (reverse (cdr (reverse all)))))
		   (fold-right (lambda (first rest) `(expt ,first ,rest)) last abl))))
   done))

(define <infixneg>
  (new (*parser (char #\-))
       *maybe
       (*pack-with
	(lambda (-? _)
	  -?))
       (*parser <infixpower>)
       (*caten 2)
       (*pack-with
	(lambda (-? pow)
	  (if -? `(- ,pow) pow)))
       done))

(define <infixmultdiv>
  (new (*parser <infixneg>)
       (*parser (one-of "*/"))
       (*pack (lambda (sign) (if (char=? sign #\*) '* '/)))
       (*parser <infixneg>)
       (*caten 2)
       *star
       (*caten 2)
       (*pack-with (lambda (neg negs)
		     (fold-left (lambda (first rest)
				  `(,(car rest) ,first ,(cadr rest))) neg negs)))
       done))

(define <infixaddsub>
  (new (*parser <infixmultdiv>)
       (*parser (one-of "+-"))
       (*pack (lambda (sign) (if (char=? sign #\+) '+ '-)))
       (*parser <infixmultdiv>)
       (*caten 2)
       *star
       (*caten 2)
       (*pack-with (lambda (md mds)
		     (fold-left (lambda (first rest)
				  `(,(car rest) ,first ,(cadr rest))) md mds)))
       done))

(define <infix-prefix-extension-prefix>
  (new
   (*parser (word "##"))
   (*parser (word "#%"))
   (*disj 2)
   done))

(define <infix-sexpr-escape>
  (new
   (*parser <infix-prefix-extension-prefix>)
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with (lambda (_ expr) expr))
   done))

(define <infix-extension>
  (new
   (*parser <infix-prefix-extension-prefix>)
   (*parser <infixaddsub>)
   (*caten 2)
   (*pack-with (lambda (_ expr) expr))
   done))

(define <sexpr>
  (<wrap-skip>
  (new (*parser <boolean>)
       (*parser <char>)
       (*parser <number>)
       (*parser <string>)
       (*parser <symbol>)
       
       (*parser <properlist>)
       (*parser <improperlist>)
       (*parser <vector>)
       (*parser <quoted>)
       (*parser <quasiquote>)
       
       (*parser <unquoteandsplice>)
       (*parser <unquote>)
       (*parser <CBname>)
       (*parser <infix-extension>)
       (*disj 14)
       done)))
