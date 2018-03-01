(define not
  (lambda (element)
    (if element
	#f
	#t)))

;;(define +
;; (letrec ((loop
;;(lambda (s)
;; (if (null? s)
;;   0
;;  (bin+ (car s)
;;       (loop (cdr s)))))))
;;(lambda s (loop s))))

;;(define length
 ;; (lambda (lst)
  ;;  (if (null? lst)
	;;0
	;;(+ 1 (length (cdr lst))))))

;;(define append
 ;; (lambda (lst1 lst2)
  ;;  (cond ((null? lst1)
	 ;;  lst2)
	  ;;(else (cons (car lst1)
		 ;;     (append (cdr lst1) lst2))))))
(define list
  (lambda args args))

(define fold_left
  (lambda (proc init lst)
    (if (null? lst) 
	init 
	(fold_left proc (proc init (car lst)) (cdr lst)))))

(define +
  (lambda x
    (fold_left (lambda (acc y)
		 (b+ acc y))
	       0
	       x)))

;;(define zero?
  ;;(lambda (element)
    ;;(and (number? element)
;;	 (equal? 0 x)))))
