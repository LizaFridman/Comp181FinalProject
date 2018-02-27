%include "scheme.s"

section .bss
global main

section .data
start_of_data:

L_const0:
	dq SOB_VOID
L_const1:
	dq SOB_NIL
L_const2:
	dq SOB_FALSE
L_const4:
	dq SOB_TRUE
section .text

main:


;(applic (fvar boolean?) ((const #t)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar boolean?) ((const #f)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar boolean?) ((const 1234)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar boolean?) ((const a)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar symbol?) ((const b)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar procedure?) ((fvar procedure?)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar eq?) ((applic (fvar car) ((const (a b c)))) (const a)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar =) ((applic (fvar car) ((applic (fvar cons) ((const 1) (const 2))))) (const 1)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar integer?) ((const 1234)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar char?) ((const a)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar null?) ((const ())))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar string?) ((const abc)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar symbol?) ((const lambda)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar vector?) ((const #(1 2 3))))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar vector?) ((const 1234)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar string?) ((const #(a b c))))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar string?) ((const 1234)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar =) ((const 3) (applic (fvar vector-length) ((const #(a #t ()))))))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar pair?) ((const (a . b))))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar pair?) ((const ())))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar zero?) ((const 0)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar zero?) ((const 234)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar +) ((const 2) (const 2/3)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar *) ((const 1/3) (const 3/5) (const 5/7)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(applic (fvar +) ((const 1/2) (const 1/3)))	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
L_exit:
	ret
