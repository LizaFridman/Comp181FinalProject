%include "scheme.s"

section .bss
global main

section .data
start_of_data:

section .text

main:
;(const 1)
	MOV RAX, sobInt1
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
;(const 2)
	MOV RAX, sobInt2
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
;(const 3)
	MOV RAX, sobInt3
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

L_exit:
	ret
