%include "scheme.s"

section .bss
global main

section .data
start_of_data:

sobVoid:
	dq SOB_VOID
sobNil:
	dq SOB_NIL
sobFalse:
	dq SOB_FALSE
sobTrue:
	dq SOB_TRUE
L_const6:
	dq MAKE_LITERAL(T_INTEGER, 1)
L_const8:
	MAKE_LITERAL_STRING 97
L_const11:
	MAKE_LITERAL_SYMBOL L_const8
L_const13:
	MAKE_LITERAL_STRING 72, 101, 108, 108, 111
section .text

main:

;(const 1)
.t_L_const6:
	MOV RAX, L_const6

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(const a)
.t_L_const11:
	MOV RAX, L_const11

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

;(const Hello)
.t_L_const13:
	MOV RAX, L_const13

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

L_exit:
	ret
