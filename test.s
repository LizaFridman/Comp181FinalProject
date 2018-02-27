%include "scheme.s"

section .bss
global main

section .data
start_of_data:

L_global0:
	dq MAKE_LITERAL(T_UNDEFINED, 0)

L_const0:
	dq SOB_VOID
L_const1:
	dq SOB_NIL
L_const2:
	dq SOB_FALSE
L_const4:
	dq SOB_TRUE
L_const6:
	dq MAKE_LITERAL(T_INTEGER, 1337)

section .text

main:
	MOV RAX, L_const6

	MOV qword [L_global0], RAX
	MOV RAX, L_const0

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
L_exit:
	ret

L_Applic_closure_error:
	MOV rax, L_error_applic

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

	JMP L_exit

section .data

L_error_applic:
	MAKE_LITERAL_STRING 65, 112, 112, 108, 121, 32, 97, 114, 103, 117, 109, 101, 110, 116, 32, 110, 111, 116, 32, 97, 32, 99, 108, 117, 115, 117, 114, 101, 33, 10

section .text
