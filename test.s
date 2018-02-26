%include "scheme.s"

section .bss
global main

section .data
start_of_data:

;; table initialization
L_const0:
	dq SOB_VOID
L_const1:
	dq SOB_NIL
L_const2:
	dq SOB_FALSE
L_const4:
	dq SOB_TRUE
L_const6:
	dq MAKE_LITERAL(T_INTEGER, 1)
L_const8:
	dq MAKE_LITERAL(T_INTEGER, 2)
L_const10:
	dq MAKE_LITERAL(T_INTEGER, 3)
L_const12:
	dq MAKE_LITERAL(T_CHAR, 'a')
L_const14:
	dq MAKE_LITERAL(T_CHAR, 'A')
L_const16:
	dq MAKE_LITERAL(T_CHAR, CHAR_NEWLINE)
L_const18:
	dq MAKE_LITERAL(T_CHAR, '"')
L_const20:
	dq MAKE_LITERAL(T_CHAR, '\')
L_const22:
	dq MAKE_LITERAL(T_INTEGER, 4)
L_const24:
	dq MAKE_LITERAL_FRACTION(L_const10, L_const22)
L_const27:
	dq MAKE_LITERAL(T_INTEGER, 5)
L_const29:
	dq MAKE_LITERAL_FRACTION(L_const22, L_const27)
L_const32:
	dq MAKE_LITERAL(T_INTEGER, 6)
L_const34:
	dq MAKE_LITERAL(T_INTEGER, 7)
L_const36:
	dq MAKE_LITERAL_FRACTION(L_const32, L_const34)
L_const39:
	dq MAKE_LITERAL_PAIR(L_const10, L_const1)
L_const42:
	dq MAKE_LITERAL_PAIR(L_const8, L_const39)
L_const45:
	dq MAKE_LITERAL_PAIR(L_const6, L_const42)
L_const48:
	dq MAKE_LITERAL_PAIR(L_const22, L_const1)
L_const51:
	dq MAKE_LITERAL_PAIR(L_const10, L_const48)
L_const54:
	dq MAKE_LITERAL_PAIR(L_const51, L_const1)
L_const57:
	dq MAKE_LITERAL_PAIR(L_const8, L_const1)
L_const60:
	dq MAKE_LITERAL_PAIR(L_const6, L_const57)
L_const63:
	dq MAKE_LITERAL_PAIR(L_const60, L_const54)
L_const66:
	dq MAKE_LITERAL(T_INTEGER, 1234)
section .text

main:


	MOV RAX, L_const6
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

	MOV RAX, L_const8
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

	MOV RAX, L_const10
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

	MOV RAX, L_const12
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

	MOV RAX, L_const14
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

	MOV RAX, L_const16
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

	MOV RAX, L_const18
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

	MOV RAX, L_const20
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

	MOV RAX, L_const24
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

	MOV RAX, L_const29
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

	MOV RAX, L_const36
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

	MOV RAX, L_const45
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

	MOV RAX, L_const45
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

	MOV RAX, L_const63
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

	MOV RAX, L_const66
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

	MOV RAX, L_const12
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
L_exit:
	ret
