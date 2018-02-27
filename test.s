%include "scheme.s"

section .bss
global main

section .data
start_of_data:

L_global0:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global1:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global2:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global3:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global4:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global5:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global6:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global7:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global8:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global9:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global10:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global11:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global12:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global13:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global14:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global15:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global16:
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
	dq MAKE_LITERAL(T_INTEGER, 1234)
L_const8:
	MAKE_LITERAL_STRING 97
L_const11:
	MAKE_LITERAL_SYMBOL L_const8
L_const13:
	MAKE_LITERAL_STRING 98
L_const16:
	MAKE_LITERAL_SYMBOL L_const13
L_const18:
	MAKE_LITERAL_STRING 99
L_const21:
	MAKE_LITERAL_SYMBOL L_const18
L_const23:
	dq MAKE_LITERAL_PAIR(L_const21, L_const1)
L_const26:
	dq MAKE_LITERAL_PAIR(L_const16, L_const23)
L_const29:
	dq MAKE_LITERAL_PAIR(L_const11, L_const26)
L_const32:
	dq MAKE_LITERAL(T_INTEGER, 1)
L_const34:
	dq MAKE_LITERAL(T_INTEGER, 2)
L_const36:
	dq MAKE_LITERAL(T_CHAR, 'a')
L_const38:
	MAKE_LITERAL_STRING 97, 98, 99
L_const43:
	MAKE_LITERAL_STRING 108, 97, 109, 98, 100, 97
L_const51:
	MAKE_LITERAL_SYMBOL L_const43
L_const53:
	dq MAKE_LITERAL(T_INTEGER, 3)
L_const55:
	MAKE_LITERAL_VECTOR  32, 34, 53
L_const60:
	MAKE_LITERAL_VECTOR  11, 16, 21
L_const65:
	MAKE_LITERAL_VECTOR  11, 4, 1
L_const70:
	dq MAKE_LITERAL_PAIR(L_const11, L_const16)
L_const73:
	dq MAKE_LITERAL(T_INTEGER, 0)
L_const75:
	dq MAKE_LITERAL(T_INTEGER, 234)
L_const77:
	dq MAKE_LITERAL_FRACTION(L_const34, L_const53)
L_const80:
	dq MAKE_LITERAL_FRACTION(L_const32, L_const53)
L_const83:
	dq MAKE_LITERAL(T_INTEGER, 5)
L_const85:
	dq MAKE_LITERAL_FRACTION(L_const53, L_const83)
L_const88:
	dq MAKE_LITERAL(T_INTEGER, 7)
L_const90:
	dq MAKE_LITERAL_FRACTION(L_const83, L_const88)
L_const93:
	dq MAKE_LITERAL_FRACTION(L_const32, L_const34)

section .text

main:


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global0

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global0

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global0

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global0

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global1

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global2

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global3

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global5

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global7

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global8

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global9

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global10

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global1

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global11

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global11

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global10

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global10

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global5

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global13

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global13

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global14

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global14

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global15

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global16

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8


;; Code-Generation-Error
	PUSH rax

	PUSH 1


	MOV RAX, L_global15

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JNE L_error_applic

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
section .text

L_Applic_closure_error:
	MOV rax, L_error_applic
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

section .data

L_error_applic:
	MAKE_LITERAL_STRING 65, 112, 112, 108, 121, 32, 97, 114, 103, 117, 109, 101, 110, 116, 32, 110, 111, 116, 32, 97, 32, 99, 108, 117, 115, 117, 114, 101, 33, 10

section .text
L_exit:
	ret
