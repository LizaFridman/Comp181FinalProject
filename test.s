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
L_const6:
	dq MAKE_LITERAL(T_INTEGER, 6)
L_const8:
	MAKE_LITERAL_STRING 104, 101, 108, 108, 111
L_const15:
	MAKE_LITERAL_SYMBOL L_const8
L_const17:
	dq MAKE_LITERAL(T_INTEGER, 1)
section .text

main:

;(or ((const #f) (or ((const #f) (const #f))) (const 6)))
;(const #f)
	MOV RAX, L_const2

	CMP RAX, L_const2
	JNE L_orEnd1
;(or ((const #f) (const #f)))
;(const #f)
	MOV RAX, L_const2

	CMP RAX, L_const2
	JNE L_orEnd2
;(const #f)
	MOV RAX, L_const2

L_orEnd2:

	CMP RAX, L_const2
	JNE L_orEnd1
;(const 6)
	MOV RAX, L_const6

L_orEnd1:
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
;(or ((const hello) (or ((const #f) (const 1)))))
;(const hello)
	MOV RAX, L_const15

	CMP RAX, L_const2
	JNE L_orEnd3
;(or ((const #f) (const 1)))
;(const #f)
	MOV RAX, L_const2

	CMP RAX, L_const2
	JNE L_orEnd4
;(const 1)
	MOV RAX, L_const17

L_orEnd4:

L_orEnd3:
	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
L_exit:
	ret
