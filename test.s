%include "scheme.s"

;;; Parameter Getters

%define param(offset) qword [rbp + offset]

struc scmframe
.old_rbp: resq 1
.ret_addr: resq 1
.env: resq 1
.arg_count: resq 1
.A0: resq 1
.A1: resq 1
.A2: resq 1
.A3: resq 1
.A4: resq 1
.A5: resq 1
endstruc

%define old_rbp param(scmframe.old_rbp)
%define ret_addr param(scmframe.ret_addr)
%define env param(scmframe.env)
%define arg_count param(scmframe.arg_count)
%define A0 param(scmframe.A0)
%define A1 param(scmframe.A1)
%define A2 param(scmframe.A2)
%define A3 param(scmframe.A3)
%define A4 param(scmframe.A4)
%define A5 param(scmframe.A5)
%define An(n) qword [rbp + 8*(n+4)]


section .bss
global main
extern malloc

section .data
start_of_data:
L_error_applic:
	MAKE_LITERAL_STRING "Error:", CHAR_SPACE, "Applic", CHAR_SPACE, "on", CHAR_SPACE, "non", CHAR_SPACE, "procedure"

L_global0:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global1:
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
	dq MAKE_LITERAL(T_INTEGER, 0)
L_const8:
	dq MAKE_LITERAL(T_INTEGER, 1337)

section .text

main:
	mov rbx, 0
	mov rax, 0
	cmp rax, 0
	je end_of_copy_envs5

	mov rdi, 8
	call malloc
.after_malloc1:
	mov rbx, rax

	XOR rax, rax
	mov rax, arg_count
	mov rdi, 8
	mul rdi
	PUSH rbx
	mov rdi, rax
	call malloc
.after_malloc2:
	POP rbx
	mov rcx, rax

	mov rdi, 0
for_copy_args2:
	cmp rdi, arg_count
	je end_of_copy_args3

	mov rax, 8
	mul rdi
	mov rdx, An(rdi)
	mov qword [rcx+rax], rdx

	inc rdi
	 jmp for_copy_args2

end_of_copy_args3:
	mov qword [rbx], rcx
	mov r14, env
	cmp r14, 0
	je end_of_copy_envs5
	mov rdi, 0

for_copy_envs4:
	cmp rdi, 0
	je end_of_copy_envs5

	mov rax, 8
	mul rdi
	mov rcx, qword [r14+rax]
	mov qword [rbx+rax+8], rcx
	inc rdi
	jmp for_copy_envs4

end_of_copy_envs5:
	PUSH rbx
	PUSH rcx
	mov rdi, 16
	call malloc
.after_malloc3:
	pop rcx
	pop rbx

	push rdx
	mov rdx, code6
	MAKE_LITERAL_CLOSURE rax, rbx, rdx 
.after_make_closure:
	pop rdx
	jmp skip_code1

code6:
	push rbp
	mov rbp, rsp

		MOV RAX, qword [rbp + 4*8]

	MOV RBX, L_const2
	CMP RAX, RBX
	JE L_ifDif9
	MOV RAX, L_const2

	JMP L_ifEnd8
L_ifDif9:
	MOV RAX, L_const4

L_ifEnd8:
	mov rbx, rax
	mov rax, arg_count
	add rax, 1
	mov rdi, 8
	mul rdi
	add rsp, rax
	mov rax, rbx

	leave
	ret

skip_code1:

	MOV qword [L_global0], RAX
	MOV RAX, L_const0

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
	MOV RAX, L_const6
	PUSH rax

	XOR rbx, rbx
	MOV rbx, 1
	PUSH rbx

	MOV RAX, L_global0

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JE .not2_check_passed
	MOV rax, L_error_applic

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
	JMP L_exit

.not2_check_passed:
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
	MOV RAX, L_const2
	PUSH rax

	XOR rbx, rbx
	MOV rbx, 1
	PUSH rbx

	MOV RAX, L_global0

	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JE .not1_check_passed
	MOV rax, L_error_applic

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
	JMP L_exit

.not1_check_passed:
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
	MOV RAX, L_const8

	MOV qword [L_global1], RAX
	MOV RAX, L_const0

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
L_exit:
	ret
