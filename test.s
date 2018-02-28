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
	dq MAKE_LITERAL(T_INTEGER, 1)

section .text

main:
	mov rbx, 0
	mov rax, 1
	cmp rax, 0
	je end_of_copy_envs32
	mov rdi, 16
	call malloc
	mov rbx, rax
	mov rax, arg_count
	mov rdi, 8
	mul rdi
	push rbx
	mov rdi, rax
	call malloc
	pop rbx
	mov rcx, rax
	mov rdi, 0
for_copy_args29:
	cmp rdi, arg_count
	je end_of_copy_args30
	mov rax, 8
	mul rdi
	mov rdx, An(rdi)
	mov qword [rcx+rax], rdx
	inc rdi
	 jmp for_copy_args29
	end_of_copy_args30:
mov qword [rbx], rcx
mov r14, env
cmp r14, 0
je end_of_copy_envs32
mov rdi, 0
for_copy_envs31:
cmp rdi, 1
je end_of_copy_envs32
mov rax, 8
mul rdi
mov rcx, qword [r14+rax]
mov qword [rbx+rax+8], rcx
inc rdi
jmp for_copy_envs31
end_of_copy_envs32:
push rbx
push rcx
mov rdi, 16
call malloc
pop rcx
pop rbx
push rdx
mov rdx, code33
MAKE_LITERAL_CLOSURE rax, rbx, rdx 
pop rdx
jmp skip_code28
code33:
push rbp
mov rbp, rsp
	MOV RAX, qword [rbp + 4*8]

	MOV RBX, L_const2
	CMP RAX, RBX
	JE L_ifDif36
	MOV RAX, L_const2

	JMP L_ifEnd35
L_ifDif36:
	MOV RAX, L_const4

L_ifEnd35:
mov rbx, rax
mov rax, arg_count
add rax, 1
mov rdi, 8
mul rdi
add rsp, rax
mov rax, rbx
leave
ret
skip_code28:

	MOV qword [L_global0], RAX
	MOV RAX, L_const0

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

	MOV qword [L_global1], RAX
	MOV RAX, L_const0

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
	mov rbx, 0
	mov rax, 1
	cmp rax, 0
	je end_of_copy_envs14
	mov rdi, 16
	call malloc
	mov rbx, rax
	mov rax, arg_count
	mov rdi, 8
	mul rdi
	push rbx
	mov rdi, rax
	call malloc
	pop rbx
	mov rcx, rax
	mov rdi, 0
for_copy_args11:
	cmp rdi, arg_count
	je end_of_copy_args12
	mov rax, 8
	mul rdi
	mov rdx, An(rdi)
	mov qword [rcx+rax], rdx
	inc rdi
	 jmp for_copy_args11
	end_of_copy_args12:
mov qword [rbx], rcx
mov r14, env
cmp r14, 0
je end_of_copy_envs14
mov rdi, 0
for_copy_envs13:
cmp rdi, 1
je end_of_copy_envs14
mov rax, 8
mul rdi
mov rcx, qword [r14+rax]
mov qword [rbx+rax+8], rcx
inc rdi
jmp for_copy_envs13
end_of_copy_envs14:
push rbx
push rcx
mov rdi, 16
call malloc
pop rcx
pop rbx
push rdx
mov rdx, code15
MAKE_LITERAL_CLOSURE rax, rbx, rdx 
pop rdx
jmp skip_code10
code15:
push rbp
mov rbp, rsp
;;Code-Generation-Error
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

	MOV RBX, L_const2
	CMP RAX, RBX
	JE L_ifDif18
	MOV RAX, L_const6

	JMP L_ifEnd17
L_ifDif18:
;;Code-Generation-Error
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

L_ifEnd17:
mov rbx, rax
mov rax, arg_count
add rax, 1
mov rdi, 8
mul rdi
add rsp, rax
mov rax, rbx
leave
ret
skip_code10:

	MOV qword [L_global6], RAX
	MOV RAX, L_const0

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
	mov rbx, 0
	mov rax, 1
	cmp rax, 0
	je end_of_copy_envs23
	mov rdi, 16
	call malloc
	mov rbx, rax
	mov rax, arg_count
	mov rdi, 8
	mul rdi
	push rbx
	mov rdi, rax
	call malloc
	pop rbx
	mov rcx, rax
	mov rdi, 0
for_copy_args20:
	cmp rdi, arg_count
	je end_of_copy_args21
	mov rax, 8
	mul rdi
	mov rdx, An(rdi)
	mov qword [rcx+rax], rdx
	inc rdi
	 jmp for_copy_args20
	end_of_copy_args21:
mov qword [rbx], rcx
mov r14, env
cmp r14, 0
je end_of_copy_envs23
mov rdi, 0
for_copy_envs22:
cmp rdi, 1
je end_of_copy_envs23
mov rax, 8
mul rdi
mov rcx, qword [r14+rax]
mov qword [rbx+rax+8], rcx
inc rdi
jmp for_copy_envs22
end_of_copy_envs23:
push rbx
push rcx
mov rdi, 16
call malloc
pop rcx
pop rbx
push rdx
mov rdx, code24
MAKE_LITERAL_CLOSURE rax, rbx, rdx 
pop rdx
jmp skip_code19
code24:
push rbp
mov rbp, rsp
;;Code-Generation-Error
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

	MOV RBX, L_const2
	CMP RAX, RBX
	JE L_ifDif27
	MOV RAX, qword [rbp + 5*8]

	JMP L_ifEnd26
L_ifDif27:
;;Code-Generation-Error
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

L_ifEnd26:
mov rbx, rax
mov rax, arg_count
add rax, 1
mov rdi, 8
mul rdi
add rsp, rax
mov rax, rbx
leave
ret
skip_code19:

	MOV qword [L_global7], RAX
	MOV RAX, L_const0

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8

	MOV qword [L_global10], RAX
	MOV RAX, L_const0

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
	mov rbx, 0
	mov rax, 1
	cmp rax, 0
	je end_of_copy_envs5
	mov rdi, 16
	call malloc
	mov rbx, rax
	mov rax, arg_count
	mov rdi, 8
	mul rdi
	push rbx
	mov rdi, rax
	call malloc
	pop rbx
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
cmp rdi, 1
je end_of_copy_envs5
mov rax, 8
mul rdi
mov rcx, qword [r14+rax]
mov qword [rbx+rax+8], rcx
inc rdi
jmp for_copy_envs4
end_of_copy_envs5:
push rbx
push rcx
mov rdi, 16
call malloc
pop rcx
pop rbx
push rdx
mov rdx, code6
MAKE_LITERAL_CLOSURE rax, rbx, rdx 
pop rdx
jmp skip_code1
code6:
push rbp
mov rbp, rsp
;;Code-Generation-Error
	PUSH rax

	PUSH 1

	MOV RAX, L_global12

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

	MOV RBX, L_const2
	CMP RAX, RBX
	JE L_ifDif9
;;Code-Generation-Error
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

	JMP L_ifEnd8
L_ifDif9:
	MOV RAX, L_const2

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

	MOV qword [L_global11], RAX
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
