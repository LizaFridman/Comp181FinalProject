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
L_global2:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global3:
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
	MAKE_LITERAL_STRING 97
L_const9:
	MAKE_LITERAL_SYMBOL L_const6
L_const11:
	MAKE_LITERAL_STRING 98
L_const14:
	MAKE_LITERAL_SYMBOL L_const11
SymbolTable: 
dq 1

section .text

main:

	MOV rdi, 16
	call malloc
	MOV rbx, rax
	PUSH rbx

	MOV rdi, 8
	call malloc
	POP rbx
	MOV rdx, L_cons
	MAKE_LITERAL_CLOSURE rax, rbx, rdx
	MOV rbx, L_global3
	MOV [rbx], rax

;; cg-define
;; cg-lambda-simple
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
;; cg-define
;; cg-lambda-opt
mov rbx, 0
mov rax, 0
cmp rbx, 0
cmp rax, 0
je loop_copy_envs_end14
	push rax 

mov rdi, 8
call malloc
mov rbx, rax
	pop rax 
mov rax, arg_count
mov rdi, 8
mul rdi
push rbx
mov rdi, rax
call malloc
pop rbx
mov rcx, rax
mov rdi, 0

loop_copy_args_11:
inc rax
cmp rax, arg_count 
sub rax, 1 
cmp rdi, arg_count
je loop_copy_args_end12
mov rax, 8
mul rdi
mov rdx, An(rdi)
mov qword [rcx+rax], rdx
inc rdi
jmp loop_copy_args_11
loop_copy_args_end12:
mov qword [rbx], rcx
mov r14, env
cmp r14, 0
jle loop_copy_envs_end14
mov rdi, 0

loop_copy_envs13:
cmp rdi, 0
je loop_copy_envs_end14
mov rax, 8
mul rdi
cmp rdi, 999999 
je loop_copy_envs13
mov rcx, qword [r14+rax]
mov qword [rbx+rax+8], rcx
inc rdi
jmp loop_copy_envs13

loop_copy_envs_end14:
push rbx
push rcx
mov rdi, 16
call malloc
pop rcx
pop rbx
MAKE_LITERAL_CLOSURE rax, rbx, code15
jmp code_end10

code15:
push rbp
mov rbp, rsp
mov rbx, L_const1
mov r10, arg_count

loop_fix_stack17:
cmp r10, 0
je loop_fix_stack_end18
mov rdi, 8
call malloc
mov rdx, rbp
add rdx, 4*8
mov r11, r10
dec r11
shl r11, 3
add rdx, r11
mov rdx, qword [rdx]
inc rax
sub rax, 1
MAKE_MALLOC_LITERAL_PAIR rax, rdx, rbx
mov rbx, rax
dec r10
jmp loop_fix_stack17

loop_fix_stack_end18:
cmp rbx, L_const1
mov qword [rbp+4*8+0*8], rbx
	MOV RAX, qword [rbp + 4*8]
leave
ret
code_end10:

	MOV qword [L_global1], RAX
	MOV RAX, L_const0

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
;; cg-define
;; cg-applic
	MOV RAX, L_const14
	PUSH rax
	MOV RAX, L_const9
	PUSH rax

	XOR rbx, rbx
	MOV rbx, 2
	PUSH rbx

	MOV RAX, L_global3

	PUSH rax
	MOV RAX, qword [RAX]
	MOV RAX, qword [RAX]
	TYPE rax
	CMP rax, T_CLOSURE
	JE .closure_1_check_passed
	POP rax
	POP rbx
	MOV rax, L_error_applic

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
	JMP L_exit

.closure_1_check_passed:
	POP rax
	MOV rax, [rax]
	MOV rax, [rax]
	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
	POP rbx
	POP rbx
	POP rbx
	POP rbx

	MOV qword [L_global2], RAX
	MOV RAX, L_const0

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
	MOV RAX, L_global2

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
L_exit:
	MOV rax, 60
	MOV rdi, 0
	syscall
