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
L_global17:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global18:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global19:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global20:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global21:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global22:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global23:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global24:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global25:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global26:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global27:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global28:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global29:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global30:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global31:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global32:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global33:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global34:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global35:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global36:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global37:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global38:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global39:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global40:
	dq MAKE_LITERAL(T_UNDEFINED, 0)
L_global41:
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
L_const10:
	dq MAKE_LITERAL(T_INTEGER, 4)
L_const12:
	dq MAKE_LITERAL(T_INTEGER, 5)
SymbolTable: 
dq 1

section .text

main:
mov rax, L_const1
mov [SymbolTable], eax 
mov [SymbolTable], rax 

L_global0_create_closure:
mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, L_global0_body
mov [L_global0], rax
jmp L_global0_exit
L_global0_body:
	push rbp
	mov rbp, rsp
	PUSHA
	MOV rax, [rbp + 8 + 1*8]
L_global0_predicate:
	CMP rax, T_NIL
	JE L_global0_match
	MOV rax, [L_const2]
	JMP L_global0_end

L_global0_match:
	MOV rax, [L_const4]
L_global0_end:
	POPA
	leave
	ret
L_global0_exit:
	NOP

L_global1_create_closure:
mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, L_global1_body
mov [L_global1], rax
jmp L_global1_exit
L_global1_body:
	push rbp
	mov rbp, rsp
	PUSHA
	MOV rax, [rbp + 8 + 1*8]
L_global1_predicate:
	CMP rax, T_BOOL
	JE L_global1_match
	MOV rax, [L_const2]
	JMP L_global1_end

L_global1_match:
	MOV rax, [L_const4]
L_global1_end:
	POPA
	leave
	ret
L_global1_exit:
	NOP

L_global2_create_closure:
mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, L_global2_body
mov [L_global2], rax
jmp L_global2_exit
L_global2_body:
	push rbp
	mov rbp, rsp
	PUSHA
	MOV rax, [rbp + 8 + 1*8]
L_global2_predicate:
	CMP rax, T_CHAR
	JE L_global2_match
	MOV rax, [L_const2]
	JMP L_global2_end

L_global2_match:
	MOV rax, [L_const4]
L_global2_end:
	POPA
	leave
	ret
L_global2_exit:
	NOP

L_global3_create_closure:
mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, L_global3_body
mov [L_global3], rax
jmp L_global3_exit
L_global3_body:
	push rbp
	mov rbp, rsp
	PUSHA
	MOV rax, [rbp + 8 + 1*8]
L_global3_predicate:
	CMP rax, T_INTEGER
	JE L_global3_match
	MOV rax, [L_const2]
	JMP L_global3_end

L_global3_match:
	MOV rax, [L_const4]
L_global3_end:
	POPA
	leave
	ret
L_global3_exit:
	NOP

L_global4_create_closure:
mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, L_global4_body
mov [L_global4], rax
jmp L_global4_exit
L_global4_body:
	push rbp
	mov rbp, rsp
	PUSHA
	MOV rax, [rbp + 8 + 1*8]
L_global4_predicate:
	CMP rax, T_INTEGER
	JE L_global4_match
	CMP rax, T_FRACTION
	JE L_global4_match
	MOV rax, [L_const2]
	JMP L_global4_end

L_global4_match:
	MOV rax, [L_const4]
L_global4_end:
	POPA
	leave
	ret
L_global4_exit:
	NOP

L_global5_create_closure:
mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, L_global5_body
mov [L_global5], rax
jmp L_global5_exit
L_global5_body:
	push rbp
	mov rbp, rsp
	PUSHA
	MOV rax, [rbp + 8 + 1*8]
L_global5_predicate:
	CMP rax, T_INTEGER
	JE L_global5_match
	CMP rax, T_FRACTION
	JE L_global5_match
	MOV rax, [L_const2]
	JMP L_global5_end

L_global5_match:
	MOV rax, [L_const4]
L_global5_end:
	POPA
	leave
	ret
L_global5_exit:
	NOP

L_global6_create_closure:
mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, L_global6_body
mov [L_global6], rax
jmp L_global6_exit
L_global6_body:
	push rbp
	mov rbp, rsp
	PUSHA
	MOV rax, [rbp + 8 + 1*8]
L_global6_predicate:
	CMP rax, T_PAIR
	JE L_global6_match
	MOV rax, [L_const2]
	JMP L_global6_end

L_global6_match:
	MOV rax, [L_const4]
L_global6_end:
	POPA
	leave
	ret
L_global6_exit:
	NOP

L_global7_create_closure:
mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, L_global7_body
mov [L_global7], rax
jmp L_global7_exit
L_global7_body:
	push rbp
	mov rbp, rsp
	PUSHA
	MOV rax, [rbp + 8 + 1*8]
L_global7_predicate:
	CMP rax, T_STRING
	JE L_global7_match
	MOV rax, [L_const2]
	JMP L_global7_end

L_global7_match:
	MOV rax, [L_const4]
L_global7_end:
	POPA
	leave
	ret
L_global7_exit:
	NOP

L_global8_create_closure:
mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, L_global8_body
mov [L_global8], rax
jmp L_global8_exit
L_global8_body:
	push rbp
	mov rbp, rsp
	PUSHA
	MOV rax, [rbp + 8 + 1*8]
L_global8_predicate:
	CMP rax, T_SYMBOL
	JE L_global8_match
	MOV rax, [L_const2]
	JMP L_global8_end

L_global8_match:
	MOV rax, [L_const4]
L_global8_end:
	POPA
	leave
	ret
L_global8_exit:
	NOP

L_global9_create_closure:
mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, L_global9_body
mov [L_global9], rax
jmp L_global9_exit
L_global9_body:
	push rbp
	mov rbp, rsp
	PUSHA
	MOV rax, [rbp + 8 + 1*8]
L_global9_predicate:
	CMP rax, T_VECTOR
	JE L_global9_match
	MOV rax, [L_const2]
	JMP L_global9_end

L_global9_match:
	MOV rax, [L_const4]
L_global9_end:
	POPA
	leave
	ret
L_global9_exit:
	NOP

L_global10_create_closure:
mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, L_global10_body
mov [L_global10], rax
jmp L_global10_exit
L_global10_body:
	push rbp
	mov rbp, rsp
	PUSHA
	MOV rax, [rbp + 8 + 1*8]
L_global10_predicate:
	CMP rax, T_CLOSURE
	JE L_global10_match
	MOV rax, [L_const2]
	JMP L_global10_end

L_global10_match:
	MOV rax, [L_const4]
L_global10_end:
	POPA
	leave
	ret
L_global10_exit:
	NOP

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, cons_body
mov [L_global21], rax
jmp cons_exit
cons_body:
push rbp
mov rbp, rsp
mov rbx, arg_count
cmp rbx, 2
jne cons_finish
mov rdi, 8
call malloc
mov rcx, An(0)
mov rdx, An(1)
MAKE_MALLOC_LITERAL_PAIR rax, rcx, rdx
cons_finish:
leave
ret
cons_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, car_body
mov [L_global2], rax
jmp car_exit
car_body:
push rbp
mov rbp, rsp
mov rax, An(0)
mov rax, [rax]
DATA_UPPER rax
add rax, start_of_data
leave
ret
car_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, cdr_body
mov [L_global19], rax
jmp cdr_exit
cdr_body:
push rbp
mov rbp, rsp
mov rax, An(0)
mov rax, [rax]
DATA_LOWER rax
add rax, start_of_data
leave
ret
cdr_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, string_to_symbol_body
mov qword [L_global32], rax
jmp string_to_symbol_exit
string_to_symbol_body:
push rbp
mov rbp, rsp
mov r11, An(0)
mov r10, [SymbolTable]
cmp r10, L_const1
je string_to_symbol_create_symbol
string_to_symbol_loop:
mov r12, r10
mov r12, [r12]
DATA_UPPER r12
add r12 , start_of_data
mov r12, [r12]
DATA r12
add r12 , start_of_data
STRING_COMPARE r12, r11
cmp rax, L_const4
je string_to_symbol_found
mov r10, [r10]
DATA_LOWER r10
add r10, start_of_data
cmp r10, L_const1
je string_to_symbol_create_symbol
jmp string_to_symbol_loop
string_to_symbol_found:
mov r10, [r10]
DATA_UPPER r10
add r10, start_of_data
mov rax, r10
jmp string_to_symbol_finish
string_to_symbol_create_symbol:
push r11
mov rdi,8
call malloc
pop r11
MAKE_MALLOC_LITERAL_SYMBOL rax , r11
mov r11, rax
mov r13, r11
mov r14, [SymbolTable]
push r11
push r14
mov rdi, 8
call malloc
pop r14
pop r11
MAKE_MALLOC_LITERAL_PAIR rax, r11 ,r14
mov [SymbolTable],rax
mov rax, r13
string_to_symbol_finish:
leave
ret
string_to_symbol_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, integer_to_char_body
mov [L_global24], rax
jmp integer_to_char_exit
integer_to_char_body:
push rbp
mov rbp, rsp
mov rbx, arg_count
cmp rbx, 1
jne integer_to_char_finish
mov rax, An(0)
mov rax, [rax]
mov rbx, rax
TYPE rax
cmp rax, T_INTEGER
jne integer_to_char_finish
sub rbx, T_INTEGER
or rbx, T_CHAR
mov rdi,8
call malloc
mov qword [rax], rbx
integer_to_char_finish:
leave
ret
integer_to_char_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, char_to_integer_body
mov [L_global20], rax
jmp char_to_integer_exit
char_to_integer_body:
push rbp
mov rbp, rsp
mov rbx, arg_count
cmp rbx, 1
jne char_to_integer_finish
mov rax, An(0)
mov rax, [rax]
mov rbx, rax
TYPE rax
cmp rax, T_CHAR
jne char_to_integer_finish
sub rbx, T_CHAR
or rbx, T_INTEGER
mov rdi,8
call malloc
mov qword [rax], rbx
char_to_integer_finish:
leave
ret
char_to_integer_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, numerator_body
mov [L_global27], rax
jmp numerator_exit
numerator_body:
push rbp
mov rbp, rsp
mov rbx, arg_count
cmp rbx, 1
jne numerator_finish
mov rax, An(0)
mov rax, [rax]
TYPE rax
cmp rax, T_INTEGER
je get_integer_numerator
cmp rax, T_FRACTION
jne numerator_finish
mov rax, An(0)
mov rax, [rax]
DATA_UPPER rax
add rax, start_of_data
jmp numerator_finish
get_integer_numerator:
mov rax, An(0)
numerator_finish:
leave
ret
numerator_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, eq_body
mov [L_global13], rax
jmp eq_exit
eq_body:
push rbp
mov rbp, rsp
mov rbx, arg_count
cmp rbx, 2
jne eq_finish
mov rax, An(0)
mov rax, [rax]
mov rbx, An(1)
mov rbx, [rbx]
cmp rax, rbx
je eq_true
mov rax, L_const2
jmp eq_finish
eq_true:
mov rax, L_const4
eq_finish:
leave
ret
eq_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, make_string_body
mov [L_global25], rax
jmp make_string_exit
make_string_body:
push rbp
mov rbp, rsp
mov rdx, qword 0
mov r9, arg_count
cmp r9, 2
jg make_string_finish
mov rax, An(0)
mov rax, [rax]
mov rbx, rax
DATA rbx
TYPE rax
cmp rax, T_INTEGER
jne make_string_finish
cmp r9, 1
je start_creating_string
mov rcx, An(1)
mov rcx, [rcx]
mov rdx, rcx
DATA rdx
TYPE rcx
cmp rcx, T_CHAR
jne make_string_finish
start_creating_string:
push rbx
push rdx
mov rdi, rbx
call malloc
pop rdx
pop rbx
mov r10, 0
for_create_string:
cmp r10, rbx
je end_of_create_string
mov byte [rax+r10], dl
inc r10
jmp for_create_string
end_of_create_string:
mov rcx, rax
MAKE_LITERAL_STRING_WITH_REGS rcx, rbx
mov rcx, rax
push rcx
mov rdi, 8
call malloc
pop rcx
mov [rax], rcx
make_string_finish:
leave
ret
make_string_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, make_vector_body
mov [L_global26], rax
jmp make_vector_exit
make_vector_body:
push rbp
mov rbp, rsp
mov rdi, 8
call malloc
mov rdx, 0
shl rdx, TYPE_BITS
add rdx, T_INTEGER
mov rbx, arg_count
mov [rax], rdx
mov rdx, rax
cmp rbx, 2
jg make_vector_finish
mov rax, An(0)
mov rax, [rax]
mov rbx, rax
DATA rbx
TYPE rax
cmp rax, T_INTEGER
jne make_vector_finish
mov r9, arg_count
cmp r9, 1
je start_creating_vector
mov rdx, An(1)
start_creating_vector:
push rbx
push rdx
shl rbx, 3
mov rdi, rbx
call malloc
pop rdx
pop rbx
mov r10, 0
for_create_vector:
cmp r10, rbx
je end_of_create_vector
mov qword [rax+r10*8], rdx
inc r10
jmp for_create_vector
end_of_create_vector:
mov rcx, rax
shl rbx, 3
MAKE_LITERAL_VECTOR_WITH_REGS rcx, rbx
mov rcx, rax
push rcx
mov rdi, 8
call malloc
pop rcx
mov [rax], rcx
make_vector_finish:
leave
ret
make_vector_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, remainder_body
mov [L_global28], rax
jmp remainder_exit
remainder_body:
push rbp
mov rbp, rsp
mov rbx, arg_count
cmp rbx, 2
jne remainder_finish
mov rax, An(0)
mov rax, [rax]
mov rcx, rax
mov rbx, An(1)
mov rbx, [rbx]
mov r10, rbx
TYPE rcx
cmp rcx, T_INTEGER
jne remainder_finish
TYPE rbx
cmp rbx, T_INTEGER
jne remainder_finish
DATA rax
mov r9, rax
DATA r10
mov rdx, qword 0
cmp r9, 0
jge is_not_negative1
neg rax
is_not_negative1:
mov rdx, qword 0
idiv r10
cmp r9, 0
jge is_not_negative2
neg rdx
is_not_negative2:
shl rdx, TYPE_BITS
add rdx, T_INTEGER
push rdx
mov rdi, 8
call malloc
pop rdx
mov [rax], rdx
remainder_finish:
leave
ret
remainder_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, string_length_body
mov [L_global29], rax
jmp string_length_exit
string_length_body:
push rbp
mov rbp, rsp
mov rbx, arg_count
cmp rbx, 1
jne string_length_finish
mov rax, An(0)
mov rax, [rax]
mov rbx,rax
TYPE rax
cmp rax, T_STRING
jne string_length_finish
STRING_LENGTH rbx
shl rbx, TYPE_BITS
add rbx, T_INTEGER
mov rdi,8
call malloc
mov [rax], rbx
string_length_finish:
leave
ret
string_length_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, string_ref_body
mov [L_global30], rax
jmp string_ref_exit
string_ref_body:
push rbp
mov rbp, rsp
mov rbx, arg_count
cmp rbx, 2
jne string_ref_finish
mov rax, An(0)
mov rax, [rax]
mov rbx, rax
mov rcx, An(1)
mov rcx, [rcx]
mov rdx, rcx
DATA rdx
TYPE rax
cmp rax, T_STRING
jne string_ref_finish
TYPE rcx
cmp rcx, T_INTEGER
jne string_ref_finish
STRING_REF cl, rbx, rdx
shl rcx, TYPE_BITS
add rcx, T_CHAR
push rcx
mov rdi,8
call malloc
pop rcx
mov [rax], rcx
string_ref_finish:
leave
ret
string_ref_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, string_set_body
mov [L_global31], rax
jmp string_set_exit
string_set_body:
push rbp
mov rbp, rsp
mov rbx, arg_count
cmp rbx, 3
jne string_set_finish
mov rax, An(0)
mov rax, [rax]
mov rbx, rax
mov r11, An(1)
mov r11, [r11]
mov rdx, r11
DATA rdx
mov r10, An(2)
mov r10, [r10]
mov rcx, r10
DATA rcx
TYPE rax
cmp rax, T_STRING
jne string_set_finish
TYPE r11
cmp r11, T_INTEGER
jne string_set_finish
TYPE r10
cmp r10, T_CHAR
jne string_set_finish
mov r12, rbx
STRING_ELEMENTS rbx
add rbx, rdx
mov byte [rbx], cl
mov rax, L_const0
string_set_finish:
leave
ret
string_set_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, bin_less_than_body
mov [L_global12], rax
jmp bin_less_than_exit
bin_less_than_body:
push rbp
mov rbp, rsp
mov rbx, arg_count
cmp rbx, 2
jne bin_less_than_finish
push An(1)
push An(0)
push 2
push qword 0
call bin_minus_body
mov rax, [rax]
mov rbx, rax
TYPE rbx
cmp rbx, T_INTEGER
je check_sign
CAR rax
check_sign:
DATA rax
cmp rax, 0
jl bin_less_than_true
mov rax, L_const2
jmp bin_less_than_finish
bin_less_than_true:
mov rax, L_const4
bin_less_than_finish:
leave
ret
bin_less_than_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, eq?_body
mov [L_global23], rax
jmp eq?_exit
eq?_body:
push rbp
mov rbp, rsp
mov rbx, arg_count
cmp rbx, 2
jne eq?_finish
mov rax, An(0)
mov rax, [rax]
mov rbx, An(1)
mov rbx, [rbx]
cmp rax, rbx
je eq?_true
mov rax, L_const2
jmp eq?_finish
eq?_true:
mov rax, L_const4
eq?_finish:
leave
ret
eq?_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, bin_plus_body
mov [L_global14], rax
jmp bin_plus_exit
bin_plus_body:
push rbp
mov rbp, rsp
mov rcx ,arg_count
mov rax ,An(0)
cmp rcx, 2
jne bin_plus_finish
mov rax ,qword [rax]
mov rcx, rax
TYPE rcx
cmp rcx, T_INTEGER
je bin_plus_arg1_int_check_arg2
bin_plus_arg1_frac_check_arg2:
mov rbx, An(1)
mov rbx, qword [rbx]
mov rcx, rbx
TYPE rcx
cmp rcx, T_INTEGER
jne bin_plus_arg1_frac_arg2_frac
mov r10, rax
mov rax, rbx
mov rbx, r10
jmp bin_plus_arg1_int_arg2_frac
bin_plus_arg1_frac_arg2_frac:
mov rcx, rax
CAR rcx
DATA rcx
mov r11, rax
CDR r11
DATA r11
mov r10, rbx
CAR r10
DATA r10
mov r12, rbx
CDR r12
DATA r12
mov r8, r11
MULT r11, r12
MULT rcx, r12
MULT r10, r8
add rcx, r10
mov rax, rcx
mov r12, r11
jmp bin_plus_simplify_and_create_fraction
bin_plus_arg1_int_check_arg2:
mov rbx, An(1)
mov rbx, qword [rbx]
mov rdx, rbx
TYPE rdx
cmp rdx, T_INTEGER
je bin_plus_arg1_int_arg2_int
bin_plus_arg1_int_arg2_frac:
mov r10, rbx
CAR r10
DATA r10
mov r12, rbx
CDR r12
DATA r12
mov r8,rax
DATA r8
MULT r8,r12
mov rax,r8
add rax,r10
bin_plus_simplify_and_create_fraction:
mov r8,  rax
mov r9, r12
push r9
push r8
call simplify_fraction
add rsp, 16
jmp bin_plus_create_new_fraction
bin_plus_arg1_int_arg2_int:
DATA rbx
DATA rax
add rbx,rax
shl rbx, 4
or rbx, T_INTEGER
push rbx
mov rdi, 8
call malloc
pop rbx
mov [rax],rbx
jmp bin_plus_finish
bin_plus_create_new_fraction:
cmp r9, 1
je bin_plus_create_new_int
shl r8, 4
or r8, T_INTEGER
shl r9, 4
or r9, T_INTEGER
mov rdi, 8
call malloc
mov rbx, rax
mov [rbx], r8
push rbx
mov rdi, 8
call malloc
pop rbx
mov rcx, rax
mov [rcx], r9
push rbx
push rcx
mov rdi, 8
call malloc
pop rcx
pop rbx
MAKE_LITERAL_FRACTION_WITH_REGS rax, rbx, rcx
jmp bin_plus_finish
bin_plus_create_new_int:
shl r8, 4
or r8, T_INTEGER
push r8
mov rdi, 8
call malloc
pop r8
mov [rax], r8
bin_plus_finish:
leave
ret
bin_plus_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, bin_div_body
mov [L_global15], rax
jmp bin_div_exit
bin_div_body:
push rbp
mov rbp, rsp
mov rcx ,arg_count
mov rax ,An(0)
cmp rcx, 2
jne bin_div_finish
mov rax ,qword [rax]
mov rcx, rax
TYPE rcx
cmp rcx, T_INTEGER
je bin_div_arg1_int_check_arg2
bin_div_arg1_frac_check_arg2:
mov rbx, An(1)
mov rbx, qword [rbx]
mov rcx, rbx
TYPE rcx
cmp rcx, T_INTEGER
je bin_div_arg1_frac_arg2_int
bin_div_arg1_frac_arg2_frac:
mov rcx, rax
CAR rcx
DATA rcx
mov r11, rax
CDR r11
DATA r11
mov r10, rbx
CAR r10
DATA r10
mov r12, rbx
CDR r12
DATA r12
mov r8, r11
MULT rcx, r12
MULT r11, r10
cmp r11, 0
jg bin_div_positive_int1
neg r11
neg rcx
bin_div_positive_int1:
mov rax, rcx
mov r12, r11
jmp bin_div_simplify_and_create_fraction
bin_div_arg1_int_check_arg2:
mov rbx, An(1)
mov rbx, qword [rbx]
mov rdx, rbx
TYPE rdx
cmp rdx, T_INTEGER
je bin_div_arg1_int_arg2_int
bin_div_arg1_int_arg2_frac:
mov r10, rbx
CAR r10
DATA r10
mov r12, rbx
CDR r12
DATA r12
mov r8,rax
DATA r8
MULT r8,r12
cmp r10, 0
jg bin_div_positive_int2
neg r10
neg r8
bin_div_positive_int2:
mov r12, r10
mov rax,r8
jmp bin_div_simplify_and_create_fraction
bin_div_arg1_frac_arg2_int:
mov r10, rax
CAR r10
DATA r10
mov r12, rax
CDR r12
DATA r12
mov r8,rbx
DATA r8
MULT r8,r12
cmp r8, 0
jg bin_div_positive_int3
neg r8
neg r10
bin_div_positive_int3:
mov rax,r8
mov r11, rax
mov rax, r10
mov r12, r11
bin_div_simplify_and_create_fraction:
mov r8,  rax
mov r9, r12
push r9
push r8
call simplify_fraction
add rsp, 16
jmp bin_div_create_new_fraction
bin_div_arg1_int_arg2_int:
DATA rax
DATA rbx
mov r12, rbx
cmp r12, 0
jg bin_div_positive_int4
neg r12
neg rax
bin_div_positive_int4:
jmp bin_div_simplify_and_create_fraction
bin_div_create_new_fraction:
cmp r9, 1
je bin_div_create_new_int
shl r8, 4
or r8, T_INTEGER
shl r9, 4
or r9, T_INTEGER
mov rdi, 8
call malloc
mov rbx, rax
mov [rbx], r8
push rbx
mov rdi, 8
call malloc
pop rbx
mov rcx, rax
mov [rcx], r9
push rbx
push rcx
mov rdi, 8
call malloc
pop rcx
pop rbx
MAKE_LITERAL_FRACTION_WITH_REGS rax, rbx, rcx
jmp bin_div_finish
bin_div_create_new_int:
shl r8, 4
or r8, T_INTEGER
push r8
mov rdi, 8
call malloc
pop r8
mov [rax], r8
bin_div_finish:
leave
ret
bin_div_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, bin_mul_body
mov [L_global16], rax
jmp bin_mul_exit
bin_mul_body:
push rbp
mov rbp, rsp
mov rcx ,arg_count
mov rax ,An(0)
cmp rcx, 2
jne bin_mul_finish
mov rax ,qword [rax]
mov rcx, rax
TYPE rcx
cmp rcx, T_INTEGER
je bin_mul_arg1_int_check_arg2
bin_mul_arg1_frac_check_arg2:
mov rbx, An(1)
mov rbx, qword [rbx]
mov rcx, rbx
TYPE rcx
cmp rcx, T_INTEGER
jne bin_mul_arg1_frac_arg2_frac
mov r10, rax
mov rax, rbx
mov rbx, r10
jmp bin_mul_arg1_int_arg2_frac
bin_mul_arg1_frac_arg2_frac:
mov rcx, rax
CAR rcx
DATA rcx
mov r11, rax
CDR r11
DATA r11
mov r10, rbx
CAR r10
DATA r10
mov r12, rbx
CDR r12
DATA r12
mov r8, r11
MULT r11, r12
MULT r10, rcx
mov rax, r10
mov r12, r11
jmp bin_mul_simplify_and_create_fraction
bin_mul_arg1_int_check_arg2:
mov rbx, An(1)
mov rbx, qword [rbx]
mov rdx, rbx
TYPE rdx
cmp rdx, T_INTEGER
je bin_mul_arg1_int_arg2_int
bin_mul_arg1_int_arg2_frac:
mov r10, rbx
CAR r10
DATA r10
mov r12, rbx
CDR r12
DATA r12
mov r8,rax
DATA r8
MULT r8,r10
mov rax,r8
bin_mul_simplify_and_create_fraction:
mov r8,  rax
mov r9, r12
push r9
push r8
call simplify_fraction
add rsp, 16
jmp bin_mul_create_new_fraction
bin_mul_arg1_int_arg2_int:
DATA rbx
DATA rax
mov r10, rax
MULT rbx,r10
shl rbx, 4
or rbx, T_INTEGER
push rbx
mov rdi, 8
call malloc
pop rbx
mov [rax],rbx
jmp bin_mul_finish
bin_mul_create_new_fraction:
cmp r9, 1
je bin_mul_create_new_int
shl r8, 4
or r8, T_INTEGER
shl r9, 4
or r9, T_INTEGER
mov rdi, 8
call malloc
mov rbx, rax
mov [rbx], r8
push rbx
mov rdi, 8
call malloc
pop rbx
mov rcx, rax
mov [rcx], r9
push rbx
push rcx
mov rdi, 8
call malloc
pop rcx
pop rbx
MAKE_LITERAL_FRACTION_WITH_REGS rax, rbx, rcx
jmp bin_mul_finish
bin_mul_create_new_int:
shl r8, 4
or r8, T_INTEGER
push r8
mov rdi, 8
call malloc
pop r8
mov [rax], r8
bin_mul_finish:
leave
ret
bin_mul_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, bin_minus_body
mov [L_global17], rax
jmp bin_minus_exit
bin_minus_body:
push rbp
mov rbp, rsp
mov rcx ,arg_count
mov rax ,An(0)
cmp rcx, 2
jne bin_minus_finish
mov rax ,qword [rax]
mov rcx, rax
TYPE rcx
cmp rcx, T_INTEGER
je bin_minus_arg1_int_check_arg2
bin_minus_arg1_frac_check_arg2:
mov rbx, An(1)
mov rbx, qword [rbx]
mov rcx, rbx
TYPE rcx
cmp rcx, T_INTEGER
je bin_minus_arg1_frac_arg2_int
bin_minus_arg1_frac_arg2_frac:
mov rcx, rax
CAR rcx
DATA rcx
mov r11, rax
CDR r11
DATA r11
mov r10, rbx
CAR r10
DATA r10
mov r12, rbx
CDR r12
DATA r12
mov r8, r11
MULT r11, r12
MULT rcx, r12
MULT r10, r8
sub rcx, r10
mov rax, rcx
mov r12, r11
jmp bin_minus_simplify_and_create_fraction
bin_minus_arg1_int_check_arg2:
mov rbx, An(1)
mov rbx, qword [rbx]
mov rdx, rbx
TYPE rdx
cmp rdx, T_INTEGER
je bin_minus_arg1_int_arg2_int
bin_minus_arg1_int_arg2_frac:
mov r10, rbx
CAR r10
DATA r10
mov r12, rbx
CDR r12
DATA r12
mov r8,rax
DATA r8
MULT r8,r12
mov rax,r8
sub rax,r10
jmp bin_minus_simplify_and_create_fraction
bin_minus_arg1_frac_arg2_int:
mov r10, rax
CAR r10
DATA r10
mov r12, rax
CDR r12
DATA r12
mov r8,rbx
DATA r8
MULT r8,r12
mov rax,r8
sub r10, rax
mov rax, r10
bin_minus_simplify_and_create_fraction:
mov r8,  rax
mov r9, r12
push r9
push r8
call simplify_fraction
add rsp, 16
jmp bin_minus_create_new_fraction
bin_minus_arg1_int_arg2_int:
DATA rbx
DATA rax
sub rax, rbx
mov r10, rax
shl r10, 4
or r10, T_INTEGER
push r10
mov rdi, 8
call malloc
pop r10
mov [rax],r10
jmp bin_minus_finish
bin_minus_create_new_fraction:
cmp r9, 1
je bin_minus_create_new_int
shl r8, 4
or r8, T_INTEGER
shl r9, 4
or r9, T_INTEGER
mov rdi, 8
call malloc
mov rbx, rax
mov [rbx], r8
push rbx
mov rdi, 8
call malloc
pop rbx
mov rcx, rax
mov [rcx], r9
push rbx
push rcx
mov rdi, 8
call malloc
pop rcx
pop rbx
MAKE_LITERAL_FRACTION_WITH_REGS rax, rbx, rcx
jmp bin_minus_finish
bin_minus_create_new_int:
shl r8, 4
or r8, T_INTEGER
push r8
mov rdi, 8
call malloc
pop r8
mov [rax], r8
bin_minus_finish:
leave
ret
bin_minus_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, apply_body
mov [L_global11], rax
jmp apply_exit
apply_body:
push rbp
mov rbp, rsp
mov rax, An(0)
mov rax, qword [rax]
mov r10, qword [rbp]
mov r11,qword [rbp+8]
mov r12, rbp
add r12, 5*8
mov rbx, rax
TYPE rbx
cmp rbx, T_CLOSURE
jne apply_finish
mov rcx, An(1)
mov rcx, qword [rcx]
mov rbx, rcx
TYPE rbx
cmp rbx, T_PAIR
je apply_start
cmp rbx, T_NIL
jne apply_finish
apply_start:
mov rsi, 0
apply_calculate_list_length:
cmp rbx, T_NIL
je apply_calculate_list_length_done
CDR rcx
mov rbx, rcx
TYPE rbx
inc rsi
jmp apply_calculate_list_length
apply_calculate_list_length_done:
shl rsi, 3
sub r12, rsi
shr rsi, 3
mov rdi, 0
mov rcx, An(1)
mov rcx, qword [rcx]
apply_loop:
cmp rdi, rsi
je apply_loop_exit
mov rbx, rcx
DATA_UPPER rbx
add rbx, start_of_data
mov qword [r12 + 8*rdi], rbx
CDR rcx
inc rdi
jmp apply_loop
apply_loop_exit:
sub r12, 8
mov qword [r12],rsi
sub r12, 8
mov rbx, rax
CLOSURE_ENV rbx
mov qword [r12], rbx
sub r12, 8
mov qword [r12], r11
mov rsp, r12
mov rbp, r10
mov rbx, rax
TYPE rbx
cmp rbx, T_CLOSURE
jne apply_finish
CLOSURE_CODE rax
jmp rax
apply_finish:
leave
ret
apply_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, symbol_to_string_body
mov [L_global33], rax
jmp symbol_to_string_exit
symbol_to_string_body:
push rbp
mov rbp, rsp
mov rax, An(0)
mov rax, [rax]
DATA rax
add rax , start_of_data
symbol_to_string_finish:
leave
ret
symbol_to_string_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, custom_vector_body
mov [L_global9], rax
jmp custom_vector_exit
custom_vector_body:
push rbp
mov rbp, rsp
mov rbx, arg_count
push rbx
shl rbx, 3
mov rdi, rbx
call malloc
pop rbx
mov r10, 0
for_vector:
cmp r10, rbx
je end_of_vector
mov rdx, An(r10)
mov qword [rax+r10*8], rdx
inc r10
jmp for_vector
end_of_vector:
mov rcx, rax
shl rbx, 3
MAKE_LITERAL_VECTOR_WITH_REGS rcx, rbx
mov rcx, rax
push rcx
mov rdi, 8
call malloc
pop rcx
mov [rax], rcx
custom_vector_finish:
leave
ret
custom_vector_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, vector_length_body
mov [L_global35], rax
jmp vector_length_exit
vector_length_body:
push rbp
mov rbp, rsp
mov rbx, arg_count
cmp rbx, 1
jne vector_length_finish
mov rax, An(0)
mov rax, [rax]
mov rbx,rax
TYPE rax
cmp rax, T_VECTOR
jne vector_length_finish
VECTOR_LENGTH rbx
shl rbx, TYPE_BITS
add rbx, T_INTEGER
mov rdi,8
call malloc
mov [rax], rbx
vector_length_finish:
leave
ret
vector_length_exit:

mov rdi, 16
call malloc
mov rbx, qword 0
MAKE_LITERAL_CLOSURE rax, rbx, vector_set_body
mov [L_global37], rax
jmp vector_set_exit
vector_set_body:
push rbp
mov rbp, rsp
mov rbx, arg_count
cmp rbx, 3
jne vector_set_finish
mov rax, An(0)
mov rax, [rax]
mov rbx, rax
mov r11, An(1)
mov r11, [r11]
mov rdx, r11
DATA rdx
mov rcx, An(2)
TYPE rax
cmp rax, T_VECTOR
jne vector_set_finish
TYPE r11
cmp r11, T_INTEGER
jne vector_set_finish
mov r12, rbx
VECTOR_ELEMENTS r12
mov [r12 + rdx*8], rcx
mov rax, L_const0
vector_set_finish:
leave
ret
vector_set_exit:
;; cg-define
;; cg-lambda-simple
	mov rbx, 0
	mov rax, 0
	cmp rax, 0
	je end_of_copy_envs30

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
for_copy_args27:
	cmp rdi, arg_count
	je end_of_copy_args28

	mov rax, 8
	mul rdi
	mov rdx, An(rdi)
	mov qword [rcx+rax], rdx

	inc rdi
	 jmp for_copy_args27

end_of_copy_args28:
	mov qword [rbx], rcx
	mov r14, env
	cmp r14, 0
	je end_of_copy_envs30
	mov rdi, 0

for_copy_envs29:
	cmp rdi, 0
	je end_of_copy_envs30

	mov rax, 8
	mul rdi
	mov rcx, qword [r14+rax]
	mov qword [rbx+rax+8], rcx
	inc rdi
	jmp for_copy_envs29

end_of_copy_envs30:
	PUSH rbx
	PUSH rcx
	mov rdi, 16
	call malloc
.after_malloc3:
	pop rcx
	pop rbx

	push rdx
	mov rdx, code31
	MAKE_LITERAL_CLOSURE rax, rbx, rdx 
.after_make_closure:
	pop rdx
	jmp skip_code26

code31:
	push rbp
	mov rbp, rsp

		MOV RAX, qword [rbp + 4*8]

	MOV RBX, L_const2
	CMP RAX, RBX
	JE L_ifDif34
	MOV RAX, L_const2

	JMP L_ifEnd33
L_ifDif34:
	MOV RAX, L_const4

L_ifEnd33:
	mov rbx, rax
	mov rax, arg_count
	add rax, 1
	mov rdi, 8
	mul rdi
	add rsp, rax
	mov rax, rbx

	leave
	ret

skip_code26:

	MOV qword [L_global38], RAX
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
je loop_copy_envs_end39
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

loop_copy_args_36:
inc rax
cmp rax, arg_count 
sub rax, 1 
cmp rdi, arg_count
je loop_copy_args_end37
mov rax, 8
mul rdi
mov rdx, An(rdi)
mov qword [rcx+rax], rdx
inc rdi
jmp loop_copy_args_36
loop_copy_args_end37:
mov qword [rbx], rcx
mov r14, env
cmp r14, 0
jle loop_copy_envs_end39
mov rdi, 0

loop_copy_envs38:
cmp rdi, 0
je loop_copy_envs_end39
mov rax, 8
mul rdi
cmp rdi, 999999 
je loop_copy_envs38
mov rcx, qword [r14+rax]
mov qword [rbx+rax+8], rcx
inc rdi
jmp loop_copy_envs38

loop_copy_envs_end39:
push rbx
push rcx
mov rdi, 16
call malloc
pop rcx
pop rbx
MAKE_LITERAL_CLOSURE rax, rbx, code40
jmp code_end35

code40:
push rbp
mov rbp, rsp
mov rbx, L_const1
mov r10, arg_count

loop_fix_stack42:
cmp r10, 0
je loop_fix_stack_end43
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
jmp loop_fix_stack42

loop_fix_stack_end43:
cmp rbx, L_const1
mov qword [rbp+4*8+0*8], rbx
	MOV RAX, qword [rbp + 4*8]
leave
ret
code_end35:

	MOV qword [L_global39], RAX
	MOV RAX, L_const0

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
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

	;; cg-applic
	MOV RAX, qword [rbp + 6*8]
	PUSH rax

	PUSH 1

	MOV RAX, [L_global0]

	MOV rax, [rax]
	MOV rbx, rax
	TYPE rbx

	CMP rbx, T_CLOSURE
	JNE .end_applic6

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
.end_applic6:
	ADD rsp, 24

	MOV RBX, L_const2
	CMP RAX, RBX
	JE L_ifDif9
	MOV RAX, qword [rbp + 5*8]

	JMP L_ifEnd8
L_ifDif9:
;; cg-applic
;; cg-applic
	MOV RAX, qword [rbp + 6*8]
	PUSH rax

	PUSH 1

	MOV RAX, [L_global19]

	MOV rax, [rax]
	MOV rbx, rax
	TYPE rbx

	CMP rbx, T_CLOSURE
	JNE .end_applic3

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
.end_applic3:
	ADD rsp, 24
	PUSH rax
;; cg-applic
;; cg-applic
	MOV RAX, qword [rbp + 6*8]
	PUSH rax

	PUSH 1

	MOV RAX, [L_global18]

	MOV rax, [rax]
	MOV rbx, rax
	TYPE rbx

	CMP rbx, T_CLOSURE
	JNE .end_applic5

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
.end_applic5:
	ADD rsp, 24
	PUSH rax
	MOV RAX, qword [rbp + 5*8]
	PUSH rax

	PUSH 2

	MOV RAX, qword [rbp + 4*8]

	MOV rax, [rax]
	MOV rbx, rax
	TYPE rbx

	CMP rbx, T_CLOSURE
	JNE .end_applic4

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
.end_applic4:
	ADD rsp, 32
	PUSH rax
	MOV RAX, qword [rbp + 4*8]
	PUSH rax

	PUSH 3

	MOV RAX, [L_global40]

	MOV rax, [rax]
	MOV rbx, rax
	TYPE rbx

	CMP rbx, T_CLOSURE
	JNE .end_applic2

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
.end_applic2:
	ADD rsp, 40

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

	MOV qword [L_global40], RAX
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
;; cg-applic
	MOV RAX, qword [rbp + 4*8]
	PUSH rax
	MOV RAX, L_const6
	PUSH rax
;; cg-lambda-simple
	mov rbx, 0
	mov rax, 1
	cmp rax, 0
	je end_of_copy_envs23

	mov rdi, 16
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
	PUSH rbx
	PUSH rcx
	mov rdi, 16
	call malloc
.after_malloc3:
	pop rcx
	pop rbx

	push rdx
	mov rdx, code24
	MAKE_LITERAL_CLOSURE rax, rbx, rdx 
.after_make_closure:
	pop rdx
	jmp skip_code19

code24:
	push rbp
	mov rbp, rsp

	;; cg-applic
	MOV RAX, qword [rbp + 5*8]
	PUSH rax
	MOV RAX, qword [rbp + 4*8]
	PUSH rax

	PUSH 2

	MOV RAX, [L_global14]

	MOV rax, [rax]
	MOV rbx, rax
	TYPE rbx

	CMP rbx, T_CLOSURE
	JNE .end_applic8

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
.end_applic8:
	ADD rsp, 32
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
	PUSH rax

	PUSH 3

	MOV RAX, [L_global40]

	MOV rax, [rax]
	MOV rbx, rax
	TYPE rbx

	CMP rbx, T_CLOSURE
	JNE .end_applic7

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
.end_applic7:
	ADD rsp, 40
leave
ret
code_end10:

	MOV qword [L_global41], RAX
	MOV RAX, L_const0

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
;; cg-applic
	MOV RAX, L_const12
	PUSH rax
	MOV RAX, L_const10
	PUSH rax
	MOV RAX, L_const8
	PUSH rax

	PUSH 3

	MOV RAX, [L_global34]

	MOV rax, [rax]
	MOV rbx, rax
	TYPE rbx

	CMP rbx, T_CLOSURE
	JNE .end_applic1

	MOV rbx, rax
	CLOSURE_ENV rbx
	PUSH rbx
	CLOSURE_CODE rax
	call rax
.end_applic1:
	ADD rsp, 40

	PUSH qword [RAX]
	call write_sob_if_not_void
	ADD rsp, 1*8
L_exit:
	MOV rax, 60
	MOV rdi, 0
	syscall
