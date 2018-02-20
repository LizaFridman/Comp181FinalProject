section .bss
extern write_sob, write_sob_if_not_void, sobTrue, sobFalse, start_of_data
global main

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
%define ret_addr param(scmframe.ret_addr))
%define env param(scmframe.env)
%define arg_count param(scmframe.arg_count))
%define A0 param(scmframe.A0)
%define A1 param(scmframe.A1)
%define A2 param(scmframe.A2)
%define A3 param(scmframe.A3)
%define A4 param(scmframe.A4)
%define A5 param(scmframe.A5)
%define An(n) qword [rbp + 8*(n+4)]

section .text

main:
;(lambda-simple (a) (lambda-simple (b) (seq ((bvar a 0 0) (pvar b 0)))))

L_exit:
	PUSH RAX
	call write_sob
	POP RAX
