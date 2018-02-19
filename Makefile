loadVar:= (load "Compiler.scm")

%: scheme.o
	echo '$(loadVar) (compile-scheme-file "$(MAKECMDGOALS).scm" "$(MAKECMDGOALS).s")' | scheme -q
	nasm -f elf64 $(MAKECMDGOALS).s -o $(MAKECMDGOALS).o
	gcc -m64 -Wall -o $(MAKECMDGOALS) $(MAKECMDGOALS).o scheme.o

scheme.o:
	nasm -f elf64 scheme.s -o scheme.o

