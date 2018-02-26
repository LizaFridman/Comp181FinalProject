loadVar:= (load "project/Compiler.scm") (load "project/sexpr-parser.scm")

loadVarInternal:= (load "Compiler.scm")

%:
	echo '$(loadVarInternal) (compile-scheme-file "$(MAKECMDGOALS).scm" "$(MAKECMDGOALS).s")' | scheme -q
	nasm -f elf64 $(MAKECMDGOALS).s -o $(MAKECMDGOALS).o
	gcc -m64 -Wall -o $(MAKECMDGOALS) $(MAKECMDGOALS).o

