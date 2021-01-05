
top: run

run: invaders.exe
	./invaders.exe

invaders.exe: c/program.o c/machine.o
	gcc $^ -o $@

c/program.o: c/program.c c/machine.h Makefile
	gcc -Wall -Werror -c $< -o $@ # -Wno-unused-variable

c/machine.o: c/machine.c c/machine.h
	gcc -Wall -Werror -c $< -o $@

c/program.c: src/*.hs
	stack run c
