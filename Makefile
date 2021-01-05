
top: test1

test1: invaders.exe
	./invaders.exe test1 > trace/test1.out
	git diff trace/test1.out

speed: invaders.exe
	./invaders.exe speed

invaders.exe: c/program.o c/machine.o
	gcc $^ -o $@

c/program.o: c/program.c c/machine.h Makefile
	gcc -Wall -Werror -c $< -o $@

c/machine.o: c/machine.c c/machine.h
	gcc -Wall -Werror -c $< -o $@

c/program.c: src/*.hs
	stack run c
