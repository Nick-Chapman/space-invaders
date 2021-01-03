
top: run

run: invaders.exe
	./invaders.exe

invaders.exe: c/program.o c/machine.o
	gcc $^ -o $@

c/program.o: c/program.c
	gcc -Wall -Werror -c $< -o $@

c/machine.o: c/machine.c
	gcc -Wall -Werror -c $< -o $@

c/program.c: src/*.hs
	stack run c
