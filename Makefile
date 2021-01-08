
top: test1

#OPT = -O1 # uncomment this for speedup: x280 -> x800 (at the cost of slower compiles)

CFLAGS = -Wall -Werror -Winline $(OPT) -I /usr/include/SDL2
LDFLAGS = -lSDL2

test1: invaders.exe
	./invaders.exe test1 > trace/test1.out
	git diff trace/test1.out

speed: invaders.exe
	./invaders.exe speed

play: invaders.exe
	./invaders.exe play

invaders.exe: c/program.o c/main.o
	gcc $^ -o $@ $(LDFLAGS)

c/program.o: c/program.c c/program.h c/shared.h Makefile
	gcc $(CFLAGS) -c $< -o $@

c/main.o: c/main.c c/shared.h Makefile
	gcc $(CFLAGS) -c $< -o $@

c/program.c: src/*.hs
	stack run c
