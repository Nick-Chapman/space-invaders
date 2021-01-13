
top: testB

#OPT = -O2 # uncomment this for speedup: x280 -> x800 (at the cost of slower compiles)

CFLAGS = -Wall -Werror -Winline $(OPT) -I /usr/include/SDL2
LDFLAGS = -lSDL2

testA: invaders.exe
	./invaders.exe testA > trace/test1.out
	git diff trace/test1.out

testB: invaders.exe
	./invaders.exe testB > trace/test1.out
	git diff trace/test1.out

testC: invaders.exe
	./invaders.exe testC > trace/test1.out
	git diff trace/test1.out

speed: speedA speedB speedC

speedA: invaders.exe
	./invaders.exe speedA

speedB: invaders.exe
	./invaders.exe speedB

speedC: invaders.exe
	./invaders.exe speedC

play0: invaders.exe
	./invaders.exe play0

play: invaders.exe
	./invaders.exe play

invaders.exe: c/program.o c/main.o
	gcc $^ -o $@ $(LDFLAGS)

c/program.o: c/program.c c/program.h c/shared.h Makefile
	gcc $(CFLAGS) -c $< -o $@ -Wno-unused-variable

c/main.o: c/main.c c/shared.h Makefile
	gcc $(CFLAGS) -c $< -o $@

c/program.c: src/*.hs
	stack run c
