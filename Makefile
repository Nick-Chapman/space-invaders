
all: invaders-opt0.exe invaders-opt2.exe

speed: speedA0 speedB0 speedC0 speedA2 speedB2 speedC2
test: testB0
play: playC2


# check speed: mode(%)=A/B/C, opt=0/2
speed%0: invaders-opt0.exe
	./$< speed$*

speed%2: invaders-opt2.exe
	./$< speed$*


# run test & diff against expected output: mode(%)=A/B/C, opt=0
test%0: invaders-opt0.exe
	./$< test$* > trace/test1.out
	git diff trace/test1.out


# play: mode(%)=A/B/C, opt=2
play%2: invaders-opt2.exe
	./$< play$*


# compile/link: opt(%)=0/2
invaders-opt%.exe: c/main-opt%.o c/program-opt%.o
	gcc $^ -o $@ -lSDL2

CFLAGS = -Wall -Werror -Winline

c/program-opt%.o: c/program.c c/machine.h c/machine.c #Makefile
	gcc -O$* $(CFLAGS) -c $< -o $@ -Wno-unused-variable

c/main-opt%.o: c/main.c c/machine.h #Makefile
	gcc -O$* $(CFLAGS) -I /usr/include/SDL2 -c $< -o $@


# generate c code...
c/program.c: src/*.hs
	stack run c
