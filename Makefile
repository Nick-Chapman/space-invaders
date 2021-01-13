
top: test

all: invaders-opt0.exe invaders-opt2.exe

speed: speedA0 speedA1 speedA2 speedB0 speedB1 speedB2 speedC0 speedC1 speedC2
test: testB0
play: playC0

.PRECIOUS: invaders-opt%.exe invaders-opt%-TRACE.exe

# check speed: mode(%)=A/B/C, opt=0/2
speed%0: invaders-opt0.exe
	@ ./$< speed$*

speed%1: invaders-opt1.exe
	@ ./$< speed$*

speed%2: invaders-opt2.exe
	@ ./$< speed$*


# run test & diff against expected output: mode(%)=A/B/C, opt=0, needs TRACE
test%0: invaders-opt0-TRACE.exe
	./$< test$* > trace/test1.out
	git diff trace/test1.out


# play: mode(%)=A/B/C, opt=0/2
play%0: invaders-opt0.exe
	./$< play$*

play%2: invaders-opt2.exe
	./$< play$*


# compile/link: opt(%)=0/2, with/without TRACE
invaders-opt%.exe: c/main-opt%.o c/program-opt%.o
	gcc $^ -o $@ -lSDL2

invaders-opt%-TRACE.exe: c/main-opt%.o c/program-opt%-TRACE.o
	gcc $^ -o $@ -lSDL2

CFLAGS = -Wall -Werror -Winline

c/main-opt%.o: c/main.c c/machine.h #Makefile
	gcc -O$* -DOPT=$* $(CFLAGS) -I /usr/include/SDL2 -c $< -o $@

c/program-opt%.o: c/program.c c/machine.h c/machine.c #Makefile
	gcc -O$* -DOPT=$* $(CFLAGS) -c $< -o $@ -Wno-unused-variable

c/program-opt%-TRACE.o: c/program.c c/machine.h c/machine.c #Makefile
	gcc -O$* -DOPT=$* -DTRACE $(CFLAGS) -c $< -o $@ -Wno-unused-variable


# generate c code...
c/program.c: src/*.hs
	stack run c
