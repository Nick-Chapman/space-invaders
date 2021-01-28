
top: test-C

test: test-A test-B test-C
play: playC1

speed: speedA0 speedA1 speedB0 speedB1 speedC0 speedC1
speed0: speedA0 speedB0 speedC0
speed1: speedA1 speedB1 speedC1
speedA: speedA0 speedA1
speedB: speedB0 speedB1
speedC: speedC0 speedC1

.SECONDARY:

# run test & diff against expected output: mode(%)=A/B/C, opt=0, needs TRACE
test-%: invaders-%-opt0-TRACE.exe
	./$< test > trace/test1.out
	git diff trace/test1.out


# check speed: mode(%)=A/B/C, opt=0/2
speed%0: invaders-%-opt0.exe
	@ ./$< speed

speed%1: invaders-%-opt1.exe
	@ ./$< speed


# play: mode(%)=A/B/C, opt=0/1
play%0: invaders-%-opt0.exe
	./$< play

play%1: invaders-%-opt1.exe
	./$< play


# compile/link: mode(%)=A/B/C, opt=0/1, opt-0 with/out TRACE

invaders-%-opt0-TRACE.exe: c/main-opt0.o c/program-%-opt0-TRACE.o
	gcc $^ -o $@ -lSDL2

invaders-%-opt0.exe: c/main-opt0.o c/program-%-opt0.o
	gcc $^ -o $@ -lSDL2

invaders-%-opt1.exe: c/main-opt1.o c/program-%-opt1.o
	gcc $^ -o $@ -lSDL2


CFLAGS = -Winline -Wall -Werror

# compile main: opt(%)=0/1

c/main-opt%.o: c/main.c c/machine.h #Makefile
	gcc -O$* -DOPT=$* $(CFLAGS) -I /usr/include/SDL2 -c $< -o $@


# compile generated program: mode(%)=A/B/C, opt=0/1, opt-0 with/out TRACE

c/program-%-opt0-TRACE.o: c/program-Mode%.c c/machine.h c/machine.c #Makefile
	gcc -O0 -DOPT=0 -DMODE_$* -DTRACE $(CFLAGS) -c $< -o $@ -Wno-unused-variable

c/program-%-opt0.o: c/program-Mode%.c c/machine.h c/machine.c #Makefile
	gcc -O0 -DOPT=0 -DMODE_$* $(CFLAGS) -c $< -o $@ -Wno-unused-variable

c/program-%-opt1.o: c/program-Mode%.c c/machine.h c/machine.c #Makefile
	gcc -O1 -DOPT=1 -DMODE_$* $(CFLAGS) -c $< -o $@ -Wno-unused-variable


# generate c code...
c/program-Mode%.c: src/*.hs
	stack run genc
