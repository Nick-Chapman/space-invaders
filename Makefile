
top: testC

speed: speedA0 speedA1 speedB0 speedB1 speedC0 speedC1
speed0: speedA0 speedB0 speedC0
speed1: speedA1 speedB1 speedC1
play: playC1

.PRECIOUS: invaders-A-opt%.exe invaders-A-opt%-TRACE.exe invaders-B-opt%.exe invaders-B-opt%-TRACE.exe invaders-C-opt%.exe invaders-C-opt%-TRACE.exe


# check speed: mode(%)=A/B/C, opt=0/2
speed%0: invaders-%-opt0.exe
	@ ./$< speed

speed%1: invaders-%-opt1.exe
	@ ./$< speed


# run test & diff against expected output: mode(%)=A/B/C, opt=0, needs TRACE
test%: invaders-%-opt0-TRACE.exe
	./$< test > trace/test1.out
	git diff trace/test1.out

# play: mode(%)=A/B/C, opt=0/1
play%0: invaders-opt0.exe
	./$< play$*

play%1: invaders-opt1.exe
	./$< play$*


# compile/link: opt(%)=0/1/2/3, with/without TRACE
invaders-A-opt%.exe: c/main-opt%.o c/program-A-opt%.o
	gcc $^ -o $@ -lSDL2

invaders-B-opt%.exe: c/main-opt%.o c/program-B-opt%.o
	gcc $^ -o $@ -lSDL2

invaders-C-opt%.exe: c/main-opt%.o c/program-C-opt%.o
	gcc $^ -o $@ -lSDL2

invaders-A-opt%-TRACE.exe: c/main-opt%.o c/program-A-opt%-TRACE.o
	gcc $^ -o $@ -lSDL2

invaders-B-opt%-TRACE.exe: c/main-opt%.o c/program-B-opt%-TRACE.o
	gcc $^ -o $@ -lSDL2

invaders-C-opt%-TRACE.exe: c/main-opt%.o c/program-C-opt%-TRACE.o
	gcc $^ -o $@ -lSDL2

#CFLAGS = --param large-function-growth=2000 -Winline -Wall -Werror
CFLAGS = -Winline -Wall -Werror

c/main-opt%.o: c/main.c c/machine.h #Makefile
	gcc -O$* -DOPT=$* $(CFLAGS) -I /usr/include/SDL2 -c $< -o $@

c/program-A-opt%.o: c/program.c c/machine.h c/machine.c #Makefile
	gcc -O$* -DOPT=$* -DMODE_A $(CFLAGS) -c $< -o $@ -Wno-unused-variable

c/program-B-opt%.o: c/program.c c/machine.h c/machine.c #Makefile
	gcc -O$* -DOPT=$* -DMODE_B $(CFLAGS) -c $< -o $@ -Wno-unused-variable

c/program-C-opt%.o: c/program.c c/machine.h c/machine.c #Makefile
	gcc -O$* -DOPT=$* -DMODE_C $(CFLAGS) -c $< -o $@ -Wno-unused-variable

c/program-A-opt%-TRACE.o: c/program.c c/machine.h c/machine.c #Makefile
	gcc -O$* -DOPT=$* -DMODE_A -DTRACE $(CFLAGS) -c $< -o $@ -Wno-unused-variable

c/program-B-opt%-TRACE.o: c/program.c c/machine.h c/machine.c #Makefile
	gcc -O$* -DOPT=$* -DMODE_B -DTRACE $(CFLAGS) -c $< -o $@ -Wno-unused-variable

c/program-C-opt%-TRACE.o: c/program.c c/machine.h c/machine.c #Makefile
	gcc -O$* -DOPT=$* -DMODE_C -DTRACE $(CFLAGS) -c $< -o $@ -Wno-unused-variable


# generate c code...
c/program.c: src/*.hs
	stack run c
