
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
test-%: obj/invaders-%-opt0-TRACE.exe
	./$< test > trace/test1.out
	git diff trace/test1.out


# check speed: mode(%)=A/B/C, opt=0/2
speed%0: obj/invaders-%-opt0.exe
	@ ./$< speed

speed%1: obj/invaders-%-opt1.exe
	@ ./$< speed


# play: mode(%)=A/B/C, opt=0/1
play%0: obj/invaders-%-opt0.exe
	./$< play

play%1: obj/invaders-%-opt1.exe
	./$< play


# compile/link: mode(%)=A/B/C, opt=0/1, opt-0 with/out TRACE

obj/invaders-%-opt0-TRACE.exe: obj/main-opt0.o obj/prog-%-opt0-TRACE.o
	gcc $^ -o $@ -lSDL2

obj/invaders-%-opt0.exe: obj/main-opt0.o obj/prog-%-opt0.o
	gcc $^ -o $@ -lSDL2

obj/invaders-%-opt1.exe: obj/main-opt1.o obj/prog-%-opt1.o
	gcc $^ -o $@ -lSDL2


CFLAGS = -Winline -Wall -Werror

# compile main: opt(%)=0/1

obj/main-opt%.o: c/main.c c/machine.h .obj
	gcc -O$* -DOPT=$* $(CFLAGS) -I /usr/include/SDL2 -c $< -o $@


# compile generated program: mode(%)=A/B/C, opt=0/1, opt-0 with/out TRACE

obj/prog-%-opt0-TRACE.o: c/prog-Mode%.c genc/Mode%.c c/machine.h c/machine.c .obj
	gcc -O0 -DOPT=0 -DMODE_$* -DTRACE $(CFLAGS) -c $< -o $@ -Wno-unused-variable

obj/prog-%-opt0.o: c/prog-Mode%.c genc/Mode%.c c/machine.h c/machine.c .obj
	gcc -O0 -DOPT=0 -DMODE_$* $(CFLAGS) -c $< -o $@ -Wno-unused-variable

obj/prog-%-opt1.o: c/prog-Mode%.c genc/Mode%.c c/machine.h c/machine.c .obj
	gcc -O1 -DOPT=1 -DMODE_$* $(CFLAGS) -c $< -o $@ -Wno-unused-variable


# generate c code...
genc/Mode%.c: src/*.hs .genc
	stack run genc


.obj:
	mkdir -p obj

.genc:
	mkdir -p genc
