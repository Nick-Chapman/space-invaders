
top: test

all: invaders-opt0.exe invaders-opt3.exe

CFLAGS = -Wall -Werror -Winline -I /usr/include/SDL2
LDFLAGS = -lSDL2


test: testB

testA: invaders-opt0.exe
	./$< testA > trace/test1.out
	git diff trace/test1.out

testB: invaders-opt0.exe
	./$< testB > trace/test1.out
	git diff trace/test1.out

testC: invaders-opt0.exe
	./$< testC > trace/test1.out
	git diff trace/test1.out


speed: speedA0 speedB0 speedC0 speedA3 speedB3 speedC3

speedA0: invaders-opt0.exe
	./$< speedA

speedB0: invaders-opt0.exe
	./$< speedB

speedC0: invaders-opt0.exe
	./$< speedC

speedA3: invaders-opt3.exe
	./$< speedA

speedB3: invaders-opt3.exe
	./$< speedB

speedC3: invaders-opt3.exe
	./$< speedC


play: playC

playA: invaders-opt0.exe
	./$< playA

playB: invaders-opt0.exe
	./$< playB

playC: invaders-opt0.exe
	./$< playC


invaders-opt0.exe: c/main-opt0.o c/program-opt0.o
	gcc $^ -o $@ $(LDFLAGS)

c/program-opt0.o: c/program.c c/machine.h c/machine.c Makefile
	gcc -O0 $(CFLAGS) -c $< -o $@ -Wno-unused-variable

c/main-opt0.o: c/main.c c/machine.h Makefile
	gcc -O0 $(CFLAGS) -c $< -o $@


invaders-opt3.exe: c/main-opt3.o c/program-opt3.o
	gcc $^ -o $@ $(LDFLAGS)

c/program-opt3.o: c/program.c c/machine.h c/machine.c Makefile
	gcc -O3 $(CFLAGS) -c $< -o $@ -Wno-unused-variable

c/main-opt3.o: c/main.c c/machine.h Makefile
	gcc -O3 $(CFLAGS) -c $< -o $@



c/program.c: src/*.hs
	stack run c
