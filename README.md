# space-invaders



### Resource for Space Invaders and 8080
- [wikipedia](https://en.wikipedia.org/wiki/Space_Invaders)
- [computerarcheology](https://www.computerarcheology.com/Arcade/SpaceInvaders)
- [computerarcheology (Hardware)](https://www.computerarcheology.com/Arcade/SpaceInvaders/Hardware.html)
- [computerarcheology (Commented dissasembly)](https://www.computerarcheology.com/Arcade/SpaceInvaders/Code.html)
- [emulator101 (emulator tutorial)](http://www.emulator101.com)
- [loadzero's restoration](https://blog.loadzero.com/blog/si78c)

### 8080 opcode summary
- [pastraiser](https://pastraiser.com/cpu/i8080/i8080_opcodes.html)
- [emulator101](http://www.emulator101.com/reference/8080-by-opcode.html)
- [classiccmp](http://www.classiccmp.org/dunfield/r/8080.txt)

### Contemporary 8080 Manuals (Scans; big!)
- [Systems User's Manual](http://www.nj7p.info/Manuals/PDFs/Intel/9800153B.pdf)
- [Assembly Language Programming Manual](http://www.classiccmp.org/dunfield/r/8080asm.pdf)



### Step 1. Emulation (TODO: rewrite)

Dependencies:
```
sudo apt-get install libsdl2-dev libsdl2-ttf-dev libsdl2-mixer-dev
```

Build/run emulation with SDL visualization:

- `stack run`
- `[insert]` coin; then start playing with `F1`
- `z` left; `x` right; `[enter]` shoots
- Game can be paused: `[delete]`
- Dip-switches can be toggled: `F3`, `F5`, `F6`, `F7`
- TILT can be activated with `[tab]`
- `stack run -- -controls` : Keyboard-mapping shown in panel next to the running game.
- `stack run -- -sf 4` : Adjust scale factor. Sensible values: 1-4

Status:

- 8080 emulation complete, except `HLT` unimplemented
- SDL emulation is quick enough -- *just* -- measured fps is reported
- `stack run` runs as fast the machine allows; about 70 fps on my laptop
- fps can be limited with a command line flag, i.e.  `stack run -- -fps 60`
- Headless emulation: `stack run trace`
- Quick regression test: `stack run test` (4 secs)
- Slow regression test: `stack run test2` (20 secs)
- Implemented `DAA` (_decimal adjust accumulator_), so credits are calculated correctly
- Spot bugs while refactoring `Semantics.hs` -- _it worked!_
- Speed measurements (no graphics) : `stack run speed-test`
- `cpudiag` testing: (`TST8080.COM`) `stack run tst`
- `143B : Mem.write: 001C -- cant write to rom` -- expected? so crash disabled


### Step 2. Compilation of emulator effects

Initial experiments involved interpreting the emulation `Effect`s, by `Compile`ing to `Residual` code. The compilation is performed with various levels of _inlining_ and the results written to files in `gen/` for inspection. This `Static` exploration performs a reachability analysis from `startPoints` in the code. Some of the start points are not determined automatically, but from information on RAM usage at [computerarcheology](https://www.computerarcheology.com/Arcade/SpaceInvaders/RAMUse.html), in particular the addresses for the handler code of the various _game objects_. The static info is regenerated with: `stack run static`

    Writing file: gen/0-decode-table.out
    Writing file: gen/0-op-programs.out
    Writing file: gen/1-programs-for-every-address.out
    Writing file: gen/2-reachable-programs.out
    Writing file: gen/3-inlined-deep.out
    Writing file: gen/4-inlined-upto-joins.out


The generated `Residual` code can be emulated within haskell, to give `FastEmulate`, a drop-in replacement for the original `SlowEmulate` which emulated the `Effect` directly. (The `Emulate` module can switch at haskell build time between the `Slow` and `Fast` variants, then `stack run speed-test` can show how fast we can run, in both _frames per second (fps)_ and as a _speedup_ factor (= `fps/60`). Alternatively, we can run `stack run -- -controls` to play the game with _fps_ showing in a side panel.

On my machine, `SlowEmulate` shows a speed of around 63 fps (`stack run speed-test`). However when graphics rendering is performed (`stack run -- -controls`), this figure drops to around 55. And then down to just `45` when a game is started (press `Insert`, followed by `F1`). These numbers are improved somewhat with `FastEmulate`. `spped-test` shows a fps of around 110 or so. With graphics we see around 95, dropping to 59 or so when playing a game.

At one point I think I had `FastEmulate` consistently playing a game at 70fps, but some code cleanup and refactoring reduced this to the current level. But we don't care! Because the point of this compilation is not to have a faster Haskell emulator, but to be able to write out the `Residual` program in a lower-level language, and have that compiled to a native executable. Which ought to run much quicker!...


### Step 3. Generation of standalone executable

The approach taken is to generate `c` code from our compiled `Residual` program, and then have this compiled to native code by `gcc`. As well as the generated C code there is a small framework of handwrittem C-code which provides:

    - declarations for the machine-state (memory, registers, cycle-count etc)
    - implementation for a some basic functions, such as `sound_control` and `parity`
    - implementation for `jumpDirect` and `jump16` which form the glue between execution of generated code
    - code to execute interrupts (`RST 1` and `RST 2`) every half-frame, or 2000000/120 cycles
    - trampoline loop. `while (fn) { fn = (Func)fn(); }`
    - entry points for execution: `test`, `speed` and `play`
    - rendering using SDL

#### Test

Run `make test` to check that `trace/test1.out` is generated correctly; the same trace is obtained from the haskell emulator by running `stack run test1`. This corresponds to running 50000 instructions (or 386490 cycles). Which equates to nearly 1/5 second of simulated time. This is just long enough for interrupt handling to become enabled, and so is a good test.

#### Play

Run `make play` to run the game. No sound yet (unlike the haskell version). But also no noticeable pauses (unlike the haskell version which I assume pauses during garbage collection). And no crazy fan spinning. Because we are using just a fraction of the host CPU to run the emulation. Here's the keys:

      <escape>  : quit
      <insert>  : insert coin
      F1        : player-1 start
      F2        : player-2 start
      'z'       : left
      'x'       : right
      <return>  : fire
      <tab>     : tilt the machine

#### Modes of code generation

We generate c-code in 3 modes (named: `A`, `B` and `C`). These modes correspond to different inlining levels as explored above, and written out to the `gen/` subdir.

    A: generate one c-function per op-code                  (gen/0-op-programs)
    B: generate one c-function per reachable ROM address    (gen/2-reachable-programs)
    C: generate one c-function per join-point ROM address   (gen/4-inlined-upto-joins)


(For ease of experimentation, and because of the overlap of the generated code between the modes, the generated code for all modes co-exists in the one file `c/program.c`)

Mode A is rather like what you would get if you wrote an emulator by hand, using the standard `fetch; decode; exec` architecture. The program counter is maintained in a pair of registers (`PCH` and `PCL`), which are used to `fetch` and must be incremented correctly with the instruction size. The `decode` step is very minimal, and just amounts to an indirection through an array containing pointers to the 256 functions, one per op-code. The `exec` is performed by the code generated for each op code.

Mode-B improves on Mode-A by generating one c-function per reachable ROM address. (These are referred to as the _slow_ programs to distinguish against the _fast_ programs generated for Mode-C.) The advantage of Mode-B is that eliminates the `fetch` and `decode` steps. We needn't explicitly maintain state for the program counter; each instruction _knows_ at what PC it resides. And after execution, when moving to the _next_ instruction, it is often the case that we know statically exactly what c-function to execute next. This is true for `JUMP` and `CALL` instructions, as well as instruction which just _fall though_ to the next instruction. The main exception to knowing statically where to continue execution is the `RET` instruction, which finds the address on the stack (via `SPH` and `SPL`). In this case we end the execution of a program segment (c-function) with `jump16` instead of `jumpDirect`. A `jump16` allows execution to continue anywhere by indirecting through a ROM-sized array of all the generated c-functions.

Mode C goes further still by generating programs (c-function) which cover linear sequences of instructions. The advantage of this is that we spend more execution time doing the emulation, and less time jumping (through the trampoline) from one code segment to the next. An additional benefit is that the generated c-code has longer basic blocks for the c-compiler to optimize. And moreover some execution effects never have to be written as generated c-code at all. For example, in the case that an instruction effect is masked by the effect of a following instruction. This happens more often that you might imagine! 8080 instructions often have effects on multiple registers and flags. Very often the flag effects are not inspected. If a subsequent instruction overwrites the same register or flag as a previous instruction, we needn't generate code for the initial write. We do however have to _flush_ out all the register/flag writes before exiting each code segment at the final jump.

#### Interrupt handling in Mode-C

There is one further tricky detail to consider when running with Mode-C code, which is it's interaction with the interrupt handling. These semantics of interrupt handling is to process the interrupt after the currently executing instruction has finished executing. The standard way for an emulator to achieve this is by tracking CPU cycles, and here we decrement `credit` counter containing remaining cycles before the next interrupt is processed. This `credit` is reduced when we call `advance(int)` as part of the execution of each instruction. When we jump/trampoline from one c-function to the next, we process an interrupt if credit has fallen below 0, (incrementing the `credit` with a new half-frame's counts worth of cycles). This all works fine when we jump between every emulated instruction. However, in mode-C, each c-function corresponds to multiple instructions. If we did nothing, then we would be late in processing the interrupts.

It turns out that for space-invaders, this is not a real problem. The code is just not that sensitive to the precise timing of interrupt handling. But for many systems this will not be so. So the question is, can we emulate using faster Mode-C code, and still get the precise timing for interrupts? The answer is yes!. All we need do is compute the maximum length (in cycles) of any of our fast Mode-C programs, and when the `credit` falls below this value we switch to jumping to the _slow_ programs. This works. And we validate that it works by seeing that we get the identical `trace/test1.out` when running `make test` (which is setup to execute in Mode-C). To get some idea of the small cost of switching to _slow_ programs for the final few cycles of each half-frame period, we note that the longest Mode-C code sequence is just 296 cycles, versus the 16666 cycles per half-frame.


#### Timings

So, how quick can we emulate the generated code in modes A/B/C under varying gcc optimization levels.

`make speed`

    version=mode=A,opt=0, sim-time(secs)=600, duration(s)=3.65,  mhz(effective)=328,  speedup(over 2Mhz)=x164
    version=mode=A,opt=1, sim-time(secs)=600, duration(s)=1.22,  mhz(effective)=985,  speedup(over 2Mhz)=x492
    version=mode=A,opt=2, sim-time(secs)=600, duration(s)=1.13,  mhz(effective)=1058, speedup(over 2Mhz)=x529
    version=mode=A,opt=3, sim-time(secs)=600, duration(s)=1.58,  mhz(effective)=761,  speedup(over 2Mhz)=x380

    version=mode=B,opt=0, sim-time(secs)=600, duration(s)=1.74,  mhz(effective)=691,  speedup(over 2Mhz)=x345
    version=mode=B,opt=1, sim-time(secs)=600, duration(s)=0.592, mhz(effective)=2026, speedup(over 2Mhz)=x1013
    version=mode=B,opt=2, sim-time(secs)=600, duration(s)=0.655, mhz(effective)=1831, speedup(over 2Mhz)=x915
    version=mode=B,opt=3, sim-time(secs)=600, duration(s)=0.659, mhz(effective)=1819, speedup(over 2Mhz)=x909

    version=mode=C,opt=0, sim-time(secs)=600, duration(s)=1.12,  mhz(effective)=1075, speedup(over 2Mhz)=x537
    version=mode=C,opt=1, sim-time(secs)=600, duration(s)=0.281, mhz(effective)=4264, speedup(over 2Mhz)=x2132
    version=mode=C,opt=2, sim-time(secs)=600, duration(s)=0.295, mhz(effective)=4074, speedup(over 2Mhz)=x2037
    version=mode=C,opt=3, sim-time(secs)=600, duration(s)=0.319, mhz(effective)=3764, speedup(over 2Mhz)=x1882


This make command builds executables for each of the 4 gcc optimization levels: 0-3, and then runs each 3 times, using the generate code for each of the three modes: A/B/C. Each run covers 10 simulated minutes, i.e. (10 * 60 * 2000000) cycles. No graphics are rendered. Run time is computed within the program, in micro-seconds, using `gettimeofday`.

The times reported appear relatively stable from one run to another. The `mhz(effective)` reports the effective speed the emulated CPU is being run at. The real hardware runs at 2Mhz. The final column reports the speedup factor we have achieved.

Compared to the haskell emulation, where we didn't even quite reach x2 speedup, even the slowest speedup of x164 reported above is quite amazing. And this rises to a factor of over x2000!

What do we see in the numbers?

-O3 is terrible. In fact the -O3 executable can only be generated if we remove `-Winline` from `$(CFLAGS)` in the `Makefile`. This means that some functions which we have annotated as `inline` are not being inlined. Probably because another function we we don't want inlined, is being inlined.

-O2 seems pretty unhelpful. It achieves something (over -O1) for Mode-A, but just makes things slower for Mode-B and C.

-O1 is nice. We get a speedup (over -O0) of around 3 to 4, depending what mode we use.

So, how do Modes A/B/C compare. For -O1, B is roughly twice as fast as A. And C is twice as fast again.
And for -O0 it is similar, although Mode-C does not get to shine quite so much.

Going forward I will only continue with optimization levels 0 and 1.
-O1 because it is the quickest to run.
-O0 because it compiles much quicker (3s vs 10s).



#### Play Faster

The graphic rendering limits/locks the rendering to the host TV refresh rate (`SDL_RENDERER_PRESENTVSYNC`). By independently measuring and reporting the fps at which we are rendering, I can see I am getting exactly 60 fps on my latop.

But we can run the emulation at whatever speed we like, just by varying the number of cycles of the 8080 we emulate between each graphics frame we render. Initially this is set as 2000000/60 = 33333 cycles. Which gives us the full correct speed. Or a speedup factor of x1. To vary the game speed, I added a couple of key bindings.

      ','       : increase emulation speed
      '.'       : decrease emulation speed

Pressing the keys will repeatedly multiply/divide the speedup factor by 1.1 (just make short key presses!) and print the new speed to the console. This allows us to very easily see what an overclocked 8080 space invaders looks like.

Running `make play` runs the Mode-C code, compiled with `-O1` optimization. This should allow us to run up with a speed up of up to x2000.

Above a factor of x3 the game becomes rather unplayable. But at x10 it is still quite nice to watch the attract screens wiz by. We can go faster still... until we reach a factor of x2000 or so. Here we will notice that the fps reported starts to drop below 60 fps. This shows that we are unable to emulate the number of cycles attempted (200000/60 * speedup-factor) within the allotted 1/60s and leaving enough time to actually do the graphics render!

So for example, at x10000 speedup, I see a reported fps of 12. (And my laptop fan starts to spin wildly). So 1/5 of the 60 fps it should be. So showing we cannot do better than the x2000 speedup which the `make speed` numbers told us to expect.

### Update 2024

Old laptop...
```
nic@luna:~/code/space-invaders$ make speed
version=mode=A,opt=0, duration(s)=3.61, speedup=x166
version=mode=A,opt=1, duration(s)=1.21, speedup=x495
version=mode=B,opt=0, duration(s)=1.68, speedup=x356
version=mode=B,opt=1, duration(s)=0.626, speedup=x959
version=mode=C,opt=0, duration(s)=1.07, speedup=x558
version=mode=C,opt=1, duration(s)=0.267, speedup=x2245
```
