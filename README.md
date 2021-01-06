# space-invaders


### Step 1. Emulation


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


### Step 2. Retarget to standalone executable

`stack run static`

    Writing file: gen/0-decode-table.out
    Writing file: gen/0-op-programs.out
    Writing file: gen/1-programs-for-every-address.out
    Writing file: gen/2-reachable-programs.out
    Writing file: gen/3-inlined-deep.out
    Writing file: gen/4-inlined-upto-joins.out


`stack run c`

    make speed

mhz=313, speedup=x156


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

