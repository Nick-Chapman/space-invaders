# space-invaders


### Step 1. Emulation


Build/run emulation with SDL visualization:

- `stack run`
- Works fairly well; apart from not being quite complete & having some bugs!
- `[insert]` coin; then start playing with `F1`
- `z` left; `x` right; `[enter]` shoots
- Game can be paused: `[delete]`
- Dip-switches can be toggled: `F3`, `F5`, `F6`, `F7`
- TILT can be activated with `[tab]`
- Full keyboard-mapping show in panel next to the running game.


Status:

- 8080 emulation basically complete
- Headless emulation: `stack run trace`
- Regression tests: `./test.sh`
- View decode table `stack run decode`
- Emulation with Gloss visualization: `stack run gloss` (mem leak; slows over time)

Next:

- Speed measurements (no graphics)
- `SDL`: report fps; introduce fps limiter
- Refactor code in `Semantics.hs` to capture more sharing
- Implement further missing/required op-codes; implement all op-codes
- `DAA` (_decimal adjust accumulator_) so credits are calculated correctly
- Bug? - player doesn't die when shot: (attract mode or game play)
- Bug? - pixel gaps appear in line at bottom of screen (or is this expected?)


Crashes:

- `32801062 [277978420] 143C : Mem.write: 001C -- cant write to rom`

Debugging:

- Spot bugs while refactoring `Semantics.hs` (the dream!)
- Get trace from another emulator: tricky; will need interrupts to align precisely
- Setup testing using `cpudiag.asm`, (A) just by itself (B) trace vs another emulator
- Calculate/print static semantics from each opcode, and inspect


### Step 2. Retarget to standalone executable (Future Plan)


### Resource for Space Invaders and 8080
- [wikipedia](https://en.wikipedia.org/wiki/Space_Invaders)
- [computerarcheology](https://www.computerarcheology.com/Arcade/SpaceInvaders)
- [computerarcheology (Hardware)](https://www.computerarcheology.com/Arcade/SpaceInvaders/Hardware.html)
- [computerarcheology (Commented dissasembly)](https://www.computerarcheology.com/Arcade/SpaceInvaders/Code.html)
- [emulator101 (emulator tutorial)](http://www.emulator101.com)

### 8080 opcode summary
- [pastraiser](https://pastraiser.com/cpu/i8080/i8080_opcodes.html)
- [emulator101](http://www.emulator101.com/reference/8080-by-opcode.html)
- [classiccmp](http://www.classiccmp.org/dunfield/r/8080.txt)

### Contemporary 8080 Manuals (Scans; big!)
- [Systems User's Manual](http://www.nj7p.info/Manuals/PDFs/Intel/9800153B.pdf)
- [Assembly Language Programming Manual](http://www.classiccmp.org/dunfield/r/8080asm.pdf)

