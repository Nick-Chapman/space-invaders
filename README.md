# space-invaders


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



### Step 1. Emulation (WIP)

Currently we crash with an out of bounds memory write, 48 (simulated) seconds into the emulation.
(Happily, we reach this point in just 37 seconds!)

`stack run`

    invaders: *crash*
    11427701  [96764229] 15DF : Mem.write: 4017


To see more: `stack run -- -poi 11427701`

Next:

- refactor code in `Semantics.hs` to capture more sharing
- fix bugs in instruction execution
- IO-subsystem, including external shift register
- speed measurements
- visualization (`Gloss`)


### Step 2. Retarget to standalone executable (Future Plan)
