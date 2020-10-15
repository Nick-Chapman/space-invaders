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

- complete required instruction decoding and semantics; it never stops...
- condition flags (think we only need Z and Carry) --DONE
- interrupt handling --DONE
- IO-subsystem, including external shift register
- refactoring, data Cond = N | NZ | C | NC | ... for JP,CALL,RET
- speed measurements
- visualization

### Step 2. Compilation to standalone executable

- The goal!
