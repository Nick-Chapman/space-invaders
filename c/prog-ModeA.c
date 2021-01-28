
#include "machine.h"
#include "machine.c"
#include "../genc/ModeA.c"

char mode[] = "A";

noinline Control jumpInterrupt(u16 pc, Func prog) {
  credit += HALF_FRAME_CYCLES;
  half ^= true;
  if (interrupts_enabled) {
    interrupts_enabled = false;
    PCH = pc >> 8;
    PCL = pc & 0xFF;
    return (Control)(half ? op_CF : op_D7);
  }
  return (Control)prog;
}

noinline Control jump16(u16 pc) {
  u8 byte = mem[pc];
  Func prog = ops_array[byte];
  if (prog == 0) {
    printf ("jump16/A : no program for op-code: %02x\n",byte);
    die;
  }
  u16 pcAfterDecode = pc+1;
  PCH = pcAfterDecode >> 8;
  PCL = pcAfterDecode & 0xFF;
  if (credit <= 0) {
    return jumpInterrupt(pc,prog);
  } else {
    return (Control)prog;
  }
}

inline static Control jumpDirect(u16 pc, Func slow, Func fast) {
  return jump16(pc);
}
