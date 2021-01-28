
#include "machine.h"
#include "machine.c"
#include "../genc/ModeC.c"

char mode[] = "C";

noinline Control jumpInterrupt(u16 pc, Func f) {
  credit += HALF_FRAME_CYCLES;
  half ^= true;
  if (interrupts_enabled) {
    interrupts_enabled = false;
    PCH = pc >> 8;
    PCL = pc & 0xFF;
    return (Control)(half ? op_CF : op_D7);
  }
  return (Control)f;
}

inline static Control jumpDirect(u16 pc, Func slow, Func fast) {
  if (credit > 300) { //longest fast program has size 296
    if (fast) {
      return (Control)fast;
    }
    return (Control)slow;
  }
  if (credit <= 0) {
    return jumpInterrupt(pc,slow);
  }
  return (Control)slow;
}

Control jump16(u16 pc) {
  if (pc>=ROM_SIZE) {
    printf ("jump16/C: (a>=ROM_SIZE) : a=%04x, ROM_SIZE=%04x\n",pc,ROM_SIZE);
    die;
  }
  Func slow = slow_progs_array[pc];
  if (slow == 0) {
    printf ("jump16/C : no slow program for target address: %04x\n",pc);
    die;
  }
  Func fast = fast_progs_array[pc];
  fast = fast ? fast : slow;
  if (fast == 0) {
    printf ("jump16/C : no fast program for target address: %04x\n",pc);
    die;
  }
  return jumpDirect(pc,slow,fast);
}
