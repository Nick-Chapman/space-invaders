#include <stdio.h>
#include "machine.h"

Control prog_0000 ();

static int icount = 0;
static int cycles = 0;

#define HALF_FRAME_CYCLES (2000000 / 120)

static int credit = HALF_FRAME_CYCLES;

int main () {
  Func fn = prog_0000;
  while (fn) {
    fn = (Func)fn();
  }
  return 0;
}

void todo(const char* s) {
  printf ("todo: %s\n",s);
}

void at(const char* s) {
}

void dump_state(const char* instruction, u16 pcAfterInstructionDecode) {
  printf("%8d  [%08d] "
         "PC:%04X "
         "A:%02X B:%02X C:%02X D:%02X E:%02X HL:%02X%02X SP:%02X%02X "
         "SZAPY:%1d%1d%1d%1d%1d"
         " : %s\n",
         icount,
         cycles,
         pcAfterInstructionDecode, //odd, but matches existing traces!
         A,B,C,D,E,H,L,SPH,SPL,
         FlagS,FlagZ,FlagA,FlagP,FlagCY,
         instruction
         );
}

void instruction(const char* instruction, u16 pcAfterInstructionDecode) {
  dump_state(instruction,pcAfterInstructionDecode);
  icount++;
  if (icount>50000) {
    printf("STOP\n");
    exit(0);
  }
}

void advance(int n) {
  cycles += n;
  credit -= n;
}

Control op_rst1();
Control op_rst2();

static bool half = false;
static int interrupts = 0;

static bool interrupts_enabled = false;

Control jumpDirect(u16 pc, Func f) {
  if (credit <= 0) {
    credit += HALF_FRAME_CYCLES;
    half ^= true;
    interrupts++;
    /*printf ("cycles = %d, interrupt (%d), enabled = %s: %s\n",
            cycles,
            interrupts,
            interrupts_enabled ? "ENABLED" : "disabled",
            half ? "rstHalf (RST 1)" : "rstVblank (RST 2)"
            );*/
    if (interrupts_enabled) {
      interrupts_enabled = false;
      PCH = e8_hi(pc);
      PCL = e8_lo(pc);
      return (Control)(half ? op_rst1 : op_rst2);
    }
  }
  //printf ("jumpDirect\n");
  return (Control)f;
}

Control jump16(u16 x) {
  switch(x) {
#define target(A) Control prog_##A (); case 0x##A: return jumpDirect( 0x##A, prog_##A );
    target(18DC)
    target(1959)
    target(08F8)
    target(195C)
    target(09BD)
    target(09C3)
    target(09B1)
    target(195F)
    target(1962)
    target(1965)
    target(1968)
    target(18DF)
    target(0AF2)
    target(0ADD)
    target(0020)
    target(005A)
    target(0ADA)
  default: {
    printf ("jump16: unknown target address: %04x\n",x);
    die;
  }
  }
}

void mem_write(u16 a,u8 e) {
  if (a>=MEM_SIZE) {
    printf ("mem_write: (a>=MEM_SIZE) : a=%04x, MEM_SIZE=%04x\n",a,MEM_SIZE);
    die
  }
  if (a<RAM_BASE) {
    printf ("mem_write: (a<RAM_BASE) : a=%04x, RAM_BASE=%04x\n",a,RAM_BASE);
    die
  }
  //printf ("mem_write: M[%04x] = %02x\n",a,e);
  mem[a] = e;
}

void sound_control(const char* sound,u1 b) {
  //printf ("sound_control: %s = %s\n", sound, b?"on":"off");
}

void enable_interrupts(void) {
  //printf ("enable_interrupts\n");
  interrupts_enabled = true;
}

u1 e1_true() { return 1; }
u1 e1_false() { return 0; }
u1 e1_flip(u1 x) { return !x; }
u1 e1_is_zero(u8 e) { return (e == 0); }
u1 e1_test_bit(u8 e,int n) { return (e>>n)&0x1; }
u1 e1_or_bit(u1 x,u1 y) { return x||y; }
u1 e1_and_bit(u1 x,u1 y) { return x&&y; }
u1 e1_hi_bit_of_17(u17 x) { return (x>>16)&0x1; }

u1 e1_parity(u8 e) {
  return (((e>>0)&0x1) + ((e>>1)&0x1) + ((e>>2)&0x1) + ((e>>3)&0x1)
          + ((e>>4)&0x1) + ((e>>5)&0x1) + ((e>>6)&0x1) + ((e>>7)&0x1)) % 2 == 0;
}

u1 e1_is_pressed(const char* s) { //TODO: use enum for buttons
  //printf ("e1_is_pressed: %s\n", s);
  return false;
}

u8 e8_hi(u16 a) { return a>>8; }
u8 e8_lo(u16 a) { return a & 0xFF; }
u8 e8_update_bit(u8 e,int n,u1 p) {
  return
    p
    ? e | (1<<n)
    : e & ~(1<<n)
    ;
}
u8 e8_complement(u8 e) { return ~e; }
u8 e8_and(u8 x,u8 y) { return x&y; }
u8 e8_or(u8 x,u8 y) { return x|y; }
u8 e8_xor(u8 x,u8 y) { return x^y; }
u8 e8_shiftR(u8 x,u8 y) { return x>>y; }
u8 e8_shiftL(u8 x,u8 y) { die; }
u8 e8_ite(u1 i,u8 t,u8 e) { die; }

u8 e8_read_mem(u16 a) {
  if (a>=MEM_SIZE) {
    printf ("e8_read_mem: (a>=MEM_SIZE) : a=%04x, MEM_SIZE=%04x\n",a,MEM_SIZE);
    die
  }
  u8 res = mem[a];
  //printf ("e8_read_mem: M[%04x] -> %02x\n",a,res);
  return res;
}

u16 e16_hi_lo(u8 hi,u8 lo) { return hi<<8 | lo; }
u16 e16_offset_addr(int n,u16 x) { return x+n; }
u16 e16_add_with_carry(u1 cin,u8 x,u8 y) { return cin+x+y; }
u16 e16_drop_hi_bit_of_17(u17 x) { return x & 0xffff; }

u17 e17_add(u16 x,u16 y) { return x+y; }
