#include <stdio.h>
#include "machine.h"

Control prog_0000 ();

int icount = 0;
int cycles = 0;

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

void instruction(const char* instruction, u16 pcAfterInstructionDecode) {
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
  icount++;
}

void advance(int n) {
  cycles += n;
}

Control jumpDirect(Func f) {
  return (Control)f;
}

Control jump16(u16 x) {
  printf ("jump16: %x\n",x);
  return 0; //TODO: lookup in array of reachable code
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

u1 e1_true() { return 1; }
u1 e1_false() { return 0; }
u1 e1_flip(u1 x) { return !x; }
u1 e1_is_zero(u8 e) { return (e == 0); }
u1 e1_test_bit(u8 e,int n) { return (e>>n)&0x1; }
u1 e1_or_bit(u1 x,u1 y) { return x||y; }
u1 e1_and_bit(u1 x,u1 y) { return x&&y; }

u1 e1_parity(u8 e) {
  return (((e>>0)&0x1) + ((e>>1)&0x1) + ((e>>2)&0x1) + ((e>>3)&0x1)
          + ((e>>4)&0x1) + ((e>>5)&0x1) + ((e>>6)&0x1) + ((e>>7)&0x1)) % 2 == 0;
}

u1 e1_is_pressed(const char* s) { //TODO: use enum for buttons
  printf ("e1_is_pressed: %s\n", s);
  die;
}

u8 e8_hi(u16 a) { return a>>8; }
u8 e8_lo(u16 a) { return a & 0xFF; }
u8 e8_update_bit(u8 e,int n,u1 p) { die; }
u8 e8_complement(u8 e) { return ~e; }
u8 e8_and(u8 x,u8 y) { return x&y; }
u8 e8_or(u8 x,u8 y) { die; }
u8 e8_xor(u8 x,u8 y) { die; }
u8 e8_shiftR(u8 x,u8 y) { die; }
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
