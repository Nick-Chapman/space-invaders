
// machine.c -- this file gets included by the generated program.c

#include <stdio.h>
#include "machine.h"

#define ROM_SIZE 0x2000
#define MEM_SIZE 0x4000

#define noinline __attribute__ ((noinline))


#ifdef TRACE
#define instruction(x...) f_instruction(x)
#else
#define instruction(x...) {}
#endif

void f_instruction(const char*, u16);

#define HALF_FRAME_CYCLES (2000000 / 120)
int credit = HALF_FRAME_CYCLES;
extern int credit;

long icount = 0;
long cycles = 0;

inline static void advance(int n) {
  credit -= n;
  cycles += n;
#ifdef TRACE
  icount++;
#endif
}


inline static void sound_control(const char* sound,u1 b) {
  //printf ("sound_control: %s = %s\n", sound, b?"on":"off");
}

static bool interrupts_enabled = false; // really this is state, just like a register

inline static void enable_interrupts(void) {
  interrupts_enabled = true;
}

inline static void disable_interrupts(void) {
  interrupts_enabled = false;
}

noinline static void unknown_output(int p,u8 b) { //watchdog
  //printf ("unknown_output: %d %02x\n",p,b);
}

inline static u8 e8_update_bit(u8,int,u1);
u8 e8_update_bit(u8 e,int n,u1 p) {
  return
    p
    ? e | (1<<n)
    : e & ~(1<<n)
    ;
}

inline static u8 e8_read_mem(u16 a) {
  if (a>=MEM_SIZE) {
    //printf ("e8_read_mem: (a>=MEM_SIZE) : a=%04x, MEM_SIZE=%04x\n",a,MEM_SIZE);
    die
  }
  u8 res = mem[a];
  //printf ("e8_read_mem: M[%04x] -> %02x\n",a,res);
  return res;
}

static bool half = false;
static int interrupts = 0;

static Control op_CF (); // defined in generated code
static Control op_D7 (); // defined in generated code


noinline Control jumpInterrupt(u16 pc, Func f) {
  credit += HALF_FRAME_CYCLES;
  half ^= true;
  interrupts++;
  if (interrupts_enabled) {
    interrupts_enabled = false;
    PCH = pc >> 8;
    PCL = pc & 0xFF;
    return (Control)(half ? op_CF : op_D7);
  }
  return (Control)f;
}

inline static Control jumpDirect(u16 pc, Func f) {
  if (credit <= 0) {
    return jumpInterrupt(pc,f);
  }
  return (Control)f;
}

noinline static void mem_write(u16 a,u8 e) {
  if (a>=MEM_SIZE) {
    //printf ("mem_write: (a>=MEM_SIZE) : a=%04x, MEM_SIZE=%04x\n",a,MEM_SIZE);
    //die;
    a -= 0x2000; // ram mirror
    //return;
  }
  if (a<ROM_SIZE) {
    //printf ("mem_write: (a<ROM_SIZE) : a=%04x, ROM_SIZE=%04x\n",a,ROM_SIZE);
    //die;
    return;
  }
  //printf ("mem_write: M[%04x] = %02x\n",a,e);
  mem[a] = e;
}

inline static u1 e1_parity(u8 e) {
  return !(((e>>0)&0x1) ^ ((e>>1)&0x1) ^ ((e>>2)&0x1) ^ ((e>>3)&0x1)
           ^ ((e>>4)&0x1) ^ ((e>>5)&0x1) ^ ((e>>6)&0x1) ^ ((e>>7)&0x1));
}

noinline static u8 e8_unknown_input(int p) {
  //printf ("unknown_input: %d\n",p);
  die
}

bool use_per_address_programs = true;
bool use_fast_programs = false;

extern Func ops_array [];
extern Func output_instruction_array [];
extern Func input_instruction_array [];
extern Func slow_progs_array [];
extern Func fast_progs_array [];

Control jump16(u16 pc) {
  if (use_per_address_programs) {

    if (pc>=ROM_SIZE) {
      printf ("jump16: (a>=ROM_SIZE) : a=%04x, ROM_SIZE=%04x\n",pc,ROM_SIZE);
      die;
    }
    Func fn =
      use_fast_programs
      ? fast_progs_array[pc]
      : slow_progs_array[pc];

    if (fn == 0) {
      printf ("jump16: no program for target address: %04x\n",pc);
      die;
    }
    return jumpDirect(pc,fn);
  }

  u8 byte = mem[pc];

  if (byte==0xD3) { //OUT
    u8 imm1 = mem[pc+1];
    Func fn = output_instruction_array[imm1];
    u16 pcAfterDecode = pc+2;
    PCH = pcAfterDecode >> 8;
    PCL = pcAfterDecode & 0xFF;
    return jumpDirect(pc,fn);

  } else if (byte==0xDB) { //IN

    u8 imm1 = mem[pc+1];
    Func fn = input_instruction_array[imm1];
    u16 pcAfterDecode = pc+2;
    PCH = pcAfterDecode >> 8;
    PCL = pcAfterDecode & 0xFF;
    return jumpDirect(pc,fn);

  } else { //other op-code

    Func fn = ops_array[byte];
    u16 pcAfterDecode = pc+1;
    PCH = pcAfterDecode >> 8;
    PCL = pcAfterDecode & 0xFF;
    return jumpDirect(pc,fn);
  }

}
