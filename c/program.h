
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "shared.h"

#define noinline __attribute__ ((noinline))

#define ROM_SIZE 0x2000

Func prog [ROM_SIZE];

u1 FlagS;
u1 FlagZ;
u1 FlagA;
u1 FlagP;
u1 FlagCY;

u8 PCH;
u8 PCL;

u8 A;
u8 B;
u8 C;
u8 D;
u8 E;
u8 H;
u8 L;
u8 SPH;
u8 SPL;

u8 Shifter_HI;
u8 Shifter_LO;
u8 Shifter_OFF;

inline static void f_at(const char*);
inline static void f_instruction(const char*, u16);
inline static void advance(int);


#define instruction(x...) f_instruction(x)
//#define instruction(x...) {}

//#define at(x...) f_at(x)
#define at(x...) {}


noinline Control jumpInterrupt(u16 pc, Func f);
inline static Control jumpDirect(u16,Func);
inline static Control jump16(u16);

noinline static void mem_write(u16,u8);
inline static void sound_control(const char*,u1);
inline static void enable_interrupts(void);
inline static void unknown_output(int,u8);

noinline static u1 e1_parity(u8);

inline static u8 e8_update_bit(u8,int,u1);
inline static u8 e8_read_mem(u16);

extern Control op_rst1();
extern Control op_rst2();

int icount = 0;
long cycles = 0;
bool dump_state_every_instruction = false;

#define HALF_FRAME_CYCLES (2000000 / 120)

static int credit = HALF_FRAME_CYCLES;

static bool half = false;
static int interrupts = 0;

static bool interrupts_enabled = false;

void f_at(const char* s) {
}

void dump_state(const char* instruction, u16 pcAfterInstructionDecode) {
  printf("%8d  [%08ld] "
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

void f_instruction(const char* instruction, u16 pcAfterInstructionDecode) {
  if (dump_state_every_instruction) {
    dump_state(instruction,pcAfterInstructionDecode);
  }
  icount++;
}

void advance(int n) {
  cycles += n;
  credit -= n;
}

Control jumpInterrupt(u16 pc, Func f) {
  credit += HALF_FRAME_CYCLES;
  half ^= true;
  interrupts++;
  if (interrupts_enabled) {
    interrupts_enabled = false;
    PCH = pc >> 8;
    PCL = pc & 0xFF;
    return (Control)(half ? op_rst1 : op_rst2);
  }
  return (Control)f;
}

Control jumpDirect(u16 pc, Func f) {
  if (credit <= 0) {
    return jumpInterrupt(pc,f);
  }
  return (Control)f;
}


Control jump16(u16 a) {
  //printf ("(%d) jump16: target address = %04x\n",icount,a);
  if (a>=ROM_SIZE) {
    printf ("jump16: (a>=ROM_SIZE) : a=%04x, ROM_SIZE=%04x\n",a,ROM_SIZE);
    die
  }
  Func fn = prog[a];
  if (fn == 0) {
    printf ("jump16: no program for target address: %04x\n",a);
    die
  }
  return jumpDirect(a,fn);
}

void mem_write(u16 a,u8 e) {
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


void sound_control(const char* sound,u1 b) {
  //printf ("sound_control: %s = %s\n", sound, b?"on":"off");
}

void enable_interrupts(void) {
  //printf ("enable_interrupts\n");
  interrupts_enabled = true;
}

void unknown_output(int p,u8 b) {
  //printf ("unknown_output: %d %02x\n",p,b);
}

u1 e1_parity(u8 e) {
  return (((e>>0)&0x1) + ((e>>1)&0x1) + ((e>>2)&0x1) + ((e>>3)&0x1)
          + ((e>>4)&0x1) + ((e>>5)&0x1) + ((e>>6)&0x1) + ((e>>7)&0x1)) % 2 == 0;
}

u8 e8_update_bit(u8 e,int n,u1 p) {
  return
    p
    ? e | (1<<n)
    : e & ~(1<<n)
    ;
}

u8 e8_read_mem(u16 a) {
  if (a>=MEM_SIZE) {
    printf ("e8_read_mem: (a>=MEM_SIZE) : a=%04x, MEM_SIZE=%04x\n",a,MEM_SIZE);
    die
  }
  u8 res = mem[a];
  //printf ("e8_read_mem: M[%04x] -> %02x\n",a,res);
  return res;
}
