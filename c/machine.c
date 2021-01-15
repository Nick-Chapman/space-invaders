
// machine.c -- this file gets included by the generated program.c

#include <stdio.h>
#include "machine.h"

#define ROM_SIZE 0x2000
#define MEM_SIZE 0x4000

#define noinline __attribute__ ((noinline))


void dump_state (const char* instruction,
                 u8 PCH, u8 PCL, u8 A, u8 B, u8 C, u8 D, u8 E, u8 H, u8 L, u8 SPH, u8 SPL,
                 u1 FlagS, u1 FlagZ, u1 FlagA, u1 FlagP, u1 FlagCY
                 ) {

  if (icount>50000) {
    printf("STOP\n");
    exit(0);
  }

  printf("%8ld  [%08ld] "
         "PC:%02X%02X "
         "A:%02X B:%02X C:%02X D:%02X E:%02X HL:%02X%02X SP:%02X%02X "
         "SZAPY:%1d%1d%1d%1d%1d"
         " : %s\n",
         icount,
         cycles,
         PCH,PCL,
         A,B,C,D,E,H,L,SPH,SPL,
         FlagS,FlagZ,FlagA,FlagP,FlagCY,
         instruction
         );
}

void f_instruction0 (u8 PCH, u8 PCL, u8 A, u8 B, u8 C, u8 D, u8 E, u8 H, u8 L, u8 SPH, u8 SPL,
                     u1 FlagS, u1 FlagZ, u1 FlagA, u1 FlagP, u1 FlagCY,
                     const char* ipat) {
  dump_state(ipat,
             PCH,PCL,A,B,C,D,E,H,L,SPH,SPL,
             FlagS,FlagZ,FlagA,FlagP,FlagCY
             );
}

void f_instruction1 (u8 PCH, u8 PCL, u8 A, u8 B, u8 C, u8 D, u8 E, u8 H, u8 L, u8 SPH, u8 SPL,
                     u1 FlagS, u1 FlagZ, u1 FlagA, u1 FlagP, u1 FlagCY,
                     const char* ipat, u8 b1) {
  static char instruction[256];
  sprintf(instruction,ipat,b1);
  dump_state(instruction,
             PCH,PCL,A,B,C,D,E,H,L,SPH,SPL,
             FlagS,FlagZ,FlagA,FlagP,FlagCY
             );
}

void f_instruction2 (u8 PCH, u8 PCL, u8 A, u8 B, u8 C, u8 D, u8 E, u8 H, u8 L, u8 SPH, u8 SPL,
                     u1 FlagS, u1 FlagZ, u1 FlagA, u1 FlagP, u1 FlagCY,
                     const char* ipat, u8 b2, u8 b1) {
  static char instruction[256];
  sprintf(instruction,ipat,b2,b1);
  dump_state(instruction,
             PCH,PCL,A,B,C,D,E,H,L,SPH,SPL,
             FlagS,FlagZ,FlagA,FlagP,FlagCY
             );
}

#ifdef TRACE
#define instruction0(x...) f_instruction0(x)
#define instruction1(x...) f_instruction1(x)
#define instruction2(x...) f_instruction2(x)
#else
#define instruction0(x...) {}
#define instruction1(x...) {}
#define instruction2(x...) {}
#endif

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

static Control op_CF (); // defined in generated code
static Control op_D7 (); // defined in generated code


u8 PCH,PCL; // program counter; high/low byte



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

extern Func ops_array [];
extern Func output_instruction_array [];
extern Func input_instruction_array [];
extern Func slow_progs_array [];
extern Func fast_progs_array [];


#ifdef MODE_A
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

#endif


#ifdef MODE_B
char mode[] = "B";

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
  if (credit <= 0) {
    return jumpInterrupt(pc,slow);
  }
  return (Control)slow;
}

Control jump16(u16 pc) {
  if (pc>=ROM_SIZE) {
    printf ("jump16/B: (a>=ROM_SIZE) : a=%04x, ROM_SIZE=%04x\n",pc,ROM_SIZE);
    die;
  }
  Func fn = slow_progs_array[pc];
  if (fn == 0) {
    printf ("jump16/B : no program for target address: %04x\n",pc);
    die;
  }
  return jumpDirect(pc,fn,fn);
}

#endif


#ifdef MODE_C
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

#endif


// rest of the registers visible to the generated code

u8 A,B,C,D,E,H,L,SPH,SPL;
u1 FlagS,FlagZ,FlagA,FlagP,FlagCY;
u8 Shifter_HI,Shifter_LO,Shifter_OFF;
