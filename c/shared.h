#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

#define die { printf("die: %s:%d (%s)\n", __FILE__, __LINE__, __FUNCTION__); exit(1); }

typedef void* (*Control)(void);
typedef Control (*Func)();

extern Control slow_0000 ();
extern Control fast_0000 ();
extern long cycles;

typedef bool u1;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u17;
typedef uint64_t u64;

#define MEM_SIZE 0x4000

extern u8 mem [MEM_SIZE];

typedef enum ButtonE
  { CoinEntry,
    Tilt,
    P1start,
    P1left,
    P1right,
    P1shoot,
    P2start,
    P2left,
    P2right,
    P2shoot,
    Dip3_livesLow,
    Dip5_livesHigh,
    Dip6_extraShipEarly,
    Dip7_coinInfoOff,
  } Button;

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

u1 e1_is_pressed(Button);

#define instruction(x...) f_instruction(x)
//#define instruction(x...) {}
void f_instruction(const char*, u16);


#define HALF_FRAME_CYCLES (2000000 / 120)

extern int credit;

extern Control jump16(u16);
extern Control jumpInterrupt(u16 pc, Func f);

static inline Control jumpDirect(u16,Func);

#define ROM_SIZE 0x2000

extern Func slow_progs_array [];
extern Func fast_progs_array [];
extern Func ops_array [];
extern Func output_instruction_array [];
extern Func input_instruction_array [];

extern Control op_CF ();
extern Control op_D7 ();

Control jumpDirect(u16 pc, Func f) {
  if (credit <= 0) {
    return jumpInterrupt(pc,f);
  }
  return (Control)f;
}
