
// machine.h -- this file gets included by main.c
//              and by machine.c (which itself is included by the generated program.c)

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

#define die { printf("die: %s:%d (%s)\n", __FILE__, __LINE__, __FUNCTION__); exit(1); }

typedef bool u1;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u17;
typedef uint64_t u64;

typedef void* (*Control)(void);
typedef Control (*Func)();

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

u1 FlagS,FlagZ,FlagA,FlagP,FlagCY;
u8 PCH,PCL;
u8 A,B,C,D,E,H,L,SPH,SPL;
u8 Shifter_HI,Shifter_LO,Shifter_OFF;

long cycles;
bool use_per_address_programs;
bool use_fast_programs;

extern u8 mem [];

Control jump16 (u16); // provided in machine.c ; used in generated code & main.c

u1 e1_is_pressed(Button); // provided in main.c ; used by generated code
