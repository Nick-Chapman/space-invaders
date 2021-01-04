
#include <stdlib.h>
#include <stdbool.h>

#define die { printf("die: %s:%d (%s)\n", __FILE__, __LINE__, __FUNCTION__); exit(1); }

typedef void* (*Control)(void);
typedef Control (*Func)();

typedef bool u1;
// TODO: use uint8_t etc
typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u17;

#define MEM_SIZE 0x4000
#define RAM_BASE 0x2000

u1 FlagS;
u1 FlagZ;
u1 FlagA;
u1 FlagP;
u1 FlagCY;

u8 SPL;
u8 SPH;
u8 A;
u8 B;
u8 C;
u8 D;
u8 E;
u8 H;
u8 L;

u8 Shifter_HI;
u8 Shifter_LO;
u8 Shifter_OFF;

u8 mem [MEM_SIZE];

void todo(const char*);
void at(const char*);
void instruction(const char*, u16);
void advance(int);

Control jumpDirect(Func);
Control jump16(u16);

void mem_write(u16,u8);

void sound_control(const char*,u1);
void enable_interrupts(void);

u1 e1_true(void);
u1 e1_false(void);
u1 e1_flip(u1);
u1 e1_is_zero(u8);
u1 e1_test_bit(u8,int);
u1 e1_or_bit(u1,u1);
u1 e1_and_bit(u1,u1);
u1 e1_hi_bit_of_17(u17);
u1 e1_parity(u8);
u1 e1_is_pressed(const char*);

u8 e8_hi(u16);
u8 e8_lo(u16);
u8 e8_update_bit(u8,int,u1);
u8 e8_complement(u8);
u8 e8_and(u8,u8);
u8 e8_or(u8,u8);
u8 e8_xor(u8,u8);
u8 e8_shiftR(u8,u8);
u8 e8_shiftL(u8,u8);
u8 e8_ite(u1,u8,u8);
u8 e8_read_mem(u16);

u16 e16_hi_lo(u8,u8);
u16 e16_offset_addr(int,u16);
u16 e16_add_with_carry(u1,u8,u8);
u16 e16_drop_hi_bit_of_17(u17);

u17 e17_add(u16,u16);
