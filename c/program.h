
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "shared.h"

#define noinline __attribute__ ((noinline))

inline static void advance(int);

noinline static void mem_write(u16,u8);
inline static void sound_control(const char*,u1);
inline static void enable_interrupts(void);
inline static void disable_interrupts(void);
inline static void unknown_output(int,u8);

noinline static u1 e1_parity(u8);

inline static u8 e8_update_bit(u8,int,u1);
inline static u8 e8_read_mem(u16);

noinline static u8 e8_unknown_input(int);

extern Control op_rst1();
extern Control op_rst2();

long cycles = 0;

int credit = HALF_FRAME_CYCLES;

static bool half = false;
static int interrupts = 0;

static bool interrupts_enabled = false;

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
    return (Control)(half ? op_CF : op_D7);
  }
  return (Control)f;
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

void disable_interrupts(void) {
  //printf ("disable_interrupts\n");
  interrupts_enabled = false;
}

void unknown_output(int p,u8 b) {
  //printf ("unknown_output: %d %02x\n",p,b);
}

u1 e1_parity(u8 e) {
  return !(((e>>0)&0x1) ^ ((e>>1)&0x1) ^ ((e>>2)&0x1) ^ ((e>>3)&0x1)
           ^ ((e>>4)&0x1) ^ ((e>>5)&0x1) ^ ((e>>6)&0x1) ^ ((e>>7)&0x1));
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

u8 e8_unknown_input(int p) {
  //printf ("unknown_input: %d\n",p);
  die
}
